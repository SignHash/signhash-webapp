module App.State where

import Prelude

import App.Env (Env)
import App.Routing (Location(..))
import App.State.Contracts as Contracts
import App.State.FileInputs as FileInputs
import App.State.Files as Files
import App.State.Locations as Locations
import App.State.Signers as Signers
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import Data.Lens ((^.), (.~))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Lib.Eth.Web3 (WEB3)
import Lib.Pux (mergeEffModels)
import Lib.SignHash.Contracts.SignHash (getSigner)
import Lib.SignHash.Types (HashSigner(..))
import Lib.SignHash.Worker (WORKER)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, mapEffects, mapState, noEffects, onlyEffects)


data Event =
  Init |
  Routing Locations.Event |
  Contract Contracts.Event |
  FileInput FileInputs.Event |
  File Files.Event |
  Signer Signers.Event |
  FileSignerFetched HashSigner


type State =
  { file :: Maybe Files.State
  , signer :: Maybe Signers.State
  , contracts :: Contracts.State
  , defaults ::
       { network :: String }
  , location :: Locations.State
  }


type FoldpResult = EffModel State Event AppEffects


init :: Env -> State
init { rpcUrl } =
  { file: Nothing
  , signer: Nothing
  , contracts: Contracts.Loading
  , defaults: { network: rpcUrl }
  , location: Verify
  }


type AppEffects =
  ( console :: CONSOLE
  , dom :: DOM
  , history :: HISTORY
  , now :: NOW
  , worker :: WORKER
  , ajax :: AJAX
  , random :: RANDOM
  , web3 :: WEB3
  )


foldp :: Event -> State -> FoldpResult

foldp Init state =
  onlyEffects state [
    pure $ Just $ Contract $ Contracts.Load state.defaults.network
  ]

foldp (Contract event) state =
  Contracts.foldp event state.contracts
  # mapEffects Contract
  # mapState \s -> state { contracts  = s}

foldp (FileInput (FileInputs.NewFile file)) state =
  { state: state { file = Just $ Files.init file, signer = Nothing }
  , effects: [ pure $ Just $ File $ Files.CalculateHash ]
  }

foldp (FileInput FileInputs.NoFile) state =
  noEffects $ state { file = Nothing }

foldp (FileInput event) state =
  FileInputs.foldp event unit
  # mapEffects FileInput
  # mapState (const state)

foldp (File (Files.Signal (Files.OnHashCalculated result))) state =
  whenContractsLoaded state \c -> onlyEffects state $ [
    do
      signer <- getSigner c.signHash result.hash
      pure $ Just $ FileSignerFetched signer
    ]

foldp (File event) state =
  case state.file of
    Nothing -> noEffects $ state
    Just fileState ->
      Files.foldp event fileState
      # mapEffects File
      # mapState \s -> state { file = Just s }

foldp (Signer event) state =
  case state.signer of
    Nothing -> noEffects $ state
    Just signerState ->
      Signers.foldp event signerState
      # mapEffects Signer
      # mapState \s -> state { signer = Just s }

foldp (FileSignerFetched signer) rootState =
  mergeEffModels fileModel signerModel rootState
  where
    fileModel state =
      onlyEffects state $
      [ pure $ Just $ File $ Files.SignerFetched signer ]
    signerModel state = case signer of
      NoSigner -> noEffects state
      HashSigner address ->
        whenContractsLoaded state \c ->
        { state: state { signer = (Just $ Signers.init address) }
        , effects:
          [ pure $ Just $ Signer $ Signers.FetchAll $ c.signProof ]
        }

foldp (Routing event) state =
  Locations.foldp event (state ^. lens)
  # mapEffects Routing
  # mapState \s -> (lens .~ s $ state)
  where
    lens = prop (SProxy :: SProxy "location")


whenContractsLoaded ::
  State
  -> (Contracts.LoadedState -> FoldpResult)
  -> FoldpResult
whenContractsLoaded { contracts: Contracts.Loaded c } fun = fun c
whenContractsLoaded state _ = noEffects state
