module App.State where

import Prelude

import App.Env (AppEnvConfig)
import App.Routing (Location(..))
import App.State.Contracts (ETHAccountChannel)
import App.State.Contracts as Contracts
import App.State.FileInputs as FileInputs
import App.State.Files as Files
import App.State.Locations as Locations
import App.State.Signers as Signers
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import Data.Lens ((^.), (.~))
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Lib.Eth.Web3 (Address, WEB3)
import Lib.Pux (mergeEffModels)
import Lib.SignHash.Contracts.SignHash (getSigner)
import Lib.SignHash.Types (HashSigner(..))
import Lib.SignHash.Worker (WORKER)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, mapEffects, mapState, noEffects, onlyEffects)


data Event =
  Init InitEnv |
  Routing Locations.Event |
  Contract Contracts.Event |
  FileInput FileInputs.Event |
  File Files.Event |
  Signer Address Signers.Event |
  FileSignerFetched HashSigner


type State =
  { file :: Maybe Files.State
  , myAccount :: Contracts.ETHAccountState Address
  , signers :: Map.Map Address Signers.State
  , contracts :: Contracts.State
  , defaults ::
       { network :: String }
  , location :: Locations.State
  }


type InitEnv =
  { ethAccountChannel :: ETHAccountChannel }


type FoldpResult = EffModel State Event AppEffects


init :: AppEnvConfig -> State
init { rpcUrl } =
  { file: Nothing
  , myAccount: Contracts.Unavailable
  , signers: Map.empty
  , contracts: Contracts.Loading
  , defaults: { network: rpcUrl }
  , location: Verify
  }


type AppEffects =
  ( console :: CONSOLE
  , dom :: DOM
  , history :: HISTORY
  , timer :: TIMER
  , now :: NOW
  , worker :: WORKER
  , ajax :: AJAX
  , random :: RANDOM
  , web3 :: WEB3
  )


foldp :: Event -> State -> FoldpResult

foldp (Init env) state =
  onlyEffects state
  [ pure $ Just $ Contract
    $ Contracts.Load state.defaults.network env.ethAccountChannel
  ]

foldp (Contract (Contracts.OnAccountChanged account)) state =
  case account of
    Contracts.Available address ->
      mergeEffModels updateMyAccount (loadSignerEffModel address) state
    otherwise -> updateMyAccount state
  where
    updateMyAccount s = noEffects $ s { myAccount = account }

foldp (Contract event) state =
  Contracts.foldp event state.contracts
  # mapEffects Contract
  # mapState \s -> state { contracts  = s}

foldp (FileInput (FileInputs.NewFile file)) state =
  { state: state { file = Just $ Files.init file }
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

foldp (Signer address event) state =
  case Map.lookup address state.signers of
    Nothing -> noEffects $ state
    Just signerState ->
      Signers.foldp event signerState
      # mapEffects (Signer address)
      # mapState \s -> state { signers = Map.insert address s state.signers }

foldp (FileSignerFetched signer) state =
  case signer of
    NoSigner -> fileModel state
    HashSigner address ->
      mergeEffModels fileModel (loadSignerEffModel address) state
  where
    fileModel s =
      onlyEffects s $ [ pure $ Just $ File $ Files.SignerFetched signer ]

foldp (Routing event) state =
  Locations.foldp event (state ^. lens)
  # mapEffects Routing
  # mapState \s -> (lens .~ s $ state)
  where
    lens = prop (SProxy :: SProxy "location")


loadSignerEffModel :: Address -> State -> FoldpResult
loadSignerEffModel address state =
  case Map.lookup address state.signers of
    Just value -> noEffects state
    Nothing ->
      whenContractsLoaded state \c ->
      let
        initSigner = Map.insert address (Signers.init address)
      in
       { state: state { signers = initSigner state.signers }
       , effects:
         [ pure $ Just $ Signer address $ Signers.FetchAll $ c.signProof ]
       }


whenContractsLoaded ::
  State
  -> (Contracts.LoadedState -> FoldpResult)
  -> FoldpResult
whenContractsLoaded { contracts: Contracts.Loaded c } fun = fun c
whenContractsLoaded state _ = noEffects state
