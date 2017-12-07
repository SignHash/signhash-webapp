module App.State where

import Prelude

import App.Env (Env)
import App.State.Contracts as Contracts
import App.State.FileInputs as FileInputs
import App.State.Files as Files
import App.State.Signers as Signers
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Lib.Pux (mergeEffModels)
import Lib.SignHash.Contracts (getSigner)
import Lib.SignHash.Proofs (fetchProof)
import Lib.SignHash.Types (HashSigner(..))
import Lib.SignHash.Worker (WORKER)
import Lib.Web3 (WEB3)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, mapEffects, mapState, noEffects, onlyEffects)


data Event =
  Init |
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
  }


type FoldpResult = EffModel State Event AppEffects


init :: Env -> State
init { rpcUrl } =
  { file: Nothing
  , signer: Nothing
  , contracts: Contracts.Loading
  , defaults: { network: rpcUrl }
  }


type AppEffects =
  ( console :: CONSOLE
  , dom :: DOM
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
      signer <- getSigner c.signerContract result.hash
      pure $ Just $ FileSignerFetched signer
    ]

foldp (File event) state =
  case state.file of
    Nothing -> noEffects $ state
    Just fileState ->
      Files.foldp event fileState
      # mapEffects File
      # mapState \s -> state { file = Just s }

foldp (Signer event) baseState =
  mergeEffModels signerEff (signerFoldp event) baseState
  where
    signerEff state = case state.signer of
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
        { state: state { signer = (Just $ Signers.init address) }
        , effects: [ pure $ Just $ Signer $ Signers.Init ]
        }


signerFoldp :: Signers.Event -> State -> FoldpResult
signerFoldp (Signers.FetchProof address method) state =
  whenContractsLoaded state \c -> onlyEffects state $ [
    do
      proof <- fetchProof c.signerContract address method
      pure $ Just $ Signer $ either
        (Signers.ProofFetchingError method)
        (Signers.ProofFetched method)
        proof
  ]
signerFoldp _ state = noEffects state


whenContractsLoaded ::
  State
  -> (Contracts.LoadedState -> FoldpResult)
  -> FoldpResult
whenContractsLoaded { contracts: Contracts.Loaded c } fun = fun c
whenContractsLoaded state _ = noEffects state
