module App.State where

import Prelude

import Lib.SignHash.Types (HashSigner(HashSigner))
import Lib.SignHash.Worker (WORKER)
import App.State.FileInputs as FileInputs
import App.State.Files as Files
import App.State.Signers as Signers
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Lib.Pux (mergeEffModels)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, mapEffects, mapState, noEffects)


data Event =
  FileInput FileInputs.Event |
  File Files.Event |
  Signer Signers.Event


type State =
  { file :: Maybe Files.State
  , signer :: Maybe Signers.State
  }


init :: State
init =
  { file: Nothing
  , signer: Nothing
  }


type AppEffects =
  ( console :: CONSOLE
  , dom :: DOM
  , now :: NOW
  , worker :: WORKER
  , ajax :: AJAX
  , random :: RANDOM
  )


foldp ::
  Event ->
  State ->
  EffModel State Event AppEffects

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

foldp (File event) baseState =
  mergeEffModels fileEff (fileFoldp event) baseState
  where
    fileEff state = case state.file of
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


fileFoldp ::
  Files.Event ->
  State ->
  EffModel State Event AppEffects
fileFoldp (Files.SignerFetched (HashSigner address)) state =
  { state: state { signer = (Just $ Signers.init address) }
  , effects: [pure $ Just $ Signer $ Signers.Init]
  }

fileFoldp _ state = noEffects $ state
