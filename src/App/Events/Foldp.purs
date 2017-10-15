module App.Events.Foldp where

import Prelude

import App.Events.FileInputs as FileInputs
import App.Events.Files as Files
import App.Events.Signers as Signers
import App.Events.Types (Event(..))
import App.Hash.Types (HashSigner(HashSigner))
import App.Hash.Worker (WORKER)
import App.State (State, signerProp)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Lib.Pux (mergeEffModels)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, mapEffects, mapState, noEffects)


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
  { state: initSigner $ state
  , effects: [pure $ Just $ Signer $ Signers.Init]
  }
  where
    initSigner = signerProp .~ (Just $ Signers.init address)

fileFoldp _ state = noEffects $ state
