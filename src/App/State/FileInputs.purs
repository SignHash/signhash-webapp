module App.State.FileInputs where

import Prelude

import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Array (fromFoldable, head)
import Data.Either (Either(..))
import Data.Foreign (renderForeignError)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Lib.Files (FileMeta, getFilesFromEvent)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent)


data Event =
  NoFile |
  NewFile FileMeta |
  FileError (Array String)


type State = Unit


type Effects eff =
  ( dom :: DOM
  , console :: CONSOLE
    | eff
    )


foldp ::
  forall eff.
  Event ->
  State ->
  EffModel State Event (Effects eff)
foldp (FileError err) state =
  onlyEffects state $ [ (traverse log err) *> pure Nothing ]
foldp _ state = noEffects $ state


newFilesEvent :: DOMEvent -> Event
newFilesEvent domEvent =
  case readFiles of
    Left errors ->
      FileError $ fromFoldable $ renderForeignError <$> errors
    Right files ->
      maybe NoFile (\f -> NewFile f) $ head files
  where
    readFiles = runExcept $ getFilesFromEvent domEvent
