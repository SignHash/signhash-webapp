module App.Events.Creators where

import Prelude

import App.Events.Types (Event(..))
import Control.Monad.Except (runExcept)
import Data.Array (fromFoldable, head)
import Data.Either (Either(..))
import Data.Foreign (renderForeignError)
import Data.Maybe (maybe)
import Lib.Files (getFilesFromEvent)
import Pux.DOM.Events (DOMEvent)


newFilesEvent :: DOMEvent -> Event
newFilesEvent domEvent =
  PreventDefault next domEvent
  where
    readFiles = runExcept $ getFilesFromEvent domEvent
    next =
      case readFiles of
        Left errors ->
          FileError $ fromFoldable $ renderForeignError <$> errors
        Right files ->
          maybe NoFile (\f -> NewFile f) $ head files
