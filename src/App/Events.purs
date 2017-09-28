module App.Events where

import Prelude

import App.State (State)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Array (fromFoldable, head)
import Data.Either (Either(..))
import Data.Foreign (ForeignError, renderForeignError)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Lib.Files (FileData, getFilesFromEvent, readFileContent)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent)

data Event = NewFile FileData | FileError String | NoFile


foldp :: Event -> State -> EffModel State Event (console :: CONSOLE, dom :: DOM)
foldp NoFile state =
  noEffects $ state { filename = Nothing }
foldp (NewFile file) state =
  {state: state { filename = Just file.name },
   effects: [readFileContent file.ref *> pure Nothing]}
foldp (FileError err) state =
  { state, effects: [ log err *> pure Nothing ]}


handleNewFile :: DOMEvent -> Event
handleNewFile evt = case readFiles of
  Left errors -> FileError $ renderForeignErrors errors
  Right files -> maybe NoFile NewFile $ head files
  where
    readFiles = runExcept $ getFilesFromEvent evt


renderForeignErrors :: NonEmptyList ForeignError -> String
renderForeignErrors errors =
  joinWith "\n" $ fromFoldable $ renderForeignError <$> errors
