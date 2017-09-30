module App.Events where

import Prelude

import App.Effects (processNewFile)
import App.State (State)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Array (fromFoldable, head)
import Data.Either (Either(..))
import Data.Foreign (ForeignError, renderForeignError)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Lib.Files (FileData, getFilesFromEvent)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent)

data Event = NewFile FileData | FileError String | NoFile | FileLoaded


foldp :: Event -> State -> EffModel State Event (console :: CONSOLE, dom :: DOM, now :: NOW)
foldp NoFile state =
  noEffects $ state { filename = Nothing }
foldp (NewFile file) state =
  {state: state {
      filename = Just file.name,
      completed = false
   },
   effects: [do
                processNewFile file.ref
                pure $ Just FileLoaded
            ]}
foldp (FileError err) state =
  { state, effects: [ log err *> pure Nothing ]}
foldp FileLoaded state =
  { state: state { completed = true }, effects: [] }


handleNewFile :: DOMEvent -> Event
handleNewFile evt = case readFiles of
  Left errors -> FileError $ renderForeignErrors errors
  Right files -> maybe NoFile NewFile $ head files
  where
    readFiles = runExcept $ getFilesFromEvent evt


renderForeignErrors :: NonEmptyList ForeignError -> String
renderForeignErrors errors =
  joinWith "\n" $ fromFoldable $ renderForeignError <$> errors
