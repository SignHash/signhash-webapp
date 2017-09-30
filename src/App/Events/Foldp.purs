module App.Events.Foldp where

import Prelude

import App.Effects (processNewFile)
import App.Events.Types (Event(..))
import App.Hash.Worker (WORKER)
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
import Lib.Files (getFilesFromEvent)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent)


foldp ::
  Event ->
  State ->
  EffModel State Event (
    console :: CONSOLE, dom :: DOM, now :: NOW, worker :: WORKER
  )
foldp NoFile state =
  noEffects $ state { file = Nothing }
foldp (NewFile file) state =
  {state: state {
      file = Just {
         meta: file,
         result: Nothing
      }
   }, effects: [processNewFile file]}
foldp (FileError err) state =
  { state, effects: [ log err *> pure Nothing ]}
foldp (FileLoaded event) state =
  noEffects $ state {
    file = updateHash state.file
  }
  where
    updateHash =
      maybe Nothing (\file -> Just $ file { result = Just event })



handleNewFile :: DOMEvent -> Event
handleNewFile evt = case readFiles of
  Left errors -> FileError $ renderForeignErrors errors
  Right files -> maybe NoFile NewFile $ head files
  where
    readFiles = runExcept $ getFilesFromEvent evt


renderForeignErrors :: NonEmptyList ForeignError -> String
renderForeignErrors errors =
  joinWith "\n" $ fromFoldable $ renderForeignError <$> errors
