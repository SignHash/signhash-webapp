module App.Events.Foldp where

import Prelude

import App.Effects (processNewFile)
import App.Events.Types (Event(..))
import App.Hash.Worker (WORKER)
import App.State (State)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import Data.Array (fromFoldable, head)
import Data.Either (Either(..))
import Data.Foreign (ForeignError, renderForeignError)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Lib.Files (getFilesFromEvent)
import Pux (EffModel, CoreEffects, noEffects)
import Pux.DOM.Events (DOMEvent)


foldp ::
  Event ->
  State ->
  EffModel State Event (
    console :: CONSOLE, dom :: DOM, now :: NOW, worker :: WORKER
  )
foldp NoFile state =
  noEffects $ state { file = Nothing }
foldp (NewFile event file) state =
  {state: state {
      file = Just {
         meta: file,
         result: Nothing
      }
   }, effects: [
      preventDefaultEffect event,
      processNewFile file
   ]}
foldp (FileError event err) state =
  { state, effects: [
       do
         log err
         liftEff $ preventDefault event
         pure Nothing
       ]}
foldp (FileLoaded event) state =
  noEffects $ state {
    file = updateHash state.file
  }
  where
    updateHash =
      maybe Nothing (\file -> Just $ file { result = Just event })
foldp (DragFiles event) state =
  { state, effects: [ preventDefaultEffect event ]}


preventDefaultEffect ::
  forall eff.
  DOMEvent ->
  Aff (CoreEffects (dom :: DOM | eff)) (Maybe Event)
preventDefaultEffect domEvent = do
  liftEff $ preventDefault domEvent
  pure Nothing


handleNewFiles :: DOMEvent -> Event
handleNewFiles evt = case readFiles of
  Left errors -> FileError evt $ renderForeignErrors errors
  Right files -> maybe NoFile (\f -> NewFile evt f) $ head files
  where
    readFiles = runExcept $ getFilesFromEvent evt


renderForeignErrors :: NonEmptyList ForeignError -> String
renderForeignErrors errors =
  joinWith "\n" $ fromFoldable $ renderForeignError <$> errors
