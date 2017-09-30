module App.Events.Foldp where

import Prelude

import App.Effects (processNewFile)
import App.Events.Types (Event(..))
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
import Lib.Hash (SJCL)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent)


foldp ::
  Event ->
  State ->
  EffModel State Event (
    console :: CONSOLE, dom :: DOM, now :: NOW, sjcl :: SJCL
  )
foldp NoFile state =
  noEffects $ state { file = Nothing }
foldp (NewFile file) state =
  {state: state {
      file = Just file,
      completed = false
   },
   effects: [processNewFile file]}
foldp (FileError err) state =
  { state, effects: [ log err *> pure Nothing ]}
foldp (FileLoaded hash) state =
  noEffects $ state {
    completed = true,
    hash = Just hash
  }


handleNewFile :: DOMEvent -> Event
handleNewFile evt = case readFiles of
  Left errors -> FileError $ renderForeignErrors errors
  Right files -> maybe NoFile NewFile $ head files
  where
    readFiles = runExcept $ getFilesFromEvent evt


renderForeignErrors :: NonEmptyList ForeignError -> String
renderForeignErrors errors =
  joinWith "\n" $ fromFoldable $ renderForeignError <$> errors
