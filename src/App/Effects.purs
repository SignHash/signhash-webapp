module App.Effects where

import Prelude

import App.Events.Types (Event(..))
import App.Hash.Worker (WORKER, calcHash, hashWorker)
import Control.Comonad (extract)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import Data.Array (fromFoldable, head)
import Data.DateTime (diff)
import Data.Either (Either(..))
import Data.Foreign (ForeignError, renderForeignError)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta, getFilesFromEvent)
import Pux.DOM.Events (DOMEvent)


processNewFile ::
  forall eff.
  FileMeta ->
  Aff (
    now :: NOW,
    dom :: DOM,
    worker :: WORKER
  | eff) (Maybe Event)
processNewFile file = do
  started <- liftEff $ nowDateTime
  worker <- liftEff $ hashWorker
  hash <- calcHash worker file.ref

  finished <- liftEff $ nowDateTime

  let elapsed :: Seconds
      elapsed = diff (extract finished) (extract started)

  pure $ Just $ FileLoaded { hash, elapsed }


processDOMFiles :: forall eff. DOMEvent -> Aff ( dom :: DOM | eff) (Maybe Event)
processDOMFiles domEvent = do
  liftEff $ preventDefault domEvent
  let
    readFiles = runExcept $ getFilesFromEvent domEvent
    next = case readFiles of
        Left errors -> FileError $ renderForeignErrors errors
        Right files -> maybe NoFile (\f -> NewFile f) $ head files

  pure $ Just next


renderForeignErrors :: NonEmptyList ForeignError -> String
renderForeignErrors errors =
  joinWith "\n" $ fromFoldable $ renderForeignError <$> errors


preventDefaultEffect ::
  forall eff.
  DOMEvent ->
  Aff (dom :: DOM | eff) (Maybe Event)
preventDefaultEffect domEvent = do
  liftEff $ preventDefault domEvent
  pure Nothing
