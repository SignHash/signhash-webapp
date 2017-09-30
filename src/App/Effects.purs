module App.Effects where

import Prelude

import App.Events.Types (Event(..))
import App.Hash.Worker (WORKER, calcHash, hashWorker)
import Control.Comonad (extract)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import DOM (DOM)
import Data.DateTime (diff)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta)


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
