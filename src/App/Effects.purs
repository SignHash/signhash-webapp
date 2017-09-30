module App.Effects where

import Prelude

import App.Events.Types (Event(..))
import Control.Comonad (extract)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import DOM (DOM)
import Data.DateTime (diff)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta, readFileByChunks)
import Lib.Hash as Hash


processNewFile ::
  forall eff.
  FileMeta ->
  Aff (
    now :: NOW,
    dom :: DOM,
    sjcl :: Hash.SJCL
               | eff) (Maybe Event)
processNewFile file = do
  sha <- liftEff $ Hash.sha256

  started <- liftEff $ nowDateTime

  readFileByChunks file 512 (onChunk sha)

  hash <- liftEff $ Hash.finalize sha

  finished <- liftEff $ nowDateTime

  let elapsed :: Seconds
      elapsed = diff (extract finished) (extract started)

  pure $ Just $ FileLoaded { hash, elapsed }

  where
    onChunk sha i chunk = do
      liftEff $ Hash.update sha chunk
