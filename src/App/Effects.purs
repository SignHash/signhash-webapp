module App.Effects where

import Prelude

import App.Events.Types (Event(..))
import Control.Comonad (extract)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import DOM (DOM)
import DOM.File.File (size)
import DOM.File.Types (File)
import Data.DateTime (diff)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds)
import Lib.Files (readFileByChunks)
import Lib.Hash as Hash


processNewFile ::
  forall eff.
  File ->
  Aff (
    console :: CONSOLE,
    now :: NOW,
    dom :: DOM,
    sjcl :: Hash.SJCL
               | eff) (Maybe Event)
processNewFile file = do
  log $ "Size: " <> (show $ floor $ size file) <> " Bytes"

  sha <- liftEff $ Hash.sha256

  started <- liftEff $ nowDateTime

  readFileByChunks file 512 (onChunk sha)

  hash <- liftEff $ Hash.finalize sha

  finished <- liftEff $ nowDateTime

  let dt :: Seconds
      dt = diff (extract finished) (extract started)
  log $ show dt

  pure $ Just $ FileLoaded hash

  where
    onChunk sha i chunk = do
      liftEff $ Hash.update sha chunk
