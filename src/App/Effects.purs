module App.Effects where

import Prelude

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
import Data.Time.Duration (Seconds)
import Lib.Files (readFileByChunks)


processNewFile ::
  forall eff.
  File ->
  Aff (console :: CONSOLE, now :: NOW, dom :: DOM | eff) Unit
processNewFile file = do
  log $ "Size: " <> (show $ floor $ size file) <> " Bytes"

  started <- liftEff $ nowDateTime

  readFileByChunks file 512 onChunk

  finished <- liftEff $ nowDateTime

  let dt :: Seconds
      dt = diff (extract finished) (extract started)
  log $ show dt

  where
    onChunk i chunk = do
      log $ "Chunk " <> show i <> "\n" <> chunk
