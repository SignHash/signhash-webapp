module Lib.SignHash.Files where

import Prelude

import Lib.SignHash.Worker (WORKER, calcHash, hashWorker)
import Control.Comonad (extract)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import DOM (DOM)
import Data.DateTime (diff)
import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta)


type HashCalculationResult =
  { hash :: String
  , elapsed :: Seconds }


calculateFileHash ::
  forall eff.
  FileMeta ->
  Aff (
    now :: NOW,
    dom :: DOM,
    worker :: WORKER
  | eff) HashCalculationResult
calculateFileHash file = do
  started <- liftEff $ nowDateTime
  worker <- liftEff $ hashWorker
  hash <- calcHash worker file.ref

  finished <- liftEff $ nowDateTime

  let elapsed = diff (extract finished) (extract started)

  pure $ { hash, elapsed }
