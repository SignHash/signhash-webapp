module App.Effects where

import Prelude

import App.Events.Types (Event(..))
import App.Hash.Worker (WORKER, calcHash, hashWorker)
import App.Types (Signer(..))
import Control.Comonad (extract)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import DOM (DOM)
import Data.DateTime (diff)
import Data.Either (either)
import Data.Maybe (Maybe(Just))
import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta)
import Network.HTTP.Affjax (AJAX, get)


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

  pure $ Just $ HashCalculated { hash, elapsed }


fetchSigners :: forall eff. String -> Aff ( ajax :: AJAX | eff) (Maybe Event)
fetchSigners hash = do
  result <- attempt $ get "http://setgetgo.com/randomword/get.php"
  let
    signer = either (const NoSigner) (\v -> Signer v.response) result

  pure $ Just $ SignerFetched signer
