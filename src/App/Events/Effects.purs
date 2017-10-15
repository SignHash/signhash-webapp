module App.Events.Effects where

import Prelude

import App.Hash.Types (HashSigner(..))
import App.Hash.Worker (WORKER, calcHash, hashWorker)
import Control.Comonad (extract)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import DOM (DOM)
import Data.DateTime (diff)
import Data.Either (Either(..))
import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta)
import Network.HTTP.Affjax (AJAX, get)


type HashCalculationResult =
  { hash :: String
  , elapsed :: Seconds }


processNewFile ::
  forall eff.
  FileMeta ->
  Aff (
    now :: NOW,
    dom :: DOM,
    worker :: WORKER
  | eff) HashCalculationResult
processNewFile file = do
  started <- liftEff $ nowDateTime
  worker <- liftEff $ hashWorker
  hash <- calcHash worker file.ref

  finished <- liftEff $ nowDateTime

  let elapsed = diff (extract finished) (extract started)

  pure $ { hash, elapsed }


fetchSigners ::
  forall eff. String -> Aff ( ajax :: AJAX | eff) HashSigner
fetchSigners hash = do
  result <- attempt $ get "http://setgetgo.com/randomword/get.php"
  pure $ case result of
    Left error -> NoSigner
    Right value -> HashSigner value.response
