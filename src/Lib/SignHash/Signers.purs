module Lib.SignHash.Signers where

import Prelude

import Lib.SignHash.Types (HashSigner(..))
import Control.Monad.Aff (Aff, attempt)
import Data.Either (Either(..))
import Network.HTTP.Affjax (AJAX, get)


fetchHashSigners ::
  forall eff. String -> Aff ( ajax :: AJAX | eff) HashSigner
fetchHashSigners hash = do
  result <- attempt $ get "http://setgetgo.com/randomword/get.php"
  pure $ case result of
    Left error -> NoSigner
    Right value -> HashSigner value.response
