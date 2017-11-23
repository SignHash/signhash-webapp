module Lib.SignHash.Proofs where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Either (Either(..))
import Lib.SignHash.Types (Address, ProofMethod(..), ProofVerification(..))
import Network.HTTP.Affjax (AJAX, get)


fetchProof ::
  forall eff.
  Address ->
  ProofMethod ->
  Aff (ajax :: AJAX, random :: RANDOM | eff) (Either Error ProofVerification)
fetchProof address GitHub = pure $ Right $ Unavailable
fetchProof address method = do
  choice <- liftEff $ randomInt 1 3

  let
    toProof r = case choice of
      1 -> Verified r.response
      2 -> Unverified r.response
      _ -> Unavailable

  result <- attempt $ get "http://setgetgo.com/randomword/get.php"
  pure $ toProof <$> result
