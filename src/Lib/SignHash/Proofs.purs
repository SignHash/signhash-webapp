module Lib.SignHash.Proofs where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Lib.SignHash.Contracts (SignerContract, getProof)
import Lib.SignHash.Proofs.Parsing (validateProof)
import Lib.SignHash.Proofs.Types (ProofVerification(..), VerificationError(..))
import Lib.SignHash.Types (Address, ProofMethod(GitHub))
import Lib.Web3 (WEB3)
import Network.HTTP.Affjax (AJAX, get)


validateGithubProof ::
  forall eff
  . Address
  -> String
  -> Aff (ajax :: AJAX | eff) (Either VerificationError Unit)
validateGithubProof address username = do
  raw <- get $ proofURL
  pure $ validateProof address raw.response
  where
    proofURL =
      "https://raw.githubusercontent.com/"
      <> username
      <> "/signhash-proof/master/proof.txt"

fetchProof ::
  forall eff
  . SignerContract
  -> Address
  -> ProofMethod
  -> Aff (ajax :: AJAX, random :: RANDOM, web3 :: WEB3 | eff) (Either Error ProofVerification)
fetchProof contract address GitHub = do
  proofValue <- getProof contract address GitHub
  case proofValue of
    Nothing -> pure $ Right Unavailable
    Just value -> toProof value <$> validateGithubProof address value
  where
    toProof value = Right <<< either (Unverified value) (const $ Verified value)
fetchProof contract address method = do
  choice <- liftEff $ randomInt 1 3

  let
    toProof r = case choice of
      1 -> Verified r.response
      2 -> Unverified r.response (UnconfirmedAddress address [])
      _ -> Unavailable

  result <- attempt $ get "http://setgetgo.com/randomword/get.php"
  pure $ toProof <$> result
