module Lib.SignHash.Proofs where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Lib.SignHash.Contracts (SignerContract, getProof)
import Lib.SignHash.Proofs.Parsing (validateProofContent)
import Lib.SignHash.Proofs.Types (ProofVerification(..), ProofVerificationFailed(..), VerificationError)
import Lib.SignHash.Proofs.Values as ProofValue
import Lib.SignHash.Types (Address, ProofMethod(GitHub))
import Lib.Web3 (WEB3)
import Network.HTTP.Affjax (AJAX, get)


getSignerProof ::
  forall eff
  . SignerContract
  -> Address
  -> ProofMethod
  -> Aff (ajax :: AJAX, web3 :: WEB3 | eff) (Either Error ProofVerification)
getSignerProof contract address method = do
  value <- getProof contract address method
  case value of
    Nothing -> pure $ Right Unavailable
    Just value ->
      case ProofValue.createProofValue value of
        Left err -> pure $ Right $ Unverified $ InvalidProofValue value err
        Right proofValue -> validateProofContains proofValue address method


validateProofContains ::
  forall eff
  . ProofValue.ProofValue
  -> Address
  -> ProofMethod
  -> Aff (ajax :: AJAX, web3 :: WEB3 | eff) (Either Error ProofVerification)
validateProofContains proofValue address GitHub = do
  verification <- attempt $ verifyGithubProof address proofValue
  pure $ toProof <$> verification
  where
    toProof (Right _) = Verified proofValue
    toProof (Left verificationError) =
      Unverified $ InvalidProofContent proofValue verificationError
validateProofContains contract address method = do
  pure $ Right $ Unavailable


verifyGithubProof ::
  forall eff
  . Address
  -> ProofValue.ProofValue
  -> Aff (ajax :: AJAX | eff) (Either VerificationError Unit)
verifyGithubProof address proofValue = do
  raw <- get $ proofURL
  pure $ validateProofContent address raw.response
  where
    username = ProofValue.extract proofValue
    proofURL =
      "https://raw.githubusercontent.com/"
      <> username
      <> "/signhash-proof/master/proof.txt"
