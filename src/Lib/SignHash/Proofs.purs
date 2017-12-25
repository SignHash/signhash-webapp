module Lib.SignHash.Proofs where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Lib.SignHash.Contracts.SignHash (SignerContract, getProof)
import Lib.SignHash.Proofs.Parsing (validateProofAddress)
import Lib.SignHash.Proofs.Types (ProofVerification(..), ProofVerificationFailed(..))
import Lib.SignHash.Proofs.Values as ProofValue
import Lib.SignHash.Proofs.Methods (ProofMethod, fetchProof)
import Lib.SignHash.Types (Address)
import Lib.Eth.Web3 (WEB3)
import Network.HTTP.Affjax (AJAX)


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
    Just entry ->
      case ProofValue.createProofValue entry of
        Left err -> pure $ Right $ Unverified $ InvalidProofValue entry err
        Right proofValue -> validateProofMethod proofValue address method


validateProofMethod ::
  forall eff
  . ProofValue.ProofValue
  -> Address
  -> ProofMethod
  -> Aff (ajax :: AJAX, web3 :: WEB3 | eff) (Either Error ProofVerification)
validateProofMethod proofValue address method = do
  proofContent <- attempt $ fetchProof method proofValue
  pure $
    toVerifcationResult
    <$> (validateProofAddress address)
    <$> proofContent
  where
    toVerifcationResult (Right _) = Verified proofValue
    toVerifcationResult (Left verificationError) =
      Unverified $ InvalidProofContent proofValue verificationError
