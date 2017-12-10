module Lib.SignHash.Proofs.Methods where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Lib.SignHash.Proofs.Values as ProofValue
import Network.HTTP.Affjax (AJAX, get)


data ProofMethod = HTTP | GitHub


derive instance eqProofMethod :: Eq ProofMethod
derive instance ordProofMethod :: Ord ProofMethod
derive instance genericProofMethod :: Generic ProofMethod _

instance showProofMethod :: Show ProofMethod where
  show = genericShow


allProofMethods :: Array ProofMethod
allProofMethods = [HTTP, GitHub]


canonicalName :: ProofMethod -> String
canonicalName HTTP = "http"
canonicalName GitHub = "github"


fetchProof ::
  forall eff
  . ProofMethod
  -> ProofValue.ProofValue
  -> Aff (ajax :: AJAX | eff) String
fetchProof GitHub proofValue = do
  result <- get $ proofURL
  pure result.response
  where
    username = ProofValue.extract proofValue
    proofURL =
      "https://raw.githubusercontent.com/"
      <> username
      <> "/signhash-proof/master/proof.txt"
fetchProof _ _ = throwError $ error $ "Method not supported"
