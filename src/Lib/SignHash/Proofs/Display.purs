module Lib.SignHash.Proofs.Display where


import Prelude

import Data.Array (find)
import Data.Maybe (isJust)
import Lib.SignHash.Proofs.Types (ProofState(..), ProofVerification(..))


data SignerDisplayStatus
  = SignerLoading
  | SignerVerified
  | SignerVerificationFailed
  | SignerNetworkError


derive instance eqSignerDisplayStatus :: Eq SignerDisplayStatus


signerDisplayStatus :: Array ProofState -> SignerDisplayStatus
signerDisplayStatus proofs =
  if pending proofs then
    SignerLoading
  else
    if failed proofs then
      SignerVerificationFailed
    else
      if networkError proofs then
        SignerNetworkError
      else
        SignerVerified
  where
    pending = exists (eq Pending)
    failed = exists case _ of
      (Finished (Unverified _)) -> true
      _ -> false
    networkError = exists case _ of
      (NetworkError) -> true
      _ -> false
    exists pred = isJust <<< find pred
