module App.Hash.Proofs where

import Prelude

import App.Hash.Types (Address, ProofMethod, ProofVerification(..))
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax (AJAX)


fetchProof ::
  forall eff.
  Address ->
  ProofMethod ->
  Aff (ajax :: AJAX | eff) ProofVerification
fetchProof address method = do
  pure $ Verified "Foo"
