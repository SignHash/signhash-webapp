module App.State.Signers where

import Prelude

import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Random (RANDOM)
import Data.Either (either)
import Data.Lens (Traversal', (%~))
import Data.Lens.Record (prop)
import Data.Map (Map, empty, insert)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Lib.SignHash.Blockies (standardAddressBlockie)
import Lib.SignHash.Contracts (SignerContract)
import Lib.SignHash.Proofs (getSignerProof)
import Lib.SignHash.Proofs.Types (ProofVerification)
import Lib.SignHash.Types (Address)
import Lib.SignHash.Proofs.Methods (ProofMethod, allProofMethods)
import Lib.Web3 (WEB3)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)


data Event =
  FetchAll SignerContract |
  ProofPending ProofMethod |
  ProofFetched ProofMethod ProofVerification |
  ProofFetchingError ProofMethod Error


type State =
  { address :: Address
  , blockie :: String
  , proofs :: SignerProofs }


type SignerProofs = Map ProofMethod ProofState


data ProofState =
  Pending |
  NetworkError |
  Finished ProofVerification


signerProofs :: Traversal' State SignerProofs
signerProofs = prop (SProxy :: SProxy "proofs")


type SignerEffects eff =
  ( console :: CONSOLE
  , web3 :: WEB3
  , ajax :: AJAX
  , random :: RANDOM
    | eff
  )


init :: Address -> State
init address =
  { address,
    blockie: standardAddressBlockie address,
    proofs: empty }


foldp ::
  forall eff.
  Event -> State -> EffModel State Event (SignerEffects eff)
foldp (FetchAll contract) state =
  onlyEffects state $
  (fetchProofEffect <$> allProofMethods) <>
  (pure <$> Just <$> ProofPending <$> allProofMethods)
  where
    fetchProofEffect method = do
      proof <- getSignerProof contract state.address method
      pure $ Just $ either
        (ProofFetchingError method)
        (ProofFetched method)
        proof
foldp (ProofPending method) state =
  noEffects $ signerProofs %~ insertProof $ state
  where
    insertProof = insert method Pending
foldp (ProofFetched method proof) state =
  noEffects $ signerProofs %~ updateProof $ state
  where
    updateProof = insert method $ Finished proof
foldp (ProofFetchingError method error) state =
  { state: signerProofs %~ setError $ state,
    effects: [ (log $ show error) *> pure Nothing ] }
  where
    setError = insert method NetworkError
