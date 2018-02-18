module App.State.Signers where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Random (RANDOM)
import Data.Either (either)
import Data.Lens (Traversal', (%~))
import Data.Lens.Record (prop)
import Data.Map (Map, delete, empty, insert)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Lib.Eth.Web3 (WEB3)
import Lib.SignHash.Blockies (standardAddressBlockie)
import Lib.SignHash.Contracts.SignProof as SignProof
import Lib.SignHash.Proofs (getSignerProof, validateProofMethod)
import Lib.SignHash.Proofs.Methods (ProofMethod, allProofMethods)
import Lib.SignHash.Proofs.Types (ProofState(..), ProofVerification(..))
import Lib.SignHash.Proofs.Values (ProofValue)
import Lib.SignHash.Types (Address)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)


data Event =
  FetchAll SignProof.Contract |
  FetchProof SignProof.Contract ProofMethod |
  ProofPending ProofMethod |
  ProofFetched ProofMethod ProofVerification |
  ProofFetchingError ProofMethod Error |
  UpdateProof ProofMethod (Maybe ProofValue)


type State =
  { address :: Address
  , blockie :: String
  , proofs :: SignerProofs }


type SignerProofs = Map ProofMethod ProofState


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
  onlyEffects state $ pure <$> Just <$> FetchProof contract <$> allProofMethods
foldp (FetchProof contract method) state =
  onlyEffects state $
  [ Just <$> fetchProofEffect contract state.address method
  , pure $ Just $ ProofPending method]
foldp (UpdateProof method (Just proofValue)) state =
  onlyEffects state $
  [ do
       validationResult <- validateProofMethod proofValue state.address method
       pure $ Just $ either
         (ProofFetchingError method)
         (ProofFetched method)
         validationResult
  ]
foldp (UpdateProof method Nothing) state =
  noEffects $ state { proofs = insert method value state.proofs }
  where
    value = Finished Unavailable
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


fetchProofEffect ::
  forall eff
  . SignProof.Contract
  -> Address
  -> ProofMethod
  -> Aff (web3 :: WEB3, ajax :: AJAX | eff) Event
fetchProofEffect contract address method = do
  proof <- getSignerProof contract address method
  pure $ either
    (ProofFetchingError method)
    (ProofFetched method)
    proof
