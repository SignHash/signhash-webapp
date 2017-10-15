module App.Events.Signers where

import Prelude

import App.Hash.Proofs (fetchProof)
import App.Hash.Types (Address, ProofMethod, ProofVerification, allProofMethods)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Random (RANDOM)
import Data.Either (either)
import Data.Lens (Traversal', (%~))
import Data.Lens.Record (prop)
import Data.Map (Map, empty, insert)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)


data Event =
  Init Address |
  FetchProof Address ProofMethod |
  ProofFetched Address ProofMethod ProofVerification |
  ProofFetchingError Address ProofMethod Error


type State =
  { address :: Address
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
  , ajax :: AJAX
  , random :: RANDOM
    | eff
  )


init :: Address -> State
init address = { address, proofs: empty }


foldp ::
  forall eff.
  Event -> State -> EffModel State Event (SignerEffects eff)
foldp (Init address) state =
  onlyEffects state $
  pure <$> Just <$> (FetchProof address) <$> allProofMethods
foldp (FetchProof address method) state =
  { state: signerProofs %~ insertProof $ state
  , effects: [fetchProofEffect]
  }
  where
    insertProof = insert method Pending
    fetchProofEffect = do
      proof <- fetchProof address method
      pure $ Just $
        either
        (ProofFetchingError address method)
        (ProofFetched address method)
        proof

foldp (ProofFetched address method proof) state =
  noEffects $ signerProofs %~ updateProof $ state
  where
    updateProof = insert method $ Finished proof

foldp (ProofFetchingError address method error) state =
  { state: signerProofs %~ setError $ state,
    effects: [ (log $ show error) *> pure Nothing ] }
  where
    setError = insert method NetworkError
