module App.State.Signers where

import Prelude

import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Random (RANDOM)
import Data.Lens (Traversal', (%~))
import Data.Lens.Record (prop)
import Data.Map (Map, empty, insert)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Lib.SignHash.Proofs.Types (ProofVerification)
import Lib.SignHash.Types (Address, ProofMethod, allProofMethods)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)


data Event =
  Init |
  FetchProof Address ProofMethod |
  ProofFetched ProofMethod ProofVerification |
  ProofFetchingError ProofMethod Error


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
foldp Init state =
  onlyEffects state $
  pure <$> Just <$> FetchProof state.address <$> allProofMethods
foldp (FetchProof address method) state =
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
