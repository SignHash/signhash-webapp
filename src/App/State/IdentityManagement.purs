module App.State.IdentityManagement where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Lib.Eth.Web3 (TxHash(..), TxResult, TxStatus, WEB3)
import Lib.SignHash.Proofs.Methods (ProofMethod)
import Lib.SignHash.Proofs.Values (ProofValue)
import Pux (EffModel, noEffects, onlyEffects)


data Event
  = Edit ProofMethod String
  | RequestUpdate ProofMethod UpdateValue
  | UpdateTxHash ProofMethod UpdateValue TxHash
  | RequestTxPool TxHash (TxStatus -> (Maybe Event))
  | UpdateTxStatus ProofMethod UpdateValue TxStatus
  | Cancel ProofMethod

type State = Map.Map ProofMethod ProofManagementState
type UpdateValue = Maybe ProofValue


data ProofManagementState
  = Editing String
  | Updating UpdateValue TxHash


type Effects eff = ( web3 :: WEB3 | eff )


foldp ::
  forall eff.
  Event ->
  State ->
  EffModel State Event (Effects eff)
foldp (Edit method value) state =
  noEffects $ Map.insert method (Editing value) state
foldp (Cancel method) state =
  noEffects $ Map.delete method state
foldp (RequestUpdate method value) state =
  noEffects state
foldp (UpdateTxHash method updateValue txHash) state =
  { state: Map.insert method (Updating updateValue txHash) state
  , effects: [ pure $ Just $ RequestTxPool txHash $ next]}
  where
    next :: TxStatus -> Maybe Event
    next = Just <<< UpdateTxStatus method updateValue
foldp (RequestTxPool txHash next) state = noEffects state
foldp (UpdateTxStatus method updateValue status) state =
  noEffects state

getMethodUIState :: ProofMethod -> State -> Maybe ProofManagementState
getMethodUIState = Map.lookup


init :: State
init = Map.empty
