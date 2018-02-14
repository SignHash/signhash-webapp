module App.State.IdentityManagement where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe)
import Lib.Eth.Web3 (TxHash, TxStatus, WEB3)
import Lib.SignHash.Proofs.Methods (ProofMethod)
import Lib.SignHash.Proofs.Values (ProofValue)
import Pux (EffModel, noEffects)


data Event
  = Edit ProofMethod String
  | RequestUpdate ProofMethod UpdateValue
  | UpdateTxHash ProofMethod UpdateValue TxHash
  | UpdateTxStatus ProofMethod UpdateValue TxHash TxStatus
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
  noEffects $ Map.insert method (Updating updateValue txHash) state
foldp (UpdateTxStatus method updateValue txHash status) state =
  noEffects $ state

getMethodUIState :: ProofMethod -> State -> Maybe ProofManagementState
getMethodUIState = Map.lookup


init :: State
init = Map.empty
