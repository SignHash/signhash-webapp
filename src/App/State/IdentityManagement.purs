module App.State.IdentityManagement where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Lib.Eth.Web3 (TxHash, TxStatus(..), WEB3)
import Lib.SignHash.Proofs.Methods (ProofMethod)
import Lib.SignHash.Proofs.Values (ProofValue)
import Pux (EffModel, noEffects)


data Event
  = Edit ProofMethod String
  | UpdateTxHash ProofMethod UpdateValue TxHash
  | UpdateTxStatus ProofMethod UpdateValue TxHash TxStatus
  | Cancel ProofMethod
  | Request Request


data Request
  = Update ProofMethod UpdateValue


type State = Maybe ProofMethodChange
type UpdateValue = Maybe ProofValue
type ProofMethodChange = Tuple ProofMethod ProofManagementState


data ProofManagementState
  = Editing String
  | Updating UpdateValue TxHash
  | UpdateFailed UpdateValue TxHash

type Effects eff = ( web3 :: WEB3 | eff )


foldp ::
  forall eff.
  Event ->
  State ->
  EffModel State Event (Effects eff)
foldp (Edit method value) state =
  noEffects $ setMethodState method (Editing value) state
foldp (Cancel method) state =
  noEffects $ Nothing
foldp (Request _) state = noEffects state
foldp (UpdateTxHash method updateValue txHash) state =
  noEffects $ setMethodState method (Updating updateValue txHash) state
foldp (UpdateTxStatus method updateValue txHash TxOk) state =
  noEffects $ Nothing
foldp (UpdateTxStatus method updateValue txHash TxFailed) state =
  noEffects $ setMethodState method (UpdateFailed updateValue txHash) state
foldp (UpdateTxStatus method updateValue txHash TxPending) state =
  noEffects $ state


setMethodState :: ProofMethod -> ProofManagementState -> State -> State
setMethodState method proofManagement state =
  Just $ Tuple method proofManagement


init :: State
init = Nothing
