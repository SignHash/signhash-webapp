module App.State.IdentityManagement where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Lib.Eth.Web3 (TxHash(..), WEB3, TxResult)
import Lib.SignHash.Proofs.Methods (ProofMethod)
import Lib.SignHash.Proofs.Values (ProofValue)
import Pux (EffModel, noEffects, onlyEffects)


data Event
  = Edit ProofMethod String
  | RequestUpdate ProofMethod UpdateValue
  | UpdateTxResult ProofMethod UpdateValue TxResult
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
foldp (UpdateTxResult method updateValue txResult) state =
  noEffects $ case txResult of
    Right txHash -> Map.insert method (Updating updateValue txHash) state
    Left error -> state

getMethodUIState :: ProofMethod -> State -> Maybe ProofManagementState
getMethodUIState = Map.lookup


init :: State
init = Map.empty
