module App.State.IdentityManagement where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Lib.Eth.Web3 (TxHash, WEB3)
import Lib.SignHash.Proofs.Methods (ProofMethod)
import Pux (EffModel, noEffects, onlyEffects)


data Event
  = Edit ProofMethod String
  | Delete ProofMethod
  | Cancel ProofMethod
  | PendingUpdate ProofMethod TxHash


type State = Maybe (Tuple ProofMethod ProofManagementState)


data ProofManagementState
  = Editing String
  | Updating


type Effects eff = ( web3 :: WEB3 | eff )


foldp ::
  forall eff.
  Event ->
  State ->
  EffModel State Event (Effects eff)
foldp (Edit method value) state =
  noEffects $ Just $ Tuple method (Editing value)
foldp (Cancel method) state =
  noEffects $ Nothing
foldp event state = noEffects state


getMethodUIState :: ProofMethod -> State -> Maybe ProofManagementState
getMethodUIState methodAsk (Just (Tuple method state))
  | methodAsk == method = Just state
  | otherwise = Nothing
getMethodUIState methodAsk _ = Nothing


init :: State
init = Nothing
