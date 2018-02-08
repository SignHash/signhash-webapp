module App.State.IdentityManagement where

import Prelude

import Data.Map as Map
import Lib.Eth.Web3 (TxHash, WEB3)
import Lib.SignHash.Proofs.Methods (ProofMethod)
import Pux (EffModel, noEffects)


data Event
  = Edit ProofMethod
  | Delete ProofMethod
  | Cancel ProofMethod
  | PendingUpdate ProofMethod TxHash


type State = Map.Map ProofMethod ProofManagementState


data ProofManagementState = Editing | Updating


type Effects eff = ( web3 :: WEB3 | eff )


-- TODO: Make it a part of Contracts state? It can't work without it.
-- OR not, it's just an UI state. Connect it only via events? Or pass an event handler in init?

foldp ::
  forall eff.
  Event ->
  State ->
  EffModel State Event (Effects eff)
foldp event state = noEffects state
