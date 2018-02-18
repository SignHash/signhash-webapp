module App.State.IdentityManagement where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Lib.Eth.Web3 (TxHash, TxStatus(..), WEB3)
import Lib.SignHash.Proofs.Methods (ProofMethod)
import Lib.SignHash.Proofs.Values (ProofValue, createProofValue)
import Pux (EffModel, noEffects, onlyEffects)
import Signal as SG
import Signal.Channel as CH
import Signal.Time (debounce)


data Event
  = Edit ProofMethod String
  | Remove ProofMethod
  | UpdateTxHash ProofMethod UpdateValue TxHash
  | UpdateTxStatus ProofMethod UpdateValue TxHash TxStatus
  | Cancel ProofMethod
  | Request Request
  | NoOp


data Request
  = Update ProofMethod UpdateValue
  | FetchProof ProofMethod ProofValue


type State = Maybe ProofMethodChange
type UpdateValue = Maybe ProofValue
type ProofMethodChange = Tuple ProofMethod ProofManagementState


data ProofManagementState
  = Editing String
  | Updating UpdateValue TxHash
  | UpdateFailed UpdateValue TxHash


type Effects eff = ( web3 :: WEB3 | eff )


type Env = { identityEvents :: CH.Channel Event }


foldp ::
  forall eff
  . Env
  -> Event
  -> State
  -> EffModel State Event (Effects eff)
foldp env event state = case event of
  (Edit method value) ->
    { state: setMethodState method (Editing value) state
    , effects: [ liftEff $ CH.send env.identityEvents event *> pure Nothing ]}
  (Remove method) ->
    onlyEffects state [ pure $ Just $ Request $ Update method Nothing ]
  (Cancel method) ->
    noEffects $ Nothing
  (Request _) -> noEffects state
  (UpdateTxHash method updateValue txHash) ->
    noEffects $ setMethodState method (Updating updateValue txHash) state
  (UpdateTxStatus method updateValue txHash TxOk) ->
    noEffects $ Nothing
  (UpdateTxStatus method updateValue txHash TxFailed) ->
    noEffects $ setMethodState method (UpdateFailed updateValue txHash) state
  (UpdateTxStatus method updateValue txHash TxPending) ->
    noEffects $ state
  NoOp -> noEffects state


setMethodState :: ProofMethod -> ProofManagementState -> State -> State
setMethodState method proofManagement state =
  Just $ Tuple method proofManagement


init :: State
init = Nothing


buildEnv :: forall eff. Eff (channel :: CH.CHANNEL | eff) Env
buildEnv = do
  identityEvents <- CH.channel NoOp
  pure { identityEvents }


getInputs :: Env -> Array (SG.Signal Event)
getInputs env =
  pure $
  env.identityEvents
  # CH.subscribe
  SG.~> SG.filterMap buildEvent NoOp
  SG.~> (debounce 500.0)

  where
    buildEvent (Edit method value) =
      case hush $ createProofValue value of
        Just validValue -> Just $ Request $ FetchProof method validValue
        Nothing -> Nothing
    buildEvent _ = Nothing
