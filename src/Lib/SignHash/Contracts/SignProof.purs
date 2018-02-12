module Lib.SignHash.Contracts.SignProof where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Promise (toAffE)
import Data.Maybe (Maybe(..), fromJust)
import FFI.Util.Function (callEff2, callEff3)
import Lib.Eth.Contracts (class EthContract, EthContractData, ContractLoadingResult, getDeployed, getResult)
import Lib.Eth.Web3 (Address, NetworkID, WEB3, Web3, TxAff)
import Lib.SignHash.Proofs.Methods (ProofMethod, canonicalName)
import Lib.SignHash.Proofs.Values as ProofValue
import Partial.Unsafe (unsafePartial)


foreign import data Contract :: Type
instance _signerContract :: EthContract Contract


foreign import contractData :: EthContractData Contract


loadContract ::
  forall eff
  . Web3
  -> NetworkID
  -> Aff (web3 :: WEB3 | eff) (ContractLoadingResult Contract)
loadContract = getDeployed contractData


update ::
  forall eff
  . Contract
  -> ProofMethod
  -> Maybe ProofValue.ProofValue
  -> Address
  -> TxAff eff
update contract method updateValue from =
  add contract methodName value from
  where
    methodName = canonicalName method
    value = ProofValue.extract $ unsafePartial $ fromJust updateValue


add ::
  forall eff
  . Contract
  -> String
  -> String
  -> Address
  -> TxAff eff
add contract key value from = do
  attempt $ toAffE $ callEff3 contract "add" key value { from }


get ::
  forall eff
  . Contract
  -> Address
  -> ProofMethod
  -> Aff (web3 :: WEB3 | eff) (Maybe String)
get contract address method = do
  proofValue <- getResult <$> rawGetProof
  pure case proofValue of
    "" -> Nothing
    value -> Just value
  where
    rawGetProof =
      toAffE $ callEff2 contract "get" address (canonicalName method)
