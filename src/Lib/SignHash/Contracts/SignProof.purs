module Lib.SignHash.Contracts.SignProof where

import Prelude hiding (add)

import Control.Monad.Aff (Aff, attempt)
import Control.Promise (toAffE)
import Data.Maybe (Maybe(..))
import FFI.Util.Function (callEff2, callEff3)
import Lib.Eth.Contracts (class EthContract, EthContractData, ContractLoadingResult, getDeployed, getResult)
import Lib.Eth.Web3 (Address, NetworkID, TxAff, WEB3, Web3)
import Lib.SignHash.Proofs.Methods (ProofMethod, canonicalName)
import Lib.SignHash.Proofs.Values as ProofValue


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
  case updateValue of
    Just value -> add contract methodName value from
    Nothing -> remove contract methodName from
  where
    methodName = canonicalName method


add ::
  forall eff
  . Contract
  -> String
  -> ProofValue.ProofValue
  -> Address
  -> TxAff eff
add contract key value from =
  attempt $ toAffE $ callEff3 contract "add" key value { from, gas: 70000 }


remove ::
  forall eff
  . Contract
  -> String
  -> Address
  -> TxAff eff
remove contract key from =
  attempt $ toAffE $ callEff2 contract "remove" key { from, gas: 40000 }


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
