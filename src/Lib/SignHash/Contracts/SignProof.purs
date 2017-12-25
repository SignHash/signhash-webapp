module Lib.SignHash.Contracts.SignProof where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Promise (Promise, toAffE)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import FFI.Util.Function (callEff2)
import Lib.Eth.Contracts (class Contract, ContractData, getDeployed, getResult, requireContractData)
import Lib.Eth.Web3 (Address, Bytes(..), WEB3, Web3)
import Lib.SignHash.Proofs.Methods (ProofMethod, canonicalName)


foreign import data SignProof :: Type
instance _signerContract :: Contract SignProof


contractData :: ContractData SignProof
contractData = requireContractData "SignProof"


loadContract ::
  forall eff. Web3 -> Aff (web3 :: WEB3 | eff) (Either Error SignProof)
loadContract = getDeployed contractData


foreign import _addProof ::
  forall eff
  . SignProof
  -> Bytes
  -> Bytes
  -> Address
  -> Eff (web3 :: WEB3 | eff) (Promise Unit)


add ::
  forall eff
  . SignProof
  -> String
  -> String
  -> Address
  -> Aff (web3 :: WEB3 | eff) Unit
add contract key value from = do
  toAffE $ _addProof contract (Bytes key) (Bytes value) from


get ::
  forall eff
  . SignProof
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
