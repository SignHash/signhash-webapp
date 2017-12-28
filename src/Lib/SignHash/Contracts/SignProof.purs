module Lib.SignHash.Contracts.SignProof where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Promise (Promise, toAffE)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import FFI.Util.Function (callEff2, callEff3)
import Lib.Eth.Contracts (class EthContract, EthContractData, getDeployed, getResult, requireContractData)
import Lib.Eth.Web3 (Address, Bytes(..), WEB3, Web3)
import Lib.SignHash.Proofs.Methods (ProofMethod, canonicalName)


foreign import data Contract :: Type
instance _signerContract :: EthContract Contract


contractData :: EthContractData Contract
contractData = requireContractData "SignProof"


loadContract ::
  forall eff. Web3 -> Aff (web3 :: WEB3 | eff) (Either Error Contract)
loadContract = getDeployed contractData


add ::
  forall eff
  . Contract
  -> String
  -> String
  -> Address
  -> Aff (web3 :: WEB3 | eff) Unit
add contract key value from = do
  toAffE $ callEff3 contract "add" key value { from }


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
