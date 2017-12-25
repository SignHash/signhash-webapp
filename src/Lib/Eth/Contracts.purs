module Lib.Eth.Contracts where

import Prelude

import Control.Monad.Aff (Aff, error)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Except (runExcept)
import Control.Promise (toAffE)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Foreign.Index ((!))
import FFI.Util (property)
import FFI.Util.Function (call1)
import Lib.Eth.Web3 (Address, WEB3, Web3)


newtype ContractData t = ContractData t

class Contract c

newtype Result e = Result e


foreign import requireContractData :: forall a. String -> ContractData a


getAddress :: forall c. Contract c => c -> Address
getAddress = prop "address"


getResult :: forall a. Result a -> a
getResult = prop "0"


getDeployed ::
  forall eff t.
  ContractData t ->
  Web3 ->
  Aff (web3 :: WEB3 | eff) (Either Error t)
getDeployed contractData web3 = do
  networkId <- toAffE (web3 `property` "net_version")

  let address =
        runExcept $
        (toForeign contractData) ! "networks" ! networkId ! "address"

  pure case address of
    Left err -> Left $ error $ "Contract is not deployed to network id:"
                <> networkId
    Right value -> do
      let contract = call1 web3 "contract" (contractData `property` "abi")
      Right $ call1 contract "at" value


prop :: forall a b. String -> a -> b
prop = flip property
