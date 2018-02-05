module Lib.Eth.Contracts where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Foreign.Index ((!))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import FFI.Util (property)
import FFI.Util.Function (call1)
import Lib.Eth.Web3 (Address, NetworkID(..), WEB3, Web3)


newtype EthContractData t = EthContractData t

class EthContract c

newtype Result e = Result e


data ContractLoadingError
  = NotDeployedToNetwork NetworkID

derive instance genericContractLoadingError :: Generic ContractLoadingError _

instance showContractLoadingError :: Show ContractLoadingError where
  show = genericShow


type ContractLoadingResult c = Either ContractLoadingError c


getAddress :: forall c. EthContract c => c -> Address
getAddress = prop "address"


getResult :: forall a. Result a -> a
getResult = prop "0"


getDeployed ::
  forall eff t
  . EthContractData t
  -> Web3
  -> NetworkID
  -> Aff (web3 :: WEB3 | eff) (ContractLoadingResult t)
getDeployed contractData web3 networkId@(NetworkID id) = do
  let address =
        runExcept $
        (toForeign contractData) ! "networks" ! id ! "address"

  pure case address of
    Left err -> Left $ NotDeployedToNetwork networkId
    Right value -> do
      let contract = call1 web3 "contract" (contractData `property` "abi")
      Right $ call1 contract "at" value


prop :: forall a b. String -> a -> b
prop = flip property
