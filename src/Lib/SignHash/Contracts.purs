module Lib.SignHash.Contracts where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Promise (Promise, toAffE)
import Data.Either (Either)
import FFI.Util (property)
import Lib.Web3 (Web3, WEB3)


type Address = String

newtype ContractABI t = ContractABI t

class Contract c


address :: forall c. Contract c => c -> Address
address = prop "address"

prop :: forall a b. String -> a -> b
prop = flip property


foreign import _getDeployed ::
  forall eff t. ContractABI t -> Eff (web3 :: WEB3 | eff) (Promise t)

getDeployed ::
  forall eff t. ContractABI t -> Aff (web3 :: WEB3 | eff) (Either Error t)
getDeployed = attempt <<< toAffE <<< _getDeployed


foreign import data SignerContract :: Type
instance _signerContract :: Contract SignerContract

foreign import loadSignerContract :: Web3 -> ContractABI SignerContract

signerContract ::
  forall eff. Web3 -> Aff (web3 :: WEB3 | eff) (Either Error SignerContract)
signerContract = getDeployed <<< loadSignerContract
