module Lib.SignHash.Contracts where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Promise (Promise, toAffE)
import Data.Array (head)
import Data.Either (Either)
import Data.Maybe (maybe)
import FFI.Util (property)
import FFI.Util.Function (callEff2)
import Lib.SignHash.Types (Checksum, HashSigner(..))
import Lib.Web3 (Web3, WEB3)


type Address = String

newtype ContractABI t = ContractABI t

class Contract c


address :: forall c. Contract c => c -> Address
address = prop "address"

prop :: forall a b. String -> a -> b
prop = flip property


getDeployed ::
  forall eff t. ContractABI t -> Aff (web3 :: WEB3 | eff) (Either Error t)
getDeployed = attempt <<< toAffE <<< prop "deployed"


foreign import data SignerContract :: Type
instance _signerContract :: Contract SignerContract

foreign import loadSignerContract :: Web3 -> ContractABI SignerContract

signerContract ::
  forall eff. Web3 -> Aff (web3 :: WEB3 | eff) (Either Error SignerContract)
signerContract = getDeployed <<< loadSignerContract


foreign import _sign ::
  forall eff.
  SignerContract ->
  Checksum ->
  Address ->
  Eff (web3 :: WEB3 | eff) (Promise Unit)

sign ::
  forall eff.
  SignerContract ->
  Checksum ->
  Address ->
  Aff (web3 :: WEB3 | eff) (Either Error Unit)
sign contract checksum signer =
  attempt $ toAffE $ _sign contract checksum signer


getSigners ::
  forall eff.
  SignerContract ->
  Checksum ->
  Int ->
  Aff (web3 :: WEB3 | eff) (Array String)
getSigners contract checksum size =
  toAffE $ callEff2 contract "getSigners" checksum size

getSigner ::
  forall eff. SignerContract -> Checksum -> Aff (web3 :: WEB3 | eff) HashSigner
getSigner contract checksum = do
  signers <- getSigners contract checksum 1
  pure $ maybe NoSigner HashSigner $ head signers
