module Lib.SignHash.Contracts where

import Prelude

import Control.Monad.Aff (Aff, attempt, error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Except (runExcept)
import Control.Promise (Promise, toAffE)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Foreign.Index ((!))
import Data.Maybe (maybe)
import Debug.Trace (spy, traceA, traceAnyM)
import FFI.Util (property)
import FFI.Util.Function (call1, callEff2)
import Lib.SignHash.Types (Checksum, HashSigner(..))
import Lib.Web3 (WEB3, Web3, bytesFromASCII)


type Address = String

newtype ContractData t = ContractData t

class Contract c


address :: forall c. Contract c => c -> Address
address = prop "address"

prop :: forall a b. String -> a -> b
prop = flip property


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


foreign import data SignerContract :: Type
instance _signerContract :: Contract SignerContract

foreign import signerContractData :: ContractData SignerContract

signerContract ::
  forall eff. Web3 -> Aff (web3 :: WEB3 | eff) (Either Error SignerContract)
signerContract = getDeployed signerContractData


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
  attempt $ toAffE $ _sign contract ("0x" <> checksum) signer


getSigners ::
  forall eff.
  SignerContract ->
  Checksum ->
  Int ->
  Aff (web3 :: WEB3 | eff) (Array String)
getSigners contract checksum size = do
  value <- toAffE $ callEff2 contract "getSigners" ("0x" <> checksum) size
  pure $ getResult value

getSigner ::
  forall eff. SignerContract -> Checksum -> Aff (web3 :: WEB3 | eff) HashSigner
getSigner contract checksum = do
  signers <- getSigners contract checksum 1
  pure $ maybe NoSigner HashSigner $ head signers


getResult :: forall a b. a -> b
getResult = prop "0"
