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
import Data.Maybe (Maybe(..), maybe)
import FFI.Util (property)
import FFI.Util.Function (call1, callEff2)
import Lib.SignHash.Types (Checksum, HashSigner(..), ProofMethod, canonicalName)
import Lib.Web3 (Address(..), Bytes(..), WEB3, Web3)


newtype ContractData t = ContractData t

class Contract c

newtype Result e = Result e


type ProofValue = String


getAddress :: forall c. Contract c => c -> Address
getAddress = prop "address"


prop :: forall a b. String -> a -> b
prop = flip property


getResult :: forall a. Result a -> a
getResult = prop "0"


checksumToBytes :: String -> Bytes
checksumToBytes = Bytes <<< append "0x"


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
  Bytes ->
  Address ->
  Eff (web3 :: WEB3 | eff) (Promise Unit)


sign ::
  forall eff.
  SignerContract ->
  Checksum ->
  Address ->
  Aff (web3 :: WEB3 | eff) (Either Error Unit)
sign contract checksum signer =
  attempt $ toAffE $ _sign contract (checksumToBytes checksum) signer


rawGetSigners ::
  forall eff.
  SignerContract ->
  Bytes ->
  Int ->
  Aff (web3 :: WEB3 | eff) (Result (Array String))
rawGetSigners contract bytes size =
  toAffE $ callEff2 contract "getSigners" bytes size


getSigners ::
  forall eff.
  SignerContract ->
  Checksum ->
  Int ->
  Aff (web3 :: WEB3 | eff) (Array String)
getSigners contract checksum size =
  getResult <$> rawGetSigners contract (checksumToBytes checksum) size


getSigner ::
  forall eff. SignerContract -> Checksum -> Aff (web3 :: WEB3 | eff) HashSigner
getSigner contract checksum = do
  signers <- getSigners contract checksum 1
  pure $ maybe NoSigner (HashSigner <<< Address) $ head signers


foreign import _addProof ::
  forall eff
  . SignerContract
  -> Bytes
  -> Bytes
  -> Address
  -> Eff (web3 :: WEB3 | eff) (Promise Unit)


addProof ::
  forall eff
  . SignerContract
  -> String
  -> String
  -> Address
  -> Aff (web3 :: WEB3 | eff) Unit
addProof contract key value from = do
  toAffE $ _addProof contract (Bytes key) (Bytes value) from


getProof ::
  forall eff
  . SignerContract
  -> Address
  -> ProofMethod
  -> Aff (web3 :: WEB3 | eff) (Maybe ProofValue)
getProof contract address method = do
  proofValue <- getResult <$> rawGetProof
  pure case proofValue of
    "" -> Nothing
    value -> Just value
  where
    rawGetProof =
      toAffE $ callEff2 contract "getProof" address (canonicalName method)
