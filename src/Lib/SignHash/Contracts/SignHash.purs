module Lib.SignHash.Contracts.SignHash where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Promise (Promise, toAffE)
import Data.Array (head)
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import FFI.Util.Function (callEff2)
import Lib.Eth.Contracts (class Contract, ContractData, Result, getDeployed, getResult, requireContractData)
import Lib.Eth.Web3 (Address(..), Bytes(..), WEB3, Web3)
import Lib.SignHash.Proofs.Methods (ProofMethod, canonicalName)
import Lib.SignHash.Types (Checksum, HashSigner(..))


foreign import data SignerContract :: Type
instance _signerContract :: Contract SignerContract


signerContractData :: ContractData SignerContract
signerContractData = requireContractData "SignHash"


signerContract ::
  forall eff. Web3 -> Aff (web3 :: WEB3 | eff) (Either Error SignerContract)
signerContract = getDeployed signerContractData


checksumToBytes :: String -> Bytes
checksumToBytes = Bytes <<< append "0x"


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
  -> Aff (web3 :: WEB3 | eff) (Maybe String)
getProof contract address method = do
  proofValue <- getResult <$> rawGetProof
  pure case proofValue of
    "" -> Nothing
    value -> Just value
  where
    rawGetProof =
      toAffE $ callEff2 contract "getProof" address (canonicalName method)
