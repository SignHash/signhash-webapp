module Lib.SignHash.Contracts.SignHash where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Promise (Promise, toAffE)
import Data.Array (head)
import Data.Either (Either)
import Data.Maybe (maybe)
import FFI.Util.Function (callEff2)
import Lib.Eth.Contracts (class EthContract, EthContractData, Result, getDeployed, getResult, requireContractData)
import Lib.Eth.Web3 (Address(..), Bytes(..), WEB3, Web3)
import Lib.SignHash.Types (Checksum, HashSigner(..))


foreign import data Contract :: Type
instance _signerContract :: EthContract Contract


contractData :: EthContractData Contract
contractData = requireContractData "SignHash"


loadContract ::
  forall eff. Web3 -> Aff (web3 :: WEB3 | eff) (Either Error Contract)
loadContract = getDeployed contractData


checksumToBytes :: String -> Bytes
checksumToBytes = Bytes <<< append "0x"


foreign import _sign ::
  forall eff.
  Contract ->
  Bytes ->
  Address ->
  Eff (web3 :: WEB3 | eff) (Promise Unit)


sign ::
  forall eff.
  Contract ->
  Checksum ->
  Address ->
  Aff (web3 :: WEB3 | eff) (Either Error Unit)
sign contract checksum signer =
  attempt $ toAffE $ _sign contract (checksumToBytes checksum) signer


rawGetSigners ::
  forall eff.
  Contract ->
  Bytes ->
  Int ->
  Aff (web3 :: WEB3 | eff) (Result (Array String))
rawGetSigners contract bytes size =
  toAffE $ callEff2 contract "list" bytes size


getSigners ::
  forall eff.
  Contract ->
  Checksum ->
  Int ->
  Aff (web3 :: WEB3 | eff) (Array String)
getSigners contract checksum size =
  getResult <$> rawGetSigners contract (checksumToBytes checksum) size


getSigner ::
  forall eff. Contract -> Checksum -> Aff (web3 :: WEB3 | eff) HashSigner
getSigner contract checksum = do
  signers <- getSigners contract checksum 1
  pure $ maybe NoSigner (HashSigner <<< Address) $ head signers
