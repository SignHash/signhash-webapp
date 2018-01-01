module Lib.Eth.Web3 where

import Prelude

import Control.Monad.Aff (Aff, Error)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Except (runExcept)
import Control.Promise (toAffE)
import DOM (DOM)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foreign (Foreign, readNullOrUndefined, unsafeFromForeign)
import Data.Maybe (Maybe(..))
import FFI.Util (property, propertyPath)


foreign import data Web3 :: Type
foreign import data WEB3 :: Effect

type Web3Config = String
type Web3Aff eff res = Aff (web3 :: WEB3 | eff) res

newtype Bytes = Bytes String

newtype TxHash = TxHash String

instance showTxHash :: Show TxHash where
  show (TxHash hash) = hash

type TxResult = Either Error TxHash

type TxAff eff = Aff (web3 :: WEB3 | eff) TxResult

newtype Address = Address String

derive instance eqAddress :: Eq Address
instance showAddress :: Show Address where
  show (Address a) = a


derive instance ordAddress :: Ord Address


foreign import bytesFromASCII :: String -> Bytes
foreign import buildWeb3 :: Web3Config -> Web3
foreign import _getInjectedWeb3 :: forall eff. Eff (dom :: DOM | eff) Foreign


getInjectedWeb3 :: forall eff. Eff (dom :: DOM | eff) (Maybe Web3)
getInjectedWeb3 = do
  injected <- _getInjectedWeb3
  let read = runExcept $ readNullOrUndefined injected
  pure case read of
    Left err -> Nothing
    Right Nothing -> Nothing
    Right (Just web3) -> Just $ unsafeFromForeign web3


getOrBuildWeb3 :: forall eff. Web3Config -> Eff (dom :: DOM | eff) Web3
getOrBuildWeb3 config = do
  injectedWeb3 <- getInjectedWeb3
  pure case injectedWeb3 of
    Just web3 -> web3
    Nothing -> buildWeb3 config


getAccounts :: forall eff. Web3 -> Web3Aff eff (Array Address)
getAccounts web3 = toAffE (web3 `property` "accounts")


getDefaultAccount :: forall eff. Web3 -> Web3Aff eff (Maybe Address)
getDefaultAccount web3 = head <$> getAccounts web3


isMetaMask :: Web3 -> Boolean
isMetaMask web3 =
  (web3 `propertyPath` ["currentProvider", "isMetaMask"]) == true
