module Lib.Web3 where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foreign (Foreign, readNullOrUndefined, unsafeFromForeign)
import Data.Maybe (Maybe(..))


foreign import data Web3 :: Type
foreign import data WEB3 :: Effect

type Web3Config = String

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