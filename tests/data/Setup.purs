module Tests.Data.Setup where

import Prelude

import Control.Monad.Aff (Aff, runAff, throwError)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Data.Array (head)
import Data.Either (Either, either)
import Data.Maybe (fromJust)
import Lib.SignHash.Contracts (sign, signerContract)
import Lib.Web3 (WEB3, buildWeb3, getAccounts)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)


liftError :: forall res eff. Either Error res -> Aff (eff) res
liftError = either throwError pure

main ::
  Eff
  (console :: CONSOLE
  , web3 :: WEB3
  , fs :: FS
  , exception :: EXCEPTION
  ) Unit
main = do
  let web3 = buildWeb3 "http://localhost:8545"

  content <- readTextFile UTF8 "./tests/data/signed.txt"
  checksum <- readTextFile UTF8 "./tests/data/signed.sha256"

  void $ runAff logShow do
    accounts <- getAccounts web3
    let signerAcc = unsafePartial $ fromJust $ head accounts
    contract <- liftError =<< signerContract web3
    sign contract checksum signerAcc
