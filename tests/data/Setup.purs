module Tests.Data.Setup where

import Prelude

import Control.Monad.Aff (Aff, error, runAff, throwError)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Data.Array (filter, index)
import Data.Either (Either(..), either)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), contains, split)
import Data.Traversable (for_, traverse)
import Lib.SignHash.Contracts (sign, signerContract)
import Lib.SignHash.Types (Checksum)
import Lib.Web3 (WEB3, buildWeb3)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS, readTextFile, readdir)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (readJSON)
import Tests.Data.Generator as Generator


liftError :: forall res eff. Either Error res -> Aff (eff) res
liftError = either throwError pure


type FileFixtureMeta =
  { checksum :: Checksum
  , signers :: Array Int }


type FileFixture =
  { path :: String
  , content :: String
  , meta :: FileFixtureMeta }


loadFile ::
  forall eff.
  String ->
  Aff (fs :: FS, console :: CONSOLE | eff) FileFixture
loadFile path = do
  log $ "Loading " <> path <> "..."
  content <- readTextFile UTF8 path
  rawMeta <- readTextFile UTF8 (path <> ".json")
  case readJSON rawMeta of
    Left err -> throwError $ error $ show $ err
    Right meta -> pure { content, meta, path }

main ::
  Eff
  (console :: CONSOLE
  , web3 :: WEB3
  , fs :: FS
  , exception :: EXCEPTION
  ) Unit
main = void $ runAff logShow do
  accounts <- readAccounts
  filePaths <- buildPaths <$> readdir Generator.rootFilesPath
  files <- traverse loadFile filePaths
  void $ signFiles accounts files
  where
    web3 = buildWeb3 "http://localhost:8545"

    buildPaths = (map $ append Generator.rootFilesPath)
                 <<< filter (not contains (Pattern ".json"))

    readAccounts = do
      content <- readTextFile UTF8 Generator.accountsPath
      pure $ split (Pattern "\n") content

    signFiles accounts files = do
      void $ traverse (signFile accounts) files

    signFile accounts file = do
      for_ file.meta.signers \acc -> do
        let signerAcc = unsafePartial $ fromJust $ index accounts acc
        contract <- liftError =<< signerContract web3
        sign contract file.meta.checksum signerAcc
