module Tests.Data.Setup where

import Prelude

import Control.Monad.Aff (Aff, error, runAff, throwError)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Data.Array (filter, index, mapWithIndex)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromJust)
import Data.StrMap (StrMap, lookup)
import Data.String (Pattern(..), contains, split)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..))
import Lib.SignHash.Contracts.SignHash as SignHash
import Lib.SignHash.Contracts.SignProof as SignProof
import Lib.SignHash.Types (Address(..), Checksum)
import Lib.Eth.Web3 (WEB3, buildWeb3)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS, readTextFile, readdir)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, readJSON)
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


type Proofs = Array { key :: String, value :: String }

type AccountProofsMap = StrMap Proofs


easyReadJSON :: forall a eff. ReadForeign a => String -> Aff (eff) a
easyReadJSON raw = case readJSON raw of
  Left err -> throwError $ error $ show $ err
  Right value -> pure value


loadFile ::
  forall eff.
  String ->
  Aff (fs :: FS, console :: CONSOLE | eff) FileFixture
loadFile path = do
  log $ "Loading " <> path <> "..."
  content <- readTextFile UTF8 path
  rawMeta <- readTextFile UTF8 (path <> ".json")
  meta <- easyReadJSON rawMeta
  pure { content, meta, path }


setupProofs ::
  forall eff
  . SignProof.Contract
  -> Array Address
  -> Aff (fs :: FS, console:: CONSOLE, web3 :: WEB3 | eff) Unit
setupProofs contract accounts = do
  raw <- readTextFile UTF8 Generator.accountsProofsPath
  proofsMap :: AccountProofsMap <- easyReadJSON raw
  void $ traverse (addAccountProofs proofsMap) (mapWithIndex Tuple accounts)
  where
    addAccountProofs proofsMap (Tuple i acc) =
      let index = show i
      in case lookup index proofsMap of
        Nothing -> pure unit
        Just proofs -> void $ traverse (addAccountProof index acc) proofs

    addAccountProof index acc { key, value } = do
      log $ "Adding account " <> index <> " " <> show acc
        <> " proof " <> key <> ":" <> value
      SignProof.add contract key value acc

main ::
  Eff
  (console :: CONSOLE
  , web3 :: WEB3
  , fs :: FS
  , exception :: EXCEPTION
  ) Unit
main = void $ runAff logShow do
  let web3 = buildWeb3 "http://localhost:8545"
  signHash <- liftError =<< SignHash.loadContract web3
  signProof <- liftError =<< SignProof.loadContract web3
  accounts <- readAccounts
  filePaths <- buildPaths <$> readdir Generator.rootFilesPath
  files <- traverse loadFile filePaths

  signFiles signHash accounts files
  setupProofs signProof accounts

  where
    buildPaths = (map $ append Generator.rootFilesPath)
                 <<< filter (not contains (Pattern ".json"))

    readAccounts = do
      content <- readTextFile UTF8 Generator.accountsPath
      pure $ Address <$> split (Pattern "\n") content

    signFiles contract accounts files = do
      void $ traverse (signFile contract accounts) files

    signFile contract accounts file = do
      for_ file.meta.signers \acc -> do
        let signerAcc = unsafePartial $ fromJust $ index accounts acc
        SignHash.sign contract file.meta.checksum signerAcc
