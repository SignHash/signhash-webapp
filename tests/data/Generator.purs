module Tests.Data.Generator where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Crypto.Simple as Crypto
import Data.Array (mapWithIndex)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Lib.Web3 (WEB3, buildWeb3, getAccounts)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS, writeTextFile)
import Simple.JSON (writeJSON)


accountsPath :: String
accountsPath = "./tests/data/accounts.txt"


rootFilesPath :: String
rootFilesPath = "./tests/data/files/"


accountsProofsPath :: String
accountsProofsPath = "./tests/data/proofs.json"


main ::
  Eff
  (console :: CONSOLE
  , web3 :: WEB3
  , fs :: FS
  , exception :: EXCEPTION
  ) Unit
main = void $ runAff logShow do
  accounts <- getAccounts web3
  saveAccounts accounts
  traverse generateSignedFile (enumerate accounts)

  where
    web3 = buildWeb3 "http://localhost:8545"
    enumerate = mapWithIndex Tuple

    saveAccounts accounts =
      writeTextFile UTF8 accountsPath (("\n" `joinWith` accounts) <> "\n")

    generateSignedFile (Tuple i account) = do
      let
        filePath = (rootFilesPath <> "acc" <> show i <> ".txt")
        fileContent = "acc" <> show i <> "\n"
        fileMeta =
          { checksum: Crypto.hash Crypto.SHA256 fileContent # Crypto.toString
          , signers: [i] }

      writeTextFile UTF8 filePath fileContent
      writeTextFile UTF8 (filePath <> ".json") (writeJSON fileMeta)
