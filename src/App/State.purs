module App.State where

import Prelude

import App.Hash.Types (HashSigner)
import App.Events.Signers as Signers
import Data.Lens (Traversal', _Just)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta)


type State = {
  file :: Maybe FileState,
  signer :: Maybe Signers.State
}


type FileState = {
  meta :: FileMeta,
  result :: Maybe FileHashResult,
  signer :: Maybe HashSigner
}


type FileHashResult = {
  hash :: String,
  elapsed :: Seconds
}


fileProp :: Traversal' State (Maybe FileState)
fileProp = prop (SProxy :: SProxy "file")

signerProp :: Traversal' State (Maybe Signers.State)
signerProp = prop (SProxy :: SProxy "signer")

fileSigner :: Traversal' State (Maybe HashSigner)
fileSigner = fileProp <<< _Just <<< prop (SProxy :: SProxy "signer")

fileResult :: Traversal' State (Maybe FileHashResult)
fileResult = fileProp <<< _Just <<< prop (SProxy :: SProxy "result")

init :: State
init = {
  file: Nothing,
  signer: Nothing
}
