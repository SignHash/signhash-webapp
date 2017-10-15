module App.State where

import Prelude

import App.Hash.Types (Address, HashSigner, ProofMethod, ProofVerification)
import Data.Map (Map)
import Data.Lens (Traversal', _Just)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta)


type State = {
  file :: Maybe FileState,
  signer :: Maybe SignerState
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


type SignerState = {
  address :: Address,
  proofs :: SignerProofs
}


type SignerProofs = Map ProofMethod ProofState


data ProofState =
  Pending |
  NetworkError |
  Finished ProofVerification


fileProp :: Traversal' State (Maybe FileState)
fileProp = prop (SProxy :: SProxy "file")

fileSigner :: Traversal' State (Maybe HashSigner)
fileSigner = fileProp <<< _Just <<< prop (SProxy :: SProxy "signer")

fileResult :: Traversal' State (Maybe FileHashResult)
fileResult = fileProp <<< _Just <<< prop (SProxy :: SProxy "result")

signerProp :: Traversal' State (Maybe SignerState)
signerProp = prop(SProxy :: SProxy "signer")

signerProofs :: Traversal' State SignerProofs
signerProofs = signerProp <<< _Just <<< prop (SProxy :: SProxy "proofs")


init :: State
init = {
  file: Nothing,
  signer: Nothing
}
