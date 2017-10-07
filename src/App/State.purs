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
  Error |
  Finished ProofVerification


fileProp :: Traversal' State FileState
fileProp = prop (SProxy :: SProxy "file") <<< _Just

fileSigner :: Traversal' State HashSigner
fileSigner = fileProp <<< prop (SProxy :: SProxy "signer") <<< _Just

fileResult :: Traversal' State FileHashResult
fileResult = fileProp <<< prop (SProxy :: SProxy "result") <<< _Just

signerProp :: Traversal' State SignerState
signerProp = prop(SProxy :: SProxy "signer") <<< _Just

signerProofs :: Traversal' State SignerProofs
signerProofs = signerProp <<< prop (SProxy :: SProxy "proofs")


init :: State
init = {
  file: Nothing,
  signer: Nothing
}
