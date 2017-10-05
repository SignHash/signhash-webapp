module App.State where
import App.Hash.Types (Address, HashSigner, ProofMethod, ProofVerification)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta)


type State = {
  file :: Maybe FileState,
  signer :: Maybe SignerState
}


type FileState = {
  meta :: FileMeta,
  result :: Maybe {
    hash :: String,
    elapsed :: Seconds
  },
  signer :: Maybe HashSigner
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


init :: State
init = {
  file: Nothing,
  signer: Nothing
}
