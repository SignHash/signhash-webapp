module App.State where
import App.Hash.Types (HashSigner)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta)


type FileState = {
  meta :: FileMeta,
  result :: Maybe {
    hash :: String,
    elapsed :: Seconds
  },
  signer :: Maybe HashSigner
}


type State = {
  file :: Maybe FileState
}


init :: State
init = {
  file: Nothing
}
