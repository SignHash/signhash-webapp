module App.State where

import App.Types (Signer)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta)


type FileState = {
  meta :: FileMeta,
  result :: Maybe {
    hash :: String,
    elapsed :: Seconds
  },
  signer :: Maybe Signer
}


type State = {
  file :: Maybe FileState
}


init :: State
init = {
  file: Nothing
}
