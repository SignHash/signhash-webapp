module App.State where

import Data.Maybe (Maybe(..))
import Lib.Files (FileData)


type State = {
  file :: Maybe FileData,
  completed :: Boolean,
  hash :: Maybe String
}


init :: State
init = {
  file: Nothing,
  completed: false,
  hash: Nothing
}
