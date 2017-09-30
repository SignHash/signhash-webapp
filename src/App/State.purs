module App.State where

import Data.Maybe (Maybe(..))


type State = {
  filename :: Maybe String,
  completed :: Boolean,
  hash :: Maybe String
}


init :: State
init = {
  filename: Nothing,
  completed: false,
  hash: Nothing
}
