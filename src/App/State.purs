module App.State where

import Data.Maybe (Maybe(..))


type State = {
  filename :: Maybe String,
  completed :: Boolean
}


init :: State
init = {
  filename: Nothing,
  completed: false
}
