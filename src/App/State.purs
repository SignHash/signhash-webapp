module App.State where

import Data.Maybe (Maybe(..))


type State = {
  filename :: Maybe String
}


init :: State
init = {
  filename: Nothing
}
