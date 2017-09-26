module Lib.Files where

import Pux.DOM.Events (DOMEvent)

type FileData = {
  name :: String
}

foreign import data File :: Type

foreign import getEventFiles :: DOMEvent -> Array File
foreign import getFileData :: File -> FileData
