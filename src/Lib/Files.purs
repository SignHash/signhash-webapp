module Lib.Files where

import Prelude

import DOM.File.Types (File)
import Data.Foreign (F, Foreign, readString, toForeign, unsafeFromForeign, unsafeReadTagged)
import Data.Foreign.Index ((!))
import Data.Traversable (traverse)
import Pux.DOM.Events (DOMEvent)


type FileData = {
  name :: String,
  ref :: File
}

readFiles :: Foreign -> F (Array Foreign)
readFiles = unsafeReadTagged "FileList"

getFiles :: DOMEvent -> F (Array FileData)
getFiles event = do
  let obj = toForeign event
  files <- readFiles =<< (obj ! "target" ! "files")
  fileData <- traverse readFileData files
  pure fileData
  where
    readFileData :: Foreign -> F FileData
    readFileData f = do
      let ref = unsafeFromForeign f :: File
      name <- readString =<< f ! "name"
      pure { name, ref }
