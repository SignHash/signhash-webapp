module Lib.Files where

import Prelude

import Control.Alternative ((<|>))
import DOM.File.File (size)
import DOM.File.Types (File)
import Data.Foreign (F, Foreign, readString, toForeign, unsafeFromForeign, unsafeReadTagged)
import Data.Foreign.Index ((!))
import Data.Int (ceil)
import Data.Traversable (traverse)
import Pux.DOM.Events (DOMEvent)


type FileMeta = {
  name :: String,
  size :: Int,
  ref :: File
}


getFilesFromEvent :: DOMEvent -> F (Array FileMeta)
getFilesFromEvent event = do
  let obj = toForeign event
  files <- getFiles obj
  fileData <- traverse readFileMeta files
  pure fileData
  where
    getFiles :: Foreign -> F (Array Foreign)
    getFiles obj =
      (readFiles =<< (obj ! "target" ! "files"))
      <|>
      (readFiles =<< (obj ! "dataTransfer" ! "files"))

    readFiles :: Foreign -> F (Array Foreign)
    readFiles = unsafeReadTagged "FileList"

    readFileMeta :: Foreign -> F FileMeta
    readFileMeta f = do
      let ref = unsafeFromForeign f :: File
      name <- readString =<< f ! "name"
      pure {
        name,
        size: ceil $ size ref,
        ref
      }
