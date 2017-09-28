module Lib.Files where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.File.File (size)
import DOM.File.FileReader (fileReader)
import DOM.File.Types (Blob, File, FileReader)
import Data.Array ((..))
import Data.Foreign (F, Foreign, readString, toForeign, unsafeFromForeign, unsafeReadTagged)
import Data.Foreign.Index ((!))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Int (ceil, floor)
import Data.Traversable (for_, traverse)
import Pux.DOM.Events (DOMEvent)


type FileData = {
  name :: String,
  ref :: File
}

foreign import _sliceFile :: Fn3 File Int Int Blob
foreign import _readBlobAsync ::
  forall e.
  (String -> Eff e Unit) ->
  Blob ->
  FileReader ->
  Eff e Unit

readBlobAsync :: forall e. Blob -> FileReader -> Aff e String
readBlobAsync blob reader =
  makeAff (\error success -> _readBlobAsync success blob reader)


sliceFile :: File -> Int -> Int -> Blob
sliceFile = runFn3 _sliceFile


getFiles :: DOMEvent -> F (Array FileData)
getFiles event = do
  let obj = toForeign event
  files <- readFiles =<< (obj ! "target" ! "files")
  fileData <- traverse readFileData files
  pure fileData
  where
    readFiles :: Foreign -> F (Array Foreign)
    readFiles = unsafeReadTagged "FileList"

    readFileData :: Foreign -> F FileData
    readFileData f = do
      let ref = unsafeFromForeign f :: File
      name <- readString =<< f ! "name"
      pure { name, ref }


readFileContent ::
  forall eff.
  File ->
  Aff (dom :: DOM, console :: CONSOLE | eff) Unit
readFileContent file = do
  let chunkSize = 8
      chunks = (ceil $ size file) / chunkSize

  log $ "Size: " <> (show $ floor $ size file) <> " Bytes"

  reader <- liftEff fileReader

  for_ (0 .. chunks) \i -> do
    let blob = sliceFile file (i * chunkSize) ((i + 1) * chunkSize)
    chunk <- readBlobAsync blob reader
    log $ "Chunk " <> show i <> "\n" <> chunk

  pure unit
