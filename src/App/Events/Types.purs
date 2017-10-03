module App.Events.Types where

import App.Types (Signer)
import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta)
import Pux.DOM.Events (DOMEvent)


data Event =
  DOMNewFiles DOMEvent |
  DOMDragFiles DOMEvent |
  NewFile FileMeta |
  FileError String |
  NoFile |
  HashCalculated {
    hash :: String,
    elapsed :: Seconds
    } |
  SignerFetched Signer
