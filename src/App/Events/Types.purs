module App.Events.Types where

import App.Hash.Types (HashSigner)
import App.Events.Signers as Signers
import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta)
import Pux.DOM.Events (DOMEvent)


data Event =
  NoOp |
  PreventDefault Event DOMEvent |
  NewFile FileMeta |
  FileError (Array String) |
  NoFile |
  HashCalculated {
    hash :: String,
    elapsed :: Seconds
    } |
  SignerFetched HashSigner |
  Signer Signers.Event
