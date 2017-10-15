module App.Events.Types where

import App.Events.Signers as Signers
import App.Events.Files as Files
import Lib.Files (FileMeta)
import Pux.DOM.Events (DOMEvent)


data Event =
  NoOp |
  NoFile |
  PreventDefault Event DOMEvent |
  NewFile FileMeta |
  FileError (Array String) |
  File Files.Event |
  Signer Signers.Event
