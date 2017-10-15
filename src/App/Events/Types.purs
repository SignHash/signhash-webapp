module App.Events.Types where

import App.Events.Signers as Signers
import App.Events.Files as Files
import App.Events.FileInputs as FileInputs


data Event =
  FileInput FileInputs.Event |
  File Files.Event |
  Signer Signers.Event
