module App.Events.Types where

import App.Hash.Types (Address, HashSigner, ProofMethod, ProofVerification)
import Control.Monad.Eff.Exception (Error)
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
  FetchProof Address ProofMethod |
  ProofFetched Address ProofMethod ProofVerification |
  ProofFetchingError Address ProofMethod Error
