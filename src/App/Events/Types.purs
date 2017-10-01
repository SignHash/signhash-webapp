module App.Events.Types where

import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta)

import Pux.DOM.Events (DOMEvent)


data Event = NewFile DOMEvent FileMeta |
             DragFiles DOMEvent |
             FileError DOMEvent String |
             NoFile |
             FileLoaded {
               hash :: String,
               elapsed :: Seconds
             }
