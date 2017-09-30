module App.Events.Types where

import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta)


data Event = NewFile FileMeta |
             FileError String |
             NoFile |
             FileLoaded {
               hash :: String,
               elapsed :: Seconds
             }
