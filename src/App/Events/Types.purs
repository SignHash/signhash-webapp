module App.Events.Types where

import Lib.Files (FileData)

data Event = NewFile FileData |
             FileError String |
             NoFile |
             FileLoaded
