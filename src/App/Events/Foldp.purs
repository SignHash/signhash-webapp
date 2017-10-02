module App.Events.Foldp where

import Prelude

import App.Effects (preventDefaultEffect, processDOMFiles, processNewFile)
import App.Events.Types (Event(..))
import App.Hash.Worker (WORKER)
import App.State (State)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import Data.Maybe (Maybe(..), maybe)
import Pux (EffModel, noEffects)


foldp ::
  Event ->
  State ->
  EffModel State Event (
    console :: CONSOLE, dom :: DOM, now :: NOW, worker :: WORKER
  )
foldp (DOMDragFiles event) state = {
  state,
  effects: [ preventDefaultEffect event ]
}
foldp (DOMNewFiles event) state = {
  state,
  effects: [ processDOMFiles event ]
}
foldp NoFile state =
  noEffects $ state { file = Nothing }
foldp (NewFile file) state = {
  state: state {
     file = Just {
        meta: file,
        result: Nothing
        }
     },
  effects: [ processNewFile file ]
}
foldp (FileError err) state = {
  state,
  effects: [ log err *> pure Nothing ]
}
foldp (HashCalculated event) state =
  noEffects $ state { file = updateHash state.file }
  where
    updateHash = maybe Nothing (\file -> Just $ file { result = Just event })
