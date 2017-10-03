module App.Events.Foldp where

import Prelude

import App.Effects (fetchSigners, preventDefaultEffect, processDOMFiles, processNewFile)
import App.Events.Types (Event(..))
import App.Hash.Worker (WORKER)
import App.State (State)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)


type AppEffects = (
    console :: CONSOLE,
    dom :: DOM,
    now :: NOW,
    worker :: WORKER,
    ajax :: AJAX
)


foldp ::
  Event ->
  State ->
  EffModel State Event AppEffects
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
        result: Nothing,
        signer: Nothing
        }
     },
  effects: [ processNewFile file ]
}
foldp (FileError err) state = {
  state,
  effects: [ log err *> pure Nothing ]
}
foldp (HashCalculated event) state = {
  state: state { file = updateHash <$> state.file },
  effects: [ fetchSigners event.hash ]
}
  where
    updateHash file = file { result = Just event }

foldp (SignerFetched signer) state =
  noEffects $ state { file = updateSigner <$> state.file }
  where
    updateSigner file = file { signer = Just signer }
