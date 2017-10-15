module App.Events.FileInputs where

import Prelude

import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Lib.Files (FileMeta)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent)


data Event =
  NoOp |
  NoFile |
  PreventDefault Event DOMEvent |
  NewFile FileMeta |
  FileError (Array String)


type State = Unit


type Effects eff =
  ( dom :: DOM
  , console :: CONSOLE
    | eff
    )


foldp ::
  forall eff.
  Event ->
  State ->
  EffModel State Event (Effects eff)

foldp NoOp state = noEffects $ state

foldp (PreventDefault next event) state =
  onlyEffects state $ [
    do
      liftEff $ preventDefault event
      pure $ Just $ next
    ]

foldp (FileError err) state =
  onlyEffects state $ [ (traverse log err) *> pure Nothing ]

foldp _ state = noEffects $ state
