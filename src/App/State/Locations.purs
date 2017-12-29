module App.State.Locations where

import Prelude

import App.Routing (Location, toURL)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent)
import Signal (Signal, (~>))
import Signal.Channel (Channel, subscribe)


data Event
  = ViewLocation Location
  | Navigate Location DOMEvent


type State = Location


type Effects eff =
  ( dom :: DOM
  , history :: HISTORY
  | eff
  )


foldp :: forall eff. Event -> State -> EffModel State Event (Effects eff)
foldp (ViewLocation location) state =
  noEffects $ location
foldp (Navigate location event) state =
  onlyEffects state
  [ liftEff do
       let url = "#" <> toURL location
       preventDefault event
       h <- history =<< window
       pushState (toForeign {}) (DocumentTitle "") (URL url) h
       pure $ Just $ ViewLocation location
  ]


buildRoutingSignal :: Channel Location -> Signal Event
buildRoutingSignal channel = subscribe channel ~> ViewLocation
