module Main where

import Prelude

import App.Env (env)
import App.Routing (Location(Verify), routing)
import App.State (AppEffects, Event(..), State, foldp, init)
import App.View (view)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Pux (App, CoreEffects, start)
import Pux.DOM.Events (DOMEvent)
import Pux.Renderer.React (renderToDOM)
import Routing (matches)
import Signal (Signal, constant, (~>))
import Signal.Channel (channel, send, subscribe)


type WebApp = App (DOMEvent -> Event) Event State
type AllEffects = Eff (CoreEffects AppEffects)


foreign import load :: Unit


initApp :: Signal Event
initApp = constant Init


-- | Start and render the app
main :: String -> State -> AllEffects WebApp
main url state = do
  routingChannel <- channel { new: Verify, old: Nothing }

  let routingSignal = (subscribe routingChannel) ~> Routing

  app <- start
    { initialState: state
    , view
    , foldp
    , inputs: [initApp, routingSignal]
    }

  renderToDOM "#app" app.markup app.input

  matches routing \old new -> send routingChannel { old, new }

  pure app


initialState :: State
initialState = init env
