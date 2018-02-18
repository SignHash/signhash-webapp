module Main where

import Prelude

import App.Env (appEnvConfig)
import App.Routing (routing)
import App.State (AppEffects, Event, State, buildEnv, foldp, getInputs, init)
import App.View (view)
import Control.Monad.Eff (Eff)
import Pux (App, CoreEffects, start)
import Pux.DOM.Events (DOMEvent)
import Pux.Renderer.React (renderToDOM)
import Routing (matches)
import Signal.Channel (send)


type WebApp = App (DOMEvent -> Event) Event State
type AllEffects = Eff (CoreEffects AppEffects)


foreign import load :: Unit


-- | Start and render the app
main :: String -> State -> AllEffects WebApp
main url state = do
  env <- buildEnv state

  let inputs = getInputs env

  app <- start
    { initialState: state
    , view
    , foldp: foldp env
    , inputs: inputs
    }

  renderToDOM "#app" app.markup app.input

  matches routing \old new -> send env.routingChannel new

  pure app


initialState :: State
initialState = init appEnvConfig
