module Main where

import Prelude

import App.Env (appEnvConfig)
import App.Routing (Location(Verify), routing)
import App.State (AppEffects, Event(..), State, InitEnv, foldp, init)
import App.State.Contracts (buildAccountsChannel, buildAccountsSignal)
import App.State.Locations (buildRoutingSignal)
import App.View (view)
import Control.Monad.Eff (Eff)
import Pux (App, CoreEffects, start)
import Pux.DOM.Events (DOMEvent)
import Pux.Renderer.React (renderToDOM)
import Routing (matches)
import Signal (Signal, constant)
import Signal.Channel (channel, send)


type WebApp = App (DOMEvent -> Event) Event State
type AllEffects = Eff (CoreEffects AppEffects)


foreign import load :: Unit


initApp :: InitEnv -> Signal Event
initApp env = constant $ Init env


-- | Start and render the app
main :: String -> State -> AllEffects WebApp
main url state = do
  routingChannel <- channel Verify
  ethAccountChannel <- buildAccountsChannel

  let
    initSignal = initApp { ethAccountChannel }
    routingSignal = Routing <$> buildRoutingSignal routingChannel
    ethAccountsSignal = Contract <$> buildAccountsSignal ethAccountChannel

  app <- start
    { initialState: state
    , view
    , foldp
    , inputs:
      [ initSignal
      , routingSignal
      , ethAccountsSignal
      ]
    }

  renderToDOM "#app" app.markup app.input

  matches routing \old new -> send routingChannel new

  pure app


initialState :: State
initialState = init appEnvConfig
