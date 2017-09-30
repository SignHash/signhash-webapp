module Main where

import App.Events (Event, foldp)
import App.State (State, init)
import App.View (view)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import Prelude hiding (div)
import Pux (App, CoreEffects, start)
import Pux.DOM.Events (DOMEvent)
import Pux.Renderer.React (renderToDOM)


type WebApp = App (DOMEvent -> Event) Event State
type AppEffects = Eff (
  CoreEffects ( console :: CONSOLE, dom :: DOM, now :: NOW ))


-- | Start and render the app
main :: String -> State -> AppEffects WebApp
main url state = do
  app <- start
    { initialState: state
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input

  pure app


initialState :: State
initialState = init
