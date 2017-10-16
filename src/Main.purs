module Main where

import App.State (AppEffects, foldp, Event, State, init)
import App.View (view)
import Control.Monad.Eff (Eff)
import Prelude hiding (div)
import Pux (App, CoreEffects, start)
import Pux.DOM.Events (DOMEvent)
import Pux.Renderer.React (renderToDOM)


type WebApp = App (DOMEvent -> Event) Event State
type AllEffects = Eff (CoreEffects AppEffects)


-- | Start and render the app
main :: String -> State -> AllEffects WebApp
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
