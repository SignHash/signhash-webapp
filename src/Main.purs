module Main where

import App.State (AppEffects, Event(..), State, foldp, init)
import App.View (view)
import Control.Monad.Eff (Eff)
import Prelude hiding (div)
import Pux (App, CoreEffects, start)
import Pux.DOM.Events (DOMEvent)
import Pux.Renderer.React (renderToDOM)
import Signal (Signal, constant)


type WebApp = App (DOMEvent -> Event) Event State
type AllEffects = Eff (CoreEffects AppEffects)


foreign import load :: Unit


initApp :: Signal Event
initApp = constant Init


-- | Start and render the app
main :: String -> State -> AllEffects WebApp
main url state = do
  app <- start
    { initialState: state
    , view
    , foldp
    , inputs: [initApp]
    }

  renderToDOM "#app" app.markup app.input

  pure app


initialState :: State
initialState = init
