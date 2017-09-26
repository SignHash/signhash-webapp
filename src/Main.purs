module Main where

import Prelude hiding (div)
import Control.Monad.Eff (Eff)
import Pux (App, CoreEffects, EffModel, start)
import Pux.DOM.Events (DOMEvent, onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, span)
import Text.Smolder.Markup (text, (#!))

data Event = Increment | Decrement

type State = Int

type WebApp = App (DOMEvent -> Event) Event State


-- | Return a new state (and effects) from each event
foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp Increment n = { state: n + 1, effects: [] }
foldp Decrement n = { state: n - 1, effects: [] }

-- | Return markup from the state
view :: State -> HTML Event
view count =
  div do
    button #! onClick (const Increment) $ text "Increment"
    span $ text (show count)
    button #! onClick (const Decrement) $ text "Decrement"

-- | Start and render the app
main :: ∀ fx. String -> State -> Eff (CoreEffects fx) WebApp
main url state = do
  app <- start
    { initialState: state
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input

  pure app


initialState :: Int
initialState = 0
