module Main where

import Control.Monad.Eff (Eff)
import Data.Array (head)
import Data.Maybe (Maybe(..), maybe)
import Lib.Files (getEventFiles, getFileData, FileData)
import Prelude hiding (div)
import Pux (App, CoreEffects, EffModel, noEffects, start)
import Pux.DOM.Events (DOMEvent, onChange)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (div, input)
import Text.Smolder.HTML.Attributes (type')
import Text.Smolder.Markup (text, (!), (#!))

data Event = NewFile FileData | NoFile

type State = {
  filename :: Maybe String
}

type WebApp = App (DOMEvent -> Event) Event State


foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp NoFile state =
  noEffects $ state { filename = Nothing }
foldp (NewFile file) state =
  noEffects $ state { filename = Just file.name }


handleNewFile :: DOMEvent -> Event
handleNewFile evt = case file of
  Nothing -> NoFile
  Just f -> NewFile $ getFileData f
  where
    file = head $ getEventFiles evt

-- | Return markup from the state
view :: State -> HTML Event
view { filename } =
  div do
    input #! onChange handleNewFile ! type' "file"
    div $ text fileDescription
  where
    fileDescription =
      maybe "Please provide a file" id filename


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


initialState :: State
initialState = { filename: Nothing }
