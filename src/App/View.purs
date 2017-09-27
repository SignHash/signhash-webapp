module App.View where

import Prelude

import App.Events (Event, handleNewFile)
import App.State (State)
import Data.Maybe (maybe)
import Pux.DOM.Events (onChange)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, input)
import Text.Smolder.HTML.Attributes (type')
import Text.Smolder.Markup (text, (!), (#!))


view :: State -> HTML Event
view { filename } =
  div do
    input #! onChange handleNewFile ! type' "file"
    div $ text fileDescription
  where
    fileDescription =
      maybe "Please provide a file" id filename
