module App.View where

import App.Events.Foldp (handleNewFile)
import App.Events.Types (Event)
import App.State (State)
import Data.Maybe (Maybe(..), maybe)
import Prelude hiding (div)
import Pux.DOM.Events (onChange)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, input)
import Text.Smolder.HTML.Attributes (type')
import Text.Smolder.Markup (text, (!), (#!))


view :: State -> HTML Event
view { filename, completed, hash } =
  div do
    input #! onChange handleNewFile ! type' "file"
    div fileStatus

  where
    fileStatus = case filename of
      Nothing ->
        div $ text "Please provide a file"
      Just value -> do
        div $ text ("Filename: " <> value)
        div $ text $ "Status:" <> statusText
        div $ text $ "sha256:" <> hashText

    hashText = maybe "unknown" id hash

    statusText =
      if completed then
        "Completed"
      else
        "Loading"
