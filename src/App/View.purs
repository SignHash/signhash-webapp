module App.View where

import App.Events.Foldp (handleNewFile)
import App.Events.Types (Event)
import App.State (State, FileState)
import Data.Maybe (Maybe(..))
import Prelude hiding (div)
import Pux.DOM.Events (onChange)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, input)
import Text.Smolder.HTML.Attributes (type')
import Text.Smolder.Markup (text, (!), (#!))


view :: State -> HTML Event
view { file } =
  div do
    input #! onChange handleNewFile ! type' "file"
    div fileStatus

  where
    fileStatus = case file of
      Nothing ->
        div $ text "Please provide a file"
      Just value -> do
        viewFile value


viewFile :: FileState -> HTML Event
viewFile { meta, result } =
  div do
    div $ text ("Filename: " <> meta.name)
    div $ text ("Size: " <> show meta.size <> " Bytes")
    div resultComponent
  where
    resultComponent = case result of
      Nothing ->
        div $ text $ "Loading..."
      Just value -> do
        div $ text $ "sha256: " <> value.hash
        div $ text $ "elapsed:" <> show value.elapsed
