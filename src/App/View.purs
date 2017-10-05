module App.View where

import App.Events.Creators (newFilesEvent)
import App.Events.Types (Event(..))
import App.State (FileState, State)
import App.Hash.Types (HashSigner(..))
import CSS as S
import CSS.TextAlign (textAlign, center)
import Data.Maybe (Maybe(..))
import Prelude hiding (div,id)
import Pux.DOM.Events (onChange, onDragOver, onDrop)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (div, input, label)
import Text.Smolder.HTML.Attributes (className, for, id, type')
import Text.Smolder.Markup (text, (!), (#!))


view :: State -> HTML Event
view { file } =
  div do
    label ! for "file-upload" ! className "custom-file-upload"
      ! style do
        S.fontSize (2.0 # S.em)
        textAlign center
        S.display S.inlineBlock
        S.width (500.0 # S.px)
        S.height (200.0 # S.px)
        S.lineHeight (200.0 # S.px)
        S.border S.solid (1.0 # S.px) S.black
      #! onDrop newFilesEvent
      #! onDragOver (PreventDefault NoOp)
      $ text "Click or drag and drop files"

    input ! id "file-upload"
      ! type' "file"
      ! style do
        S.display S.displayNone
      #! onChange newFilesEvent

    div fileStatus

  where
    fileStatus = case file of
      Nothing ->
        div $ text "Please provide a file"
      Just value -> do
        viewFile value


viewFile :: FileState -> HTML Event
viewFile { meta, result, signer } =
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
        signerComponent

    signerComponent = case signer of
      Nothing ->
        div $ text $ "Fetching signer..."
      Just (HashSigner address) ->
        div $ text $ "Signer address: " <> address
      Just NoSigner ->
        div $ text $ "Not signed"
