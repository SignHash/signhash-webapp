module App.View where

import App.State.Signers as Signers
import App.State.Files as Files
import App.State.FileInputs as FileInputs
import App.State (Event(..), State)
import Lib.SignHash.Types (HashSigner(..), ProofMethod, ProofVerification(..))
import CSS as S
import CSS.TextAlign (textAlign, center)
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Prelude hiding (div,id)
import Pux.DOM.Events (onChange, onDragOver, onDrop)
import Pux.DOM.HTML (HTML, child)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (div, hr, input, label, li, ul)
import Text.Smolder.HTML.Attributes (className, for, id, type')
import Text.Smolder.Markup (text, (!), (#!))


view :: State -> HTML Event
view { file, signer } =
  do
    child FileInput viewFileInput unit

    hr
    div fileStatus
    hr
    div signerStatus

  where
    fileStatus = case file of
      Nothing ->
        div $ text "Please provide a file"
      Just value -> viewFile value

    signerStatus = case signer of
      Nothing -> div $ text "No signer"
      Just value -> child Signer viewSigner value


viewFileInput :: Unit -> HTML FileInputs.Event
viewFileInput _ =
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
      #! onDrop FileInputs.newFilesEvent
      #! onDragOver (FileInputs.PreventDefault FileInputs.NoOp)
      $ text "Click or drag and drop files"

    input ! id "file-upload"
      ! type' "file"
      ! style do
        S.display S.displayNone
      #! onChange FileInputs.newFilesEvent




viewFile :: Files.State -> HTML Event
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


viewSigner :: Signers.State -> HTML Signers.Event
viewSigner { address, proofs } =
  div do
    div $ text "Signer"
    div $ text ("Address: " <> address)
    div $ text "Proofs:"
    viewProofs proofs


viewProofs :: Signers.SignerProofs -> HTML Signers.Event
viewProofs proofs =
  ul $ for_ unfolded renderProof
  where
    unfolded :: Array (Tuple ProofMethod Signers.ProofState)
    unfolded = toUnfoldable proofs
    renderProof (Tuple method state) = do
      li do
        div $ text ("Method: " <> show method)
        div $ text ("Verification: " <> viewProofState state)

    viewProofState Signers.Pending = "Pending..."
    viewProofState Signers.NetworkError = "Network error"
    viewProofState (Signers.Finished (Verified msg)) =
      "Verified: " <> msg
    viewProofState (Signers.Finished (Unverified msg)) =
      "Verification failed: " <> msg
    viewProofState (Signers.Finished Unavailable) =
      "No proof defined"
