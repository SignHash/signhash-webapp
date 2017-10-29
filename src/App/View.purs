module App.View where

import App.State (Event(..), State)
import App.State.FileInputs as FileInputs
import App.State.Files as Files
import App.State.Signers as Signers
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Lib.SignHash.Types (HashSigner(..), ProofMethod, ProofVerification(..))
import Prelude hiding (div,id)
import Pux.DOM.Events (onChange, onDragOver, onDrop)
import Pux.DOM.HTML (HTML, child)
import Text.Smolder.HTML (div, h1, hr, input, label, li, table, tbody, td, th, thead, tr, ul)
import Text.Smolder.HTML.Attributes (className, for, id, type')
import Text.Smolder.Markup (text, (!), (#!))


view :: State -> HTML Event
view { file, signer } =
  do
    div ! className "content" $ do
      h1 $ text "SignHash"
      hr
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
    label
      ! for "file-upload"
      ! className "file-upload"
      #! onDrop FileInputs.newFilesEvent
      #! onDragOver (FileInputs.PreventDefault FileInputs.NoOp)
      $ text "Click or drag and drop files"

    input
      ! id "file-upload"
      ! type' "file"
      #! onChange FileInputs.newFilesEvent


viewFile :: Files.State -> HTML Event
viewFile { meta, result, signer } =
  div do
    table ! className "u-full-width" $ tbody $ do
      renderRow "FileName" meta.name
      renderRow "Size" $ show meta.size <> " Bytes"
      renderRow "SHA256" $ maybe loading _.hash result
      renderRow "Signer address" $ maybe loading renderSigner signer

  where
    loading = "Loading..."
    renderRow label value =
      tr $ do
        th $ text label
        td $ text value

    renderSigner (HashSigner address) = address
    renderSigner NoSigner = "Not signed"


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
