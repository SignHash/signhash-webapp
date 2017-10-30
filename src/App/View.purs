module App.View where

import Text.Smolder.HTML

import App.State (Event(..), State)
import App.State.FileInputs as FileInputs
import App.State.Files as Files
import App.State.Signers as Signers
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Lib.SignHash.Types (HashSigner(..), ProofMethod, ProofVerification(..))
import Prelude (Unit, discard, show, unit, ($), (<>))
import Pux.DOM.Events (onChange, onDragOver, onDrop)
import Pux.DOM.HTML (HTML, child)
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
      case file of
        Nothing -> text $ ""
        Just value -> do
          h4 $ text "Signers"
          div signerStatus
      h4 $ text "File status"
      div fileStatus

  where
    fileStatus = case file of
      Nothing ->
        div $ text "Please provide a file"
      Just value -> viewFile value

    signerStatus = case signer of
      Nothing -> div do
        text loading
        hr
      Just value -> child Signer viewSigner value


viewFileInput :: Unit -> HTML FileInputs.Event
viewFileInput _ =
  div do
    label
      ! for "file-upload"
      ! className "file-upload"
      #! onDrop FileInputs.newFilesEvent
      #! onDragOver (FileInputs.PreventDefault FileInputs.NoOp)
      $ div do
        h3 $ text "Click or drag and drop files"

    input
      ! id "file-upload"
      ! type' "file"
      #! onChange FileInputs.newFilesEvent


viewFile :: Files.State -> HTML Event
viewFile { meta, result, signer } =
  table ! className "u-full-width" $ tbody $ do
    renderRow "FileName" meta.name
    renderRow "Size" $ show meta.size <> " Bytes"
    renderRow "SHA256" $ maybe loading _.hash result
    renderRow "Signer address" $ maybe loading renderSigner signer

  where
    renderRow label value =
      tr $ do
        th $ text label
        td $ text value

    renderSigner (HashSigner address) = address
    renderSigner NoSigner = "Not signed"


viewSigner :: Signers.State -> HTML Signers.Event
viewSigner { address, proofs } = do
  div $ do
    h5 $ text $ "#" <> address
    viewProofs proofs


viewProofs :: Signers.SignerProofs -> HTML Signers.Event
viewProofs proofs =
  table ! className "u-full-width" $ do
    thead $ tr do
      th $ text "Method"
      th $ text "Status"
      th $ text "Details"
    tbody $ for_ unfolded renderProof
  where
    unfolded :: Array (Tuple ProofMethod Signers.ProofState)
    unfolded = toUnfoldable proofs

    renderProof (Tuple method state) = do
      tr do
        td $ text $ show method
        td $ renderIcon icon
        td $ text $ msg
      where
        (Tuple icon msg) = proofDetails state

    proofDetails Signers.Pending =
      Tuple "fa fa-spinner" ""
    proofDetails Signers.NetworkError =
      Tuple "fa fa-exclamation-triangle" "Network error"
    proofDetails (Signers.Finished (Verified msg)) =
      Tuple "fa fa-check" msg
    proofDetails (Signers.Finished (Unverified msg)) =
      Tuple "fa fa-exclamation-circle" $ "Failed: " <> msg
    proofDetails (Signers.Finished Unavailable) =
      Tuple "fa fa-ban" "No proof defined"


renderIcon :: forall a. String -> HTML a
renderIcon name = i ! className ("fa-lg " <> name) $ text ""


loading :: String
loading = "Loading..."
