module App.View where

import Text.Smolder.HTML

import App.State (Event(..), State)
import App.State.FileInputs as FileInputs
import App.State.Files as Files
import App.State.Signers as Signers
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Lib.SignHash.Types (HashSigner(..), ProofMethod, ProofVerification(..))
import Prelude (discard, show, ($), (<>))
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
      child FileInput viewFileInput $ isJust file
      case file of
        Nothing -> empty
        Just value -> do
          viewFile value
          h4 $ text "Signers"
          div signerStatus

  where
    signerStatus = case signer of
      Nothing -> div do
        text loading
        hr
      Just value -> child Signer viewSigner $ value


viewFileInput :: Boolean -> HTML FileInputs.Event
viewFileInput small =
  div do
    label
      ! for "file-upload"
      ! className ("file-upload" <> if small then " small" else "")
      #! onDrop FileInputs.newFilesEvent
      #! onDragOver (FileInputs.PreventDefault FileInputs.NoOp)
      $ do
        renderIcon "fa-file-o"
        text "Verify a file"
        if small then empty else
          div ! className "hint"
          $ text "Click or drag and drop on the page"

    input
      ! id "file-upload"
      ! type' "file"
      #! onChange FileInputs.newFilesEvent


viewFile :: Files.State -> HTML Event
viewFile { meta, result, signer } = do
  table ! className "u-full-width file-info" $ tbody $ do
    renderRow "Name" meta.name
    renderRow "Size" $ show meta.size <> " Bytes"
    renderRow "SHA256" $ maybe loading _.hash result

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
  table ! className "u-full-width signers" $ do
    thead $ tr do
      th $ text "Method"
      th $ text "Status"
      th $ text "Details"
    tbody $ for_ unfolded renderProof
  where
    unfolded :: Array (Tuple ProofMethod Signers.ProofState)
    unfolded = toUnfoldable proofs

    renderProof (Tuple method state) = do
      tr ! className cls $ do
        td $ text $ show method
        td $ renderIcon icon
        td $ text $ msg
      where
        {icon, msg, cls} = proofDetails state

    proofDetails Signers.Pending =
      { icon: "fa-spinner", msg: "", cls: ""}
    proofDetails Signers.NetworkError =
      { icon: "fa-exclamation-triangle"
      , msg: "Network error", cls: "network-error"}
    proofDetails (Signers.Finished (Verified msg)) =
      { icon: "fa-check", msg, cls: "verified"}
    proofDetails (Signers.Finished (Unverified msg)) =
      { icon: "fa-exclamation-circle", msg: "Failed: " <> msg, cls: "failed" }
    proofDetails (Signers.Finished Unavailable) =
      { icon: "fa-ban", msg: "No proof defined", cls: "not-defined"}


renderIcon :: forall a. String -> HTML a
renderIcon name = i ! className ("fa fa-lg " <> name) $ text ""


loading :: String
loading = "Loading..."


empty :: forall a. HTML a
empty = text ""
