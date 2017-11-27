module App.View where

import Text.Smolder.HTML hiding (address)

import App.State (Event(..), State)
import App.State.Contracts as Contracts
import App.State.FileInputs as FileInputs
import App.State.Files as Files
import App.State.Signers as Signers
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Lib.SignHash.Contracts (getAddress)
import Lib.SignHash.Proofs.Types (ProofVerification(..))
import Lib.SignHash.Types (HashSigner(..), ProofMethod, canonicalName)
import Prelude (discard, show, ($), (<>))
import Pux.DOM.Events (onChange, onDragOver, onDrop)
import Pux.DOM.HTML (HTML, child)
import Text.Smolder.HTML.Attributes (className, for, id, src, type')
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (Attribute, attribute, text, (!), (#!))


foreign import images ::
  { logo :: String }


dataQA :: String -> Attribute
dataQA = attribute "data-qa"


view :: State -> HTML Event
view { file, signer, contracts } =
  do
    div ! className "content" $ do
      div ! className "header" $ do
        img ! src images.logo
        h1 $ text "SignHash"
      hr
      child FileInput viewFileInput $ isJust file
      case file of
        Nothing -> empty
        Just loaded -> do
          viewFile loaded
          div $ signerStatus loaded.signer
      hr
      viewContracts contracts

  where
    signerStatus = case _ of
      Nothing -> h4 $ text "Loading signer..."
      Just (NoSigner) -> h4 $ text "No signers"
      Just (HashSigner s) -> do
        h4 $ text $ "Signers:"
        case signer of
          Nothing -> div do
            text loading
            hr
          Just value -> child Signer viewSigner $ value


viewContracts :: Contracts.State -> HTML Event
viewContracts Contracts.Loading =
  text $ "Loading contract..."
viewContracts (Contracts.Loaded state) = do
  span $ text $ "Contract: "
  span ! dataQA "contract-address"
    $ text $ show $ getAddress state.signerContract
viewContracts (Contracts.Error err) = do
  div
    ! A.title err
    $ text "Error while loading contract"


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

    renderSigner (HashSigner address) = show address
    renderSigner NoSigner = "Not signed"


viewSigner :: Signers.State -> HTML Signers.Event
viewSigner { address, proofs } = do
  div $ do
    h5 $ text $ "#" <> show address
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
        td ! dataQA tagname $ text $ msg
      where
        { icon, msg, cls } = proofDetails state
        tagname = "proof-details-" <> canonicalName method

    proofDetails Signers.Pending =
      { icon: "fa-spinner", msg: "", cls: ""}
    proofDetails Signers.NetworkError =
      { icon: "fa-exclamation-triangle"
      , msg: "Network error", cls: "network-error"}
    proofDetails (Signers.Finished (Verified proofValue)) =
      { icon: "fa-check", msg: "Verified: " <> proofValue, cls: "verified"}
    proofDetails (Signers.Finished (Unverified proofValue error)) =
      { icon: "fa-exclamation-circle"
      , msg: "Verification failed for " <> proofValue <> ": " <> show error
      , cls: "failed"
      }
    proofDetails (Signers.Finished Unavailable) =
      { icon: "fa-ban", msg: "No proof defined", cls: "not-defined"}


renderIcon :: forall a. String -> HTML a
renderIcon name = i ! className ("fa fa-lg " <> name) $ text ""


loading :: String
loading = "Loading..."


empty :: forall a. HTML a
empty = text ""
