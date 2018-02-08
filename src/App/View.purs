module App.View where

import Text.Smolder.HTML hiding (address)

import App.Routing as Routing
import App.State (Event(..), State, signerLens)
import App.State.Contracts as Contracts
import App.State.FileInputs as FileInputs
import App.State.Files as Files
import App.State.Signers as Signers
import App.View.Common (addressLink, clear, dataQA, empty, expectResult, guardAccountUnlocked, ignoreEvent, images, loading, navigate, onClickAction, preventingDefault, proofHref, proofMethodIcon, renderBlockie, renderContractsLoadingError, renderEthIcon, renderIcon, renderLinkIcon, renderList, renderSection, renderSectionHighlighted, renderSectionWarning, sectionHeader, sectionStatus, txLink)
import App.View.Identity (viewIdentity)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Lens ((^.))
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (drop, length, take, toLower)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Lib.Eth.Contracts (class EthContract, ContractLoadingError(..), getAddress)
import Lib.SignHash.Proofs.Display (SignerDisplayStatus(..), signerDisplayStatus)
import Lib.SignHash.Proofs.Methods (ProofMethod(..), canonicalName)
import Lib.SignHash.Proofs.Types (ProofState(..), ProofVerification(..))
import Lib.SignHash.Proofs.Values as ProofValues
import Lib.SignHash.Types (Address, HashSigner(..))
import Prelude (discard, not, show, ($), (-), (<>), (==))
import Pux.DOM.Events (onChange, onClick, onDragOver, onDrop)
import Pux.DOM.HTML (HTML, child)
import Text.Smolder.HTML.Attributes (className, for, id, src, type')
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (text, (!), (#!))


view :: State -> HTML Event
view state =
  do
    div ! className "Content" $ do
      div ! className "Header" $ do
        img ! src images.logo
        span ! className "title" $ text "SignHash"

      nav ! className "Navbar" $ do
        ul do
          li $ a ! A.href "#" #! onClick (navigate Routing.Identity) $ text "Account"
          li $ a ! A.href "#" #! onClick (navigate Routing.Sign) $ text "Sign"
          li $ a ! A.href "#" #! onClick (navigate Routing.Verify) $ text "Verify"

      viewContent state

      hr
      div ! className "Contracts-info" $ do
        viewContracts state.contracts
      clear


viewContent :: State -> HTML Event
viewContent state@{ contracts: Contracts.Error error } = do
  renderContractsLoadingError error
viewContent state@{ location: Routing.Verify } = do
  withFileDetails state.file $ \file -> do
    viewFileSigners state file
viewContent state@{ location: Routing.Sign } = do
  withFileDetails state.file $ \file -> do
    sectionHeader "Signing address"
    guardAccountUnlocked state.myAccount $
      viewSigningDetails state file
viewContent state@{ location: Routing.Identity } = do
  viewIdentity state


withFileDetails ::
  Maybe Files.State
  -> (Files.State -> HTML Event)
  -> HTML Event
withFileDetails fileState viewDetails = do
  viewFileInput (isJust fileState)
  case fileState of
    Nothing -> empty
    Just file -> do
      viewFile file
      viewDetails file


viewFileSigners :: State -> Files.State -> HTML Event
viewFileSigners state file = do
  div $ signerStatus file.signer
  where
    signerStatus = case _ of
      Nothing -> sectionHeader "Loading signer..."
      Just NoSigner -> do
        renderSectionWarning do
          sectionStatus (renderIcon "fa-times") do
            text "This file has not been signed."
          a
            ! A.className "Button"
            #! onClick (navigate Routing.Sign)
            $ text "Sign it"

      Just (HashSigner address) -> do
        let selfSigned = case state.myAccount of
              Contracts.Available myAccount -> myAccount == address
              _ -> false
        sectionHeader "Signers"
        renderSection do
          if selfSigned then
            sectionStatus (renderIcon "fa-check-circle-o") do
            text "You have signed this file."
            else empty

          case state ^. signerLens address of
            Nothing -> div do
              loading
              hr
            Just value -> do
              renderList
                [ child (Signer address) viewSigner $ value
                , child (Signer address) viewSigner $ value
                , child (Signer address) viewSigner $ value
                ]
        if not selfSigned then
          a
            ! A.className "Button block"
            #! onClick (navigate Routing.Sign)
            $ text "Add your signature"
          else empty


viewSigningDetails :: State -> Files.State -> Address -> HTML Event
viewSigningDetails state file address = do
  div ! dataQA "my-id" $ do
    let myDetails = expectResult $ state ^. signerLens address
    renderSectionHighlighted do
      child (Signer address) viewSigner $ myDetails
    case file.result of
      Nothing -> empty
      Just fileDetails ->
        case state.signingTx of
          Nothing ->
            a
              ! A.href "#"
              ! dataQA "sign"
              ! A.className "Button block"
              #! onClickAction (SignFile fileDetails.hash)
              $ do
                text "Sign it"
                renderLinkIcon "fa-paper-plane-o"
          Just (Left err) ->
            div
              ! A.className "Button block disabled"
              $ text "Error while issuing transaction"

          Just (Right hash) -> do
            let txStatus = Contracts.viewTxResult hash state.contracts
            div
              ! A.className "Button block disabled"
              $ txLink hash txStatus


viewContracts :: Contracts.State -> HTML Event
viewContracts Contracts.Loading =
  text $ "Loading contract..."
viewContracts (Contracts.Loaded state) = do
  contractLink "SignHash" state.signHash
  br
  contractLink "SignProof" state.signProof
  where
    contractLink :: forall c. EthContract c => String -> c -> HTML Event
    contractLink name contract = do
      span $ text $ name <> ": "
      (addressLink $ getAddress contract)
        ! dataQA ((toLower name) <> "-address")

viewContracts (Contracts.Error err) = do
  let msg = case err of
        NotDeployedToNetwork id ->
          "Contracts have not been deployed to selected network"
  div $ text msg


viewFileInput :: Boolean -> HTML Event
viewFileInput small =
  div do
    label
      ! for "file-upload"
      ! className ("File-upload" <> if small then " small" else "")
      #! onDrop handleNewFiles
      #! onDragOver ignoreEvent
      $ do
        renderIcon "fa-file-o"
        text (if small then "Upload another file" else "Upload file")
        if small then empty else
          div ! className "hint"
          $ text "Click or drag and drop on the page"
    input
      ! id "file-upload"
      ! type' "file"
      #! onChange handleNewFiles
  where
    handleNewFiles ev =
      preventingDefault (FileInput $ FileInputs.newFilesEvent ev) ev


viewFile :: Files.State -> HTML Event
viewFile { meta, result, signer } = do
  div ! className "Files Section" $ do
    div ! className "row" $ do
      renderRow "Filename" $ text $ meta.name
      renderRow "SHA256" $ maybe loading renderHash result
      renderRow "Size" $ renderSize meta.size

  where
    renderRow label value =
      div ! className "columns attribute four" $ do
        span ! className "header" $ text (label <> ":")
        span ! className "value" $ value

    renderSigner (HashSigner address) = show address
    renderSigner NoSigner = "Not signed"

    renderHash { hash } =
      let visibleOffset = 6
      in a
         ! dataQA "checksum"
         ! A.title hash
         ! A.href "#"
         $ text $ take visibleOffset hash
           <> "..."
           <> drop (length hash - visibleOffset) hash

    renderSize size = text $ show meta.size <> " B"


viewSigner :: Signers.State -> HTML Signers.Event
viewSigner { address, proofs, blockie } = do
  div ! className "Signer" $ do
    div ! className "header" $ do
      span ! className "status" $ do
        viewSignerStatus
    viewProofs address proofArray
    clear
  where
    proofStates = fromFoldable $ Map.values proofs

    proofArray :: Array (Tuple ProofMethod ProofState)
    proofArray = Map.toUnfoldable proofs

    viewSignerStatus =
      case signerDisplayStatus proofStates of
        SignerLoading -> renderIcon "fa-circle-o-notch loading"
        SignerVerified -> renderBlockie blockie
        SignerVerificationFailed -> renderIcon "fa-exclamation-circle failed"
        SignerNetworkError -> renderIcon "fa-question-circle-o network-error"


viewProofs ::
  Address
  -> Array (Tuple ProofMethod ProofState)
  -> HTML Signers.Event
viewProofs address proofs =
  div ! className "proofs" $ do
    renderEthProof
    for_ proofs renderProof
  where
    renderProof (Tuple method state) = do
      case state of
        Finished result -> renderFinishedProof method result
        NetworkError -> renderNetworkError method
        Pending -> empty

    renderFinishedProof method Unavailable = empty
    renderFinishedProof method (Unverified error) =
      div
        ! className ("proof " <> "failed")
        ! dataQA ("proof-details-" <> canonicalName method)
        $ do
          proofMethodIcon method
          span ! dataQA "error" ! A.title (show error)
            $ text $ show method <> " verification failed"

    renderFinishedProof method (Verified proofValue) =
      div
        ! className ("proof " <> "verified")
        ! dataQA ("proof-details-" <> canonicalName method)
        $ do
          proofMethodIcon method
          a ! A.href (proofHref method proof) ! A.target "_blank" $ text proof
      where
        proof = ProofValues.extract proofValue

    renderNetworkError method =
      div ! className ("proof" <> "network-error") $ do
        proofMethodIcon method
        text $ "Network error while fetching proof"

    renderEthProof =
      div ! className "proof" $ do
        renderEthIcon
        (addressLink address) ! dataQA "proof-details-eth"
