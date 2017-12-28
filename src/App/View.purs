module App.View where

import Text.Smolder.HTML hiding (address)

import App.State (Event(..), State)
import App.State.Contracts as Contracts
import App.State.FileInputs as FileInputs
import App.State.Files as Files
import App.State.Signers as Signers
import Data.Array (fromFoldable)
import Data.Map (toUnfoldable, values)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (drop, length, take, toLower)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Lib.Eth.Contracts (class EthContract, getAddress)
import Lib.SignHash.Proofs.Display (SignerDisplayStatus(..), signerDisplayStatus)
import Lib.SignHash.Proofs.Methods (ProofMethod(..), canonicalName)
import Lib.SignHash.Proofs.Types (ProofState(..), ProofVerification(..))
import Lib.SignHash.Proofs.Values as ProofValues
import Lib.SignHash.Types (Address(..), HashSigner(..))
import Prelude (discard, show, ($), (<>), (-))
import Pux.DOM.Events (onChange, onDragOver, onDrop)
import Pux.DOM.HTML (HTML, child)
import Text.Smolder.HTML.Attributes (className, for, id, src, type')
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (Attribute, attribute, text, (!), (#!))


foreign import images ::
  { logo :: String
  , ethIcon :: String }


dataQA :: String -> Attribute
dataQA = attribute "data-qa"


view :: State -> HTML Event
view { file, signer, contracts } =
  do
    div ! className "Content" $ do
      div ! className "Header" $ do
        img ! src images.logo
        span ! className "title" $ text "SignHash"
        hr

      child FileInput viewFileInput $ isJust file
      case file of
        Nothing -> empty
        Just loaded -> do
          viewFile loaded
          div $ signerStatus loaded.signer

      hr
      div ! className "Contracts-info" $ do
        viewContracts contracts
      clear

  where
    signerStatus = case _ of
      Nothing -> signerHeader "Loading signer..."
      Just (NoSigner) -> signerHeader "No signers"
      Just (HashSigner s) -> do
        signerHeader "Signers"
        case signer of
          Nothing -> div do
            text loading
            hr
          Just value -> div do
            child Signer viewSigner $ value

    signerHeader msg =
      h4 ! className "Signer-header" $ text msg

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
  div
    ! A.title err
    $ text "Error while loading contract"


viewFileInput :: Boolean -> HTML FileInputs.Event
viewFileInput small =
  div do
    label
      ! for "file-upload"
      ! className ("File-upload" <> if small then " small" else "")
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
  div ! className "Files Section" $ do
    div ! className "row" $ do
      renderRow "Filename" $ text $ meta.name
      renderRow "SHA256" $ maybe (text loading) renderHash result
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
    proofStates = fromFoldable $ values proofs

    proofArray :: Array (Tuple ProofMethod ProofState)
    proofArray = toUnfoldable proofs

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
          methodIcon method
          span ! dataQA "error" ! A.title (show error)
            $ text $ show method <> " verification failed"

    renderFinishedProof method (Verified proofValue) =
      div
        ! className ("proof " <> "verified")
        ! dataQA ("proof-details-" <> canonicalName method)
        $ do
          methodIcon method
          a ! A.href (methodHref method proof) ! A.target "_blank" $ text proof
      where
        proof = ProofValues.extract proofValue

    renderNetworkError method =
      div ! className ("proof" <> "network-error") $ do
        methodIcon method
        text $ "Network error while fetching proof"

    renderEthProof =
      div ! className "proof" $ do
        renderEthIcon
        addressLink address

    methodIcon GitHub = renderIcon "fa-github"
    methodIcon HTTP = renderIcon "fa-world"

    methodHref GitHub username =
      "https://github.com/" <> username
    methodHref HTTP domain =
      "http://" <> domain


renderIcon :: forall a. String -> HTML a
renderIcon name = i ! className ("fa fa-lg " <> name) $ text ""


renderEthIcon :: forall a. Html a
renderEthIcon =
  img
  ! className "Icon"
  ! src images.ethIcon


renderBlockie :: forall a. String -> HTML a
renderBlockie blockieSrc =
  img ! className "blockie" ! src blockieSrc


loading :: String
loading = "Loading..."


empty :: forall a. HTML a
empty = text ""


clear :: forall a. HTML a
clear = div ! className "Clear" $ empty


addressLink :: forall a. Address -> HTML a
addressLink address = do
  a
  ! A.href (addressURL address)
  ! className "AddressURL"
  ! A.target "_blank"
  $ text $ show address


addressURL :: Address -> String
addressURL (Address address) =
  "https://etherscan.io/address/" <> address
