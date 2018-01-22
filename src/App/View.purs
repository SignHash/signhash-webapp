module App.View where

import Text.Smolder.HTML hiding (address)

import App.Routing (Location(..))
import App.State (Event(..), State, signerLens)
import App.State.Contracts as Contracts
import App.State.FileInputs as FileInputs
import App.State.Files as Files
import App.State.Locations as Locations
import App.State.Signers as Signers
import Data.Array (fromFoldable, mapWithIndex)
import Data.Either (Either(..))
import Data.Lens ((^.))
import Data.Map (toUnfoldable, values)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.String (drop, length, take, toLower)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Lib.Eth.Contracts (class EthContract, getAddress)
import Lib.Eth.Web3 (TxHash(..), TxStatus(..))
import Lib.SignHash.Proofs.Display (SignerDisplayStatus(..), signerDisplayStatus)
import Lib.SignHash.Proofs.Methods (ProofMethod(..), canonicalName)
import Lib.SignHash.Proofs.Types (ProofState(..), ProofVerification(..))
import Lib.SignHash.Proofs.Values as ProofValues
import Lib.SignHash.Types (Address(..), HashSigner(..))
import Partial.Unsafe (unsafePartial)
import Prelude (discard, mod, not, show, ($), (-), (<<<), (<>), (==))
import Pux.DOM.Events (DOMEvent, onChange, onClick, onDragOver, onDrop)
import Pux.DOM.HTML (HTML, child)
import Text.Smolder.HTML.Attributes (className, for, id, src, type')
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (Attribute, EventHandlers, attribute, text, (!), (#!))


foreign import images ::
  { logo :: String
  , ethIcon :: String }


dataQA :: String -> Attribute
dataQA = attribute "data-qa"


view :: State -> HTML Event
view state =
  do
    div ! className "Content" $ do
      div ! className "Header" $ do
        img ! src images.logo
        span ! className "title" $ text "SignHash"

      nav ! className "Navbar" $ do
        ul do
          li $ a ! A.href "#" $ text "Account"
          li $ a ! A.href "#" #! onClick (navigate Sign) $ text "Sign"
          li $ a ! A.href "#" #! onClick (navigate Verify) $ text "Verify"

      viewContent state

      hr
      div ! className "Contracts-info" $ do
        viewContracts state.contracts
      clear


viewContent :: State -> HTML Event
viewContent state@{ location: Verify } = do
  withFileDetails state.file $ \file -> do
    viewFileSigners state file
viewContent state@{ location: Sign } = do
  withFileDetails state.file $ \file -> do
    sectionHeader "Signing address"
    guardAccountUnlocked state.myAccount $
      viewSigningDetails state file


withFileDetails ::
  Maybe Files.State
  -> (Files.State -> HTML Event)
  -> HTML Event
withFileDetails file viewDetails = do
  viewFileInput (isJust file)
  case file of
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
            #! onClick (navigate Sign)
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
              text loading
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
            #! onClick (navigate Sign)
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
  div
    ! A.title err
    $ text "Error while loading contract"


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
        (addressLink address) ! dataQA "proof-details-eth"

    methodIcon GitHub = renderIcon "fa-github"
    methodIcon HTTP = renderIcon "fa-world"

    methodHref GitHub username =
      "https://github.com/" <> username
    methodHref HTTP domain =
      "http://" <> domain



guardAccountUnlocked ::
  Contracts.ETHAccountState Address -> (Address -> HTML Event) -> HTML Event
guardAccountUnlocked state view = do
  case state of
    Contracts.Unavailable -> do
      renderSectionWarning do
        sectionStatus (renderIcon "fa-wrench") do
          text "Please install MetaMask extension"
    Contracts.Locked ->
      renderSectionWarning do
        sectionStatus (renderIcon "fa-unlock-alt") do
          text "Please unlock MetaMask extension"
    Contracts.Available address -> view address


sectionHeader :: forall a. String -> HTML a
sectionHeader msg =
  h4 ! className "Section-header" $ text msg


renderSection :: forall a. HTML a -> HTML a
renderSection = div ! className "Section"


renderSectionWarning :: forall a. HTML a -> HTML a
renderSectionWarning = div ! className "Section warning"


renderSectionHighlighted :: forall a. HTML a -> HTML a
renderSectionHighlighted = div ! className "Section highlighted"


sectionStatus :: forall a. HTML a -> HTML a -> HTML a
sectionStatus i content =
  div ! className "status" $ do
    div ! className "status-icon" $ i
    content


sectionHighlight :: Attribute
sectionHighlight = className "highlighted"


sectionWarning :: Attribute
sectionWarning = className "warning"


renderIcon :: forall a. String -> HTML a
renderIcon name = i ! className ("fa fa-lg " <> name) $ text ""

renderLinkIcon :: forall a. String -> HTML a
renderLinkIcon name = i ! className ("Link-icon fa fa-lg " <> name) $ text ""


renderEthIcon :: forall a. Html a
renderEthIcon =
  img
  ! className "Icon"
  ! src images.ethIcon


renderBlockie :: forall a. String -> HTML a
renderBlockie blockieSrc =
  img ! className "blockie" ! src blockieSrc


renderList :: forall a. Array (HTML a) -> HTML a
renderList elements = do
  let indexed = mapWithIndex Tuple elements
  div ! className "List" $
    for_ indexed \(Tuple i el) ->
      if i `mod` 2 == 1 then
        div ! className "row row-even" $ el
      else
        div ! className "row row-odd" $ el


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


txLink :: forall a. TxHash -> Maybe TxStatus -> HTML a
txLink hash status = do
  div ! dataQA "tx-status" $ do
    text case status of
      Nothing -> "Tx " <> (take 8 $ show hash ) <> "..."
      Just result -> case result of
        TxPending -> "Tx Pending..."
        TxFailed -> "Tx Failed"
        TxOk -> "Tx Successful"
    a
      ! A.href (txURL hash)
      ! A.target "_blank"
      $ renderIcon "Link-icon fa-external-link"


addressURL :: Address -> String
addressURL (Address address) =
  "https://etherscan.io/address/" <> address


txURL :: TxHash -> String
txURL (TxHash hash) =
  "https://etherscan.io/tx/" <> hash


navigate :: Location -> DOMEvent -> Event
navigate location = Routing <<< Locations.Navigate location


ignoreEvent :: DOMEvent -> Event
ignoreEvent = PreventDefault Nothing


preventingDefault :: Event -> DOMEvent -> Event
preventingDefault next = PreventDefault $ Just next


onClickAction :: Event -> EventHandlers (DOMEvent -> Event)
onClickAction next = onClick $ preventingDefault next


-- | Use where result is expected to be always `Just` due to the app logic,
-- | eg. in internal store lookups
expectResult :: forall a. Maybe a -> a
expectResult a = unsafePartial $ fromJust $ a
