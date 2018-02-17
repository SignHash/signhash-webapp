module App.View.Identity (viewIdentity) where

import Prelude

import App.State (State, signerLens)
import App.State.Contracts (TxStatusGetter, viewTxResult)
import App.State.IdentityManagement (ProofMethodChange)
import App.State.IdentityManagement as Identity
import App.State.Signers as Signers
import App.View.Common (dataQA, empty, expectResult, guardAccountUnlocked, guardContractsLoaded, proofMethodIcon, renderBlockie, renderEthIcon, renderIcon, renderSection, txLink)
import Data.Either (Either(..), fromRight, hush, isRight)
import Data.Lens ((^.))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Lib.Eth.Web3 (Address, TxHash(..), TxStatus, getTxResult)
import Lib.SignHash.Proofs.Methods (ProofMethod, allProofMethods, canonicalName)
import Lib.SignHash.Proofs.Types as Proofs
import Lib.SignHash.Proofs.Values as ProofValue
import Pux.DOM.Events (onClick, onInput, targetValue)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (Markup, text, (!), (#!))


viewIdentity :: State -> HTML Identity.Event
viewIdentity state = do
  guardContractsLoaded state.contracts \c -> do
    guardAccountUnlocked state.myAccount \address -> do
      let
        signerState = expectResult $ state ^. signerLens address
        getTxStatus = (viewTxResult state.contracts)

      H.div do
        case state.identityUI of
          Nothing ->
            viewUnlockedIdentity signerState address
          Just managementState ->
            viewMethodManagement managementState getTxStatus


viewUnlockedIdentity ::
  Signers.State
  -> Address
  -> HTML Identity.Event
viewUnlockedIdentity { blockie, proofs } address = do
  H.div ! A.className "Identity" $ do
    renderSection do
      H.h4 ! A.className "title" $ do
        renderEthIcon
        text "Ethereum"
      H.div $ do
        renderBlockie blockie
        H.div $ text $ show address

      for_ allProofMethods \proofMethod ->
        let
          loadedProof = Map.lookup proofMethod proofs
        in
          case loadedProof of
            Just proof -> viewProofMethod proofMethod proof
            Nothing -> empty


viewProofMethod ::
  ProofMethod
  -> Proofs.ProofState
  -> HTML Identity.Event
viewProofMethod method value = do
  renderSection do
    H.h4 ! A.className "title" $ do
      proofMethodIcon method
      text $ show method
    H.div do
      case value of
        Proofs.Pending -> text $ "Loading..."
        Proofs.NetworkError -> text $ "Network error"
        (Proofs.Finished verification) ->
          renderFinishedProofRow method verification


renderFinishedProofRow ::
  ProofMethod
  -> Proofs.ProofVerification
  -> HTML Identity.Event
renderFinishedProofRow method verification = do
  let
    storedValue = getStoredValue verification

  renderVerificationIcon verification
  H.div ! dataQA ("identity-" <> canonicalName method <> "-content") $
    case storedValue of
      Just value -> do
        text value
        editButton value method
      Nothing -> do
        text "No proof defined"
        addButton method


viewMethodManagement ::
  ProofMethodChange
  -> TxStatusGetter
  -> HTML Identity.Event
viewMethodManagement (Tuple method managementState) getTxStatus = do
  H.div ! A.className "Identity-management" $ do

    H.h4 ! A.className "title" $ do
      proofMethodIcon method
      text $ show method

    case managementState of
      Identity.Editing inputValue -> do
        let
          validatedValue = ProofValue.createProofValue inputValue
          isValid = isRight validatedValue

        identityInput method inputValue true

        H.button
          #! onClick (const $ Identity.Cancel method)
          $ renderIcon "fa-times"

        updateButton method $ hush validatedValue

        H.div do
          text if isValid
            then "Value correct"
            else "Value incorrect"

      Identity.Updating updateValue txHash -> do
        let txStatus = getTxStatus txHash
        H.div do
          case updateValue of
            Just nextValue ->
              text $ "Updating to " <> (ProofValue.extract nextValue)
            Nothing ->
              text $ "Removing proof..."

          H.br
          txLink txHash txStatus

      Identity.UpdateFailed updateValue txHash -> do
        let txStatus = getTxStatus txHash
        text "Update failed. Now that's unexpected."
        H.br
        txLink txHash txStatus


addButton :: ProofMethod -> HTML Identity.Event
addButton method =
  H.button
    ! dataQA ("identity-" <> canonicalName method <> "-add")
    #! onClick (const $ Identity.Edit method "")
    $ renderIcon "fa-plus"


editButton :: String -> ProofMethod -> HTML Identity.Event
editButton value method =
  H.button
    ! dataQA ("identity-" <> canonicalName method <> "-edit")
    #! onClick (const $ Identity.Edit method value)
    $ renderIcon "fa-pencil-square-o"


updateButton ::
  ProofMethod -> Maybe ProofValue.ProofValue -> HTML Identity.Event
updateButton method validatedValue = case validatedValue of
  Nothing ->
    H.button
      ! A.disabled "disabled"
      $ renderIcon "fa-check"
  justValue ->
    H.button
      ! dataQA ("identity-" <> canonicalName method <> "-update")
      #! onClick (const $ Identity.RequestUpdate method justValue)
      $ renderIcon "fa-check"


identityInput :: ProofMethod -> String -> Boolean -> HTML Identity.Event
identityInput method value enabled = do
  H.input
    ! dataQA ("identity-" <> canonicalName method <> "-input")
    ! A.value value
    ! A.disabled (if enabled then "" else "disabled")
    #! onInput (Identity.Edit method <<< targetValue)


getStoredValue :: Proofs.ProofVerification -> Maybe String
getStoredValue = case _ of
  Proofs.Unavailable -> Nothing
  Proofs.Verified value -> Just $ ProofValue.extract value
  Proofs.Unverified error ->
    case error of
      Proofs.InvalidProofContent proofValue _ ->
        Just $ ProofValue.extract proofValue
      Proofs.InvalidProofValue value valueError ->
        Just value


renderVerificationIcon :: forall a. Proofs.ProofVerification -> HTML a
renderVerificationIcon = case _ of
  Proofs.Unavailable -> empty
  Proofs.Verified _ -> renderIcon "fa-check"
  Proofs.Unverified _ -> renderIcon "fa-exclamation-circle"


renderProofValue ::
  ProofMethod
  -> Maybe String
  -> Maybe Identity.ProofManagementState
  -> HTML Identity.Event
renderProofValue method storedValue uiState =
  H.div ! dataQA ("identity-" <> canonicalName method <> "-content") $
  case uiState of
    Just (Identity.Editing inputValue) -> do
      identityInput method inputValue true
    _ -> do
      case storedValue of
        Just value -> do
          identityInput method value false
        Nothing -> do
          text "No proof defined"
