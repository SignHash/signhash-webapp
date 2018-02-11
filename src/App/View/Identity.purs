module App.View.Identity (viewIdentity) where

import Prelude

import App.State (State, signerLens)
import App.State.IdentityManagement (getMethodUIState)
import App.State.IdentityManagement as Identity
import App.View.Common (empty, expectResult, guardAccountUnlocked, guardContractsLoaded, proofMethodIcon, renderBlockie, renderEthIcon, renderIcon, renderSection)
import Data.Either (Either(..), fromRight, hush, isRight)
import Data.Lens ((^.))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Lib.Eth.Web3 (Address)
import Lib.SignHash.Proofs.Methods (ProofMethod, allProofMethods)
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
      viewUnlockedIdentity state address


viewUnlockedIdentity :: State -> Address -> HTML Identity.Event
viewUnlockedIdentity state address = do
  let
    { blockie, proofs } = expectResult $ state ^. signerLens address

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
          uiState = Identity.getMethodUIState proofMethod state.identityUI
          loadedProof = Map.lookup proofMethod proofs
        in
          case loadedProof of
            Just proofState -> viewProofMethod proofMethod uiState proofState
            Nothing -> empty


viewProofMethod ::
  ProofMethod
  -> Maybe Identity.ProofManagementState
  -> Proofs.ProofState
  -> HTML Identity.Event
viewProofMethod method uiState value = do
  renderSection do
    H.h4 ! A.className "title" $ do
      proofMethodIcon method
      text $ show method
    H.div do
      case value of
        Proofs.Pending -> text $ "Loading..."
        Proofs.NetworkError -> text $ "Network error"
        (Proofs.Finished result) -> renderProofManagement method result uiState


renderProofManagement ::
  ProofMethod
  -> Proofs.ProofVerification
  -> Maybe Identity.ProofManagementState
  -> HTML Identity.Event
renderProofManagement method proofVerification uiState = do
  let
    storedValue = getStoredValue proofVerification

  renderProofValue method storedValue uiState

  case uiState of
    Nothing -> do
      case storedValue of
        Just value -> editButton value method
        Nothing -> addButton method

    Just (Identity.Editing inputValue) -> do
      let
        validatedValue = ProofValue.createProofValue inputValue
        isValid = isRight validatedValue

      H.button
        #! onClick (const $ Identity.Cancel method)
        $ renderIcon "fa-times"

      case hush validatedValue of
        Just proofValue ->
          H.button
            #! onClick (const $ Identity.Update method proofValue)
            $ renderIcon "fa-check"
        Nothing ->
          H.button
            ! A.disabled "disabled"
            $ renderIcon "fa-check"

      H.div do
        text if isValid
          then "Value correct"
          else "Value incorrect"

    Just Identity.Updating ->
      H.div do
        text "Updating..."


addButton :: ProofMethod -> HTML Identity.Event
addButton method =
  H.button
    #! onClick (const $ Identity.Edit method "")
    $ renderIcon "fa-plus"


editButton :: String -> ProofMethod -> HTML Identity.Event
editButton value method =
  H.button
    #! onClick (const $ Identity.Edit method value)
    $ renderIcon "fa-pencil-square-o"


identityInput :: ProofMethod -> String -> Boolean -> HTML Identity.Event
identityInput method value enabled = do
  H.input
    ! A.value value
    ! A.disabled (if enabled then "" else "disabled")
    #! onInput (\ev -> Identity.Edit method (targetValue ev))


getStoredValue :: Proofs.ProofVerification -> Maybe String
getStoredValue = case _ of
  Proofs.Unavailable -> Nothing
  Proofs.Verified value -> Just $ ProofValue.extract value
  Proofs.Unverified error ->
    case error of
      Proofs.InvalidProofContent proofValue _ ->
        Just $ ProofValue.extract proofValue
      Proofs.InvalidProofValue value error ->
        Just value


renderProofValue ::
  ProofMethod
  -> Maybe String
  -> Maybe Identity.ProofManagementState
  -> HTML Identity.Event
renderProofValue method storedValue uiState =
  case uiState of
    Just (Identity.Editing inputValue) -> do
      identityInput method inputValue true
    _ -> do
      case storedValue of
        Just value -> do
          identityInput method value false
        Nothing -> do
          text "No proof defined"
