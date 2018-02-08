module App.View.Identity (viewIdentity) where

import Prelude

import App.State (Event, State, signerLens)
import App.State.IdentityManagement as Identity
import App.View.Common (empty, expectResult, guardAccountUnlocked, guardContractsLoaded, proofMethodIcon, renderBlockie, renderEthIcon, renderIcon, renderSection, sectionHeader, sectionStatus)
import Data.Lens ((^.))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Lib.Eth.Web3 (Address)
import Lib.SignHash.Proofs.Methods (ProofMethod, allProofMethods)
import Lib.SignHash.Proofs.Types as Proofs
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (text, (!))


viewIdentity :: State -> HTML Event
viewIdentity state = do
  guardContractsLoaded state.contracts \c -> do
    guardAccountUnlocked state.myAccount \address -> do
      viewUnlockedIdentity state address


viewUnlockedIdentity :: State -> Address -> HTML Event
viewUnlockedIdentity state address = do
  let { blockie, proofs } = expectResult $ state ^. signerLens address

  H.div ! A.className "Identity" $ do
    renderSection do
      H.h4 ! A.className "title" $ do
        renderEthIcon
        text "Ethereum"
      H.div $ do
        renderBlockie blockie
        H.div $ text $ show address

      for_ allProofMethods \proofMethod ->
        case Map.lookup proofMethod proofs of
          Just proofState -> viewProofMethod proofMethod Nothing proofState
          Nothing -> empty


viewProofMethod ::
  ProofMethod
  -> Maybe Identity.ProofManagementState
  -> Proofs.ProofState
  -> HTML Event
viewProofMethod method uiState value = do
  renderSection do
    H.h4 ! A.className "title" $ do
      proofMethodIcon method
      text $ show method
    H.div do
      case value of
        Proofs.Pending -> text $ "Loading..."
        Proofs.NetworkError -> text $ "Network error"
        (Proofs.Finished result) -> renderProofResult result

renderProofResult :: Proofs.ProofVerification -> HTML Event
renderProofResult (Proofs.Verified value) = text $ "Verified " <> show value
renderProofResult (Proofs.Unverified value) = text $ "Unverified " <> show value
renderProofResult Proofs.Unavailable = text $ "Not defined"
