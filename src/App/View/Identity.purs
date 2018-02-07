module App.View.Identity where

import Prelude

import App.State (Event, State, signerLens)
import App.View.Common (expectResult, guardAccountUnlocked, guardContractsLoaded, proofMethodIcon, renderBlockie, renderEthIcon, renderIcon, renderSection, sectionHeader, sectionStatus)
import Data.Lens ((^.))
import Data.Traversable (for_)
import Lib.Eth.Web3 (Address)
import Lib.SignHash.Proofs.Methods (allProofMethods)
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
  let { blockie } = expectResult $ state ^. signerLens address

  H.div ! A.className "Identity" $ do
    renderSection do
      H.h4 ! A.className "title" $ do
        renderEthIcon
        text "Ethereum"
      H.div $ do
        renderBlockie blockie
        H.div $ text $ show address

      for_ allProofMethods \proof -> do
        renderSection do
          H.h4 ! A.className "title" $ do
            proofMethodIcon proof
            text $ show proof
          H.div ! A.className "columns eight" $ text $ show address
