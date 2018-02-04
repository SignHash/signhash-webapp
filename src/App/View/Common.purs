module App.View.Common where

import Prelude

import App.Routing (Location)
import App.State (Event(..))
import App.State.Locations as Locations
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (take)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Lib.Eth.Web3 (Address(..), TxHash(..), TxStatus(..))
import Partial.Unsafe (unsafePartial)
import Pux.DOM.Events (DOMEvent, onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (Attribute, EventHandlers, attribute, text, (!))


foreign import images ::
  { logo :: String
  , ethIcon :: String }


dataQA :: String -> Attribute
dataQA = attribute "data-qa"


sectionHeader :: forall a. String -> HTML a
sectionHeader msg =
  H.h4 ! A.className "Section-header" $ text msg


renderSection :: forall a. HTML a -> HTML a
renderSection = H.div ! A.className "Section"


renderSectionWarning :: forall a. HTML a -> HTML a
renderSectionWarning = H.div ! A.className "Section warning"


renderSectionHighlighted :: forall a. HTML a -> HTML a
renderSectionHighlighted = H.div ! A.className "Section highlighted"


sectionStatus :: forall a. HTML a -> HTML a -> HTML a
sectionStatus i content =
  H.div ! A.className "status" $ do
    H.div ! A.className "status-icon" $ i
    content


sectionHighlight :: Attribute
sectionHighlight = A.className "highlighted"


sectionWarning :: Attribute
sectionWarning = A.className "warning"


renderIcon :: forall a. String -> HTML a
renderIcon name = H.i ! A.className ("fa fa-lg " <> name) $ text ""

renderLinkIcon :: forall a. String -> HTML a
renderLinkIcon name = H.i ! A.className ("Link-icon fa fa-lg " <> name) $ text ""


renderEthIcon :: forall a. HTML a
renderEthIcon =
  H.img
  ! A.className "Icon"
  ! A.src images.ethIcon


renderBlockie :: forall a. String -> HTML a
renderBlockie blockieSrc =
  H.img ! A.className "blockie" ! A.src blockieSrc


renderList :: forall a. Array (HTML a) -> HTML a
renderList elements = do
  let indexed = mapWithIndex Tuple elements
  H.div ! A.className "List" $
    for_ indexed \(Tuple i el) ->
      if i `mod` 2 == 1 then
        H.div ! A.className "row row-even" $ el
      else
        H.div ! A.className "row row-odd" $ el


loading :: forall a. HTML a
loading = text "Loading..."


empty :: forall a. HTML a
empty = text ""


clear :: forall a. HTML a
clear = H.div ! A.className "Clear" $ empty


addressLink :: forall a. Address -> HTML a
addressLink address = do
  H.a
  ! A.href (addressURL address)
  ! A.className "AddressURL"
  ! A.target "_blank"
  $ text $ show address


txLink :: forall a. TxHash -> Maybe TxStatus -> HTML a
txLink hash status = do
  H.div ! dataQA "tx-status" $ do
    text case status of
      Nothing -> "Tx " <> (take 8 $ show hash ) <> "..."
      Just result -> case result of
        TxPending -> "Tx Pending..."
        TxFailed -> "Tx Failed"
        TxOk -> "Tx Successful"
    H.a
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
