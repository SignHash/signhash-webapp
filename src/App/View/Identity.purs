module App.View.Identity where

import Prelude

import App.State (Event, State)
import App.View.Common (guardAccountUnlocked, guardContractsLoaded)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (text)


viewIdentity :: State -> HTML Event
viewIdentity state = do
  guardContractsLoaded state.contracts \c -> do
    guardAccountUnlocked state.myAccount \address -> do
      text $ "My address is " <> show address
