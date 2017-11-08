module App.State.Contracts where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Lib.SignHash.Contracts (SignerContract, signerContract)
import Lib.Web3 (WEB3, Web3, getOrBuildWeb3)
import Pux (EffModel, noEffects, onlyEffects)


data Event =
  Load String |
  EthLoaded Web3 SignerContract |
  EthError Error


data State =
  Loading |
  Error String |
  Loaded
  { web3 :: Web3
  , signerContract :: SignerContract }


type Effects eff =
  ( web3 :: WEB3
  , dom :: DOM | eff)


foldp ::
  forall eff.
  Event ->
  State ->
  EffModel State Event (Effects eff)

foldp (Load defaultNetwork) state =
  onlyEffects state $ [
    do
      web3 <- liftEff $ getOrBuildWeb3 defaultNetwork
      deployed <- signerContract web3
      case deployed of
        Right contract ->
          pure $ Just $ EthLoaded web3 contract
        Left err -> do
          pure $ Just $ EthError err
  ]

foldp (EthLoaded web3 signerContract) state =
  noEffects $ Loaded { web3, signerContract }

foldp (EthError err) state = noEffects $ Error $ show $ err
