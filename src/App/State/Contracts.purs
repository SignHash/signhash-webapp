module App.State.Contracts where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Lib.Eth.Web3 (WEB3, Web3, getOrBuildWeb3)
import Lib.SignHash.Contracts.SignHash as SignHash
import Lib.SignHash.Contracts.SignProof as SignProof
import Pux (EffModel, noEffects, onlyEffects)


data Event =
  Load String |
  EthLoaded Web3 Contracts |
  EthError Error


type Contracts =
  { signHash :: SignHash.Contract
    , signProof :: SignProof.Contract }


data State =
  Loading |
  Error String |
  Loaded LoadedState


type LoadedState =
  { web3 :: Web3
  , signHash :: SignHash.Contract
  , signProof :: SignProof.Contract }


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
      signHashResult <- SignHash.loadContract web3
      signProofResult <- SignProof.loadContract web3
      let
        loaded =
          { signHash: _, signProof: _ }
          <$> signHashResult
          <*> signProofResult

      case loaded of
        Right contracts ->
          pure $ Just $ EthLoaded web3 contracts
        Left err -> do
          pure $ Just $ EthError err
  ]
foldp (EthLoaded web3 contracts) state =
  noEffects $ Loaded $
    { web3
    , signHash: contracts.signHash
    , signProof: contracts.signProof }
foldp (EthError err) state = noEffects $ Error $ show $ err
