module App.State.Contracts where

import Prelude

import Control.Monad.Aff (attempt, launchAff)
import Control.Monad.Aff.Console (CONSOLE, error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Timer (TIMER, setInterval)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Lib.Eth.Web3 (Address, WEB3, Web3, getDefaultAccount, getOrBuildWeb3, isMetaMask)
import Lib.SignHash.Contracts.SignHash as SignHash
import Lib.SignHash.Contracts.SignProof as SignProof
import Pux (EffModel, noEffects, onlyEffects)
import Signal (Signal, dropRepeats, (~>))
import Signal.Channel (CHANNEL, Channel, channel, send, subscribe)


data Event
  = Load String ETHAccountChannel
  | EthLoaded Web3 Contracts ETHAccountChannel
  | EthError Error
  | AccountChanged ProviderDetails
  | OnAccountChanged (ETHAccountState Address)


data ETHAccountState a
  = Unavailable
  | Locked
  | Available a


instance functorETHAccountState :: Functor ETHAccountState where
  map _ Unavailable = Unavailable
  map _ Locked = Locked
  map f (Available a) = Available (f a)


newtype ProviderDetails =
  ProviderDetails { address :: Maybe Address, externalProvider :: Boolean }

derive instance eqProviderDetails :: Eq ProviderDetails


type ETHAccountChannel = Channel ProviderDetails


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
  , console :: CONSOLE
  , timer :: TIMER
  , dom :: DOM | eff)


foldp ::
  forall eff.
  Event ->
  State ->
  EffModel State Event (Effects eff)
foldp (Load defaultNetwork channel) state =
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
          pure $ Just $ EthLoaded web3 contracts channel
        Left err -> do
          pure $ Just $ EthError err
  ]
foldp (EthLoaded web3 contracts channel) state =
  { state: Loaded $
    { web3
    , signHash: contracts.signHash
    , signProof: contracts.signProof }
  , effects:
    [ do
         liftEff $ void $ setInterval 1000 $ void $ launchAff do
           let externalProvider = isMetaMask web3
           result <- attempt $ getDefaultAccount web3
           case result of
             Left err -> do
               error $ show err
               liftEff $ send channel
                 $ ProviderDetails { address: Nothing, externalProvider }
             Right address ->
               liftEff $ send channel
                 $ ProviderDetails { address, externalProvider }
         pure Nothing
    ]
  }
foldp (EthError err) state = noEffects $ Error $ show $ err
foldp
  (AccountChanged (ProviderDetails { address, externalProvider }))
  (Loaded state) =
    onlyEffects (Loaded state) $
    [ pure $ Just $ OnAccountChanged
      $ case address of
        Nothing ->
          if externalProvider then
            Locked
          else
            Unavailable
        Just loaded -> Available loaded
    ]
foldp (AccountChanged _) state = noEffects state
foldp (OnAccountChanged _) state = noEffects state


buildAccountsChannel ::
  forall eff. Eff (channel :: CHANNEL | eff) ETHAccountChannel
buildAccountsChannel =
  channel $ ProviderDetails { address: Nothing, externalProvider: false }

buildAccountsSignal :: ETHAccountChannel -> Signal Event
buildAccountsSignal channel =
  (channel # subscribe # dropRepeats) ~> AccountChanged
