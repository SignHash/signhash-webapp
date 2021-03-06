module App.State.Contracts where

import Prelude

import Control.Monad.Aff (attempt, delay, launchAff)
import Control.Monad.Aff.Console (CONSOLE, error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Timer (TIMER, setInterval)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', Traversal', _Just, prism', (.~), (^?))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Lib.Eth.Contracts (ContractLoadingError)
import Lib.Eth.Web3 (Address, TxHash, TxStatus(..), WEB3, Web3, getDefaultAccount, getNetworkId, getOrBuildWeb3, getTxResult, isMetaMask)
import Lib.SignHash.Contracts.SignHash as SignHash
import Lib.SignHash.Contracts.SignProof as SignProof
import Pux (EffModel, noEffects, onlyEffects)
import Signal (Signal, dropRepeats, (~>))
import Signal.Channel (CHANNEL, Channel, channel, send, subscribe)


data Event
  = Load String ETHAccountChannel
  | EthLoaded Web3 Contracts ETHAccountChannel
  | EthError ContractLoadingError
  | PoolTx TxHash
  | PoolTxTry TxHash
  | PoolTxResult TxHash TxStatus
  | AccountChanged ProviderDetails
  | OnAccountChanged (ETHAccountState Address)
  | OnTxResult TxHash TxStatus


data State =
  Loading |
  Error ContractLoadingError |
  Loaded LoadedState


type Contracts =
  { signHash :: SignHash.Contract
  , signProof :: SignProof.Contract }


type Transactions = Map.Map TxHash TxStatus


type LoadedState =
  { web3 :: Web3
  , signHash :: SignHash.Contract
  , signProof :: SignProof.Contract
  , transactions :: Transactions }


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


type Effects eff =
  ( web3 :: WEB3
  , console :: CONSOLE
  , timer :: TIMER
  , dom :: DOM | eff)


accUpdateInterval :: Int
accUpdateInterval = 1000


txPoolInterval :: Milliseconds
txPoolInterval = Milliseconds 2000.0


foldp ::
  forall eff.
  Event ->
  State ->
  EffModel State Event (Effects eff)
foldp (Load defaultNetwork channel) state =
  onlyEffects state $ [
    do
      web3 <- liftEff $ getOrBuildWeb3 defaultNetwork
      networkId <- getNetworkId web3
      signHashResult <- SignHash.loadContract web3 networkId
      signProofResult <- SignProof.loadContract web3 networkId
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
    , signProof: contracts.signProof
    , transactions: Map.empty }
  , effects:
    [ do
         liftEff $ void $ setInterval accUpdateInterval $ void $ launchAff do
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
foldp (EthError err) state = noEffects $ Error $ err
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
foldp (PoolTx hash) state =
  { state: setTxResult hash TxPending state
  , effects: [ pure $ Just $ PoolTxTry hash ]
  }
foldp (PoolTxTry hash) (Loaded state) =
  onlyEffects (Loaded state) $
  [ do
       delay txPoolInterval
       result <- getTxResult state.web3 hash
       pure $ Just $ case result of
         Nothing -> PoolTxTry hash
         Just status ->
           let
             result = if status then TxOk else TxFailed
           in
             PoolTxResult hash result
  ]
foldp (PoolTxResult hash status) state =
  { state: setTxResult hash status state
  , effects: [ pure $ Just $ OnTxResult hash status ] }
foldp (OnTxResult hash status) state = noEffects state
foldp (OnAccountChanged _) state = noEffects state
foldp _ Loading = noEffects Loading
foldp _ state@(Error _) = noEffects state


buildAccountsChannel ::
  forall eff. Eff (channel :: CHANNEL | eff) ETHAccountChannel
buildAccountsChannel =
  channel $ ProviderDetails { address: Nothing, externalProvider: false }

buildAccountsSignal :: ETHAccountChannel -> Signal Event
buildAccountsSignal channel =
  (channel # subscribe # dropRepeats) ~> AccountChanged


viewTxResult :: TxHash -> State -> Maybe TxStatus
viewTxResult hash state =
  state ^? (txLens hash <<< _Just)


setTxResult :: TxHash -> TxStatus -> State -> State
setTxResult hash status = (txLens hash .~ Just status)


txLens :: TxHash -> Traversal' State (Maybe TxStatus)
txLens hash =
  loaded <<< transactions <<< at hash
  where
    transactions :: Lens' LoadedState Transactions
    transactions = prop (SProxy :: SProxy "transactions")

    loaded :: Prism' State LoadedState
    loaded = prism' Loaded fromState
      where
        fromState (Error _) = Nothing
        fromState (Loading) = Nothing
        fromState (Loaded s) = Just s
