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
import Signal as SG
import Signal.Channel (CHANNEL, Channel, channel, send, subscribe)


data Event ev
  = Load String
  | EthLoaded Web3 Contracts
  | EthError ContractLoadingError
  | AccountChanged ProviderDetails
  | PoolTx TxHash (TxResultHandler ev)
  | PoolTxTry TxHash (TxResultHandler ev)
  | PoolTxResult TxHash TxStatus (TxResultHandler ev)
  | Signal (Signal ev)
  | Request (Request ev)


data Signal ev
  = OnAccountChanged (ETHAccountState Address)


data Request ev
  = HandleTxResult TxHash TxStatus (TxResultHandler ev)


data State =
  Loading |
  Error ContractLoadingError |
  Loaded LoadedState


type Env =
  { ethAccountChannel :: ETHAccountChannel }


type TxResultHandler ev =  TxStatus -> Array ev


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
  = LoadingAccount
  | Unavailable
  | Locked
  | Available a


instance functorETHAccountState :: Functor ETHAccountState where
  map _ Unavailable = Unavailable
  map _ Locked = Locked
  map _ LoadingAccount = LoadingAccount
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


type TxStatusGetter = TxHash -> Maybe TxStatus


accUpdateInterval :: Int
accUpdateInterval = 1000


txPoolInterval :: Milliseconds
txPoolInterval = Milliseconds 2000.0


foldp ::
  forall eff ev
  . Env
  -> Event ev
  -> State
  -> EffModel State (Event ev) (Effects eff)
foldp env (Load defaultNetwork) state =
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
          pure $ Just $ EthLoaded web3 contracts
        Left err -> do
          pure $ Just $ EthError err
  ]
foldp env (EthLoaded web3 contracts) state =
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
               liftEff $ send env.ethAccountChannel
                 $ ProviderDetails { address: Nothing, externalProvider }
             Right address ->
               liftEff $ send env.ethAccountChannel
                 $ ProviderDetails { address, externalProvider }
         pure Nothing
    ]
  }
foldp env (EthError err) state = noEffects $ Error $ err
foldp
  env
  (AccountChanged (ProviderDetails { address, externalProvider }))
  (Loaded state) =
    onlyEffects (Loaded state) $
    [ pure $ Just $ Signal $ OnAccountChanged
      $ case address of
        Nothing ->
          if externalProvider then
            Locked
          else
            Unavailable
        Just loaded -> Available loaded
    ]
foldp env (PoolTx hash next) state =
  { state: setTxResult hash TxPending state
  , effects: [ pure $ Just $ PoolTxTry hash next ]
  }
foldp env (PoolTxTry hash next) (Loaded state) =
  onlyEffects (Loaded state) $
  [ do
       delay txPoolInterval
       fetchingResult <- getTxResult state.web3 hash
       pure $ Just $ case fetchingResult of
         Nothing -> PoolTxTry hash next
         Just status ->
           let
             txResult = if status then TxOk else TxFailed
           in
             PoolTxResult hash txResult next
  ]
foldp env (PoolTxResult hash status next) state =
  { state: setTxResult hash status state
  , effects: [ pure $ Just $ Request $ HandleTxResult hash status next ] }
foldp env (Signal _) state = noEffects state
foldp env (Request _) state = noEffects state
foldp env _ Loading = noEffects Loading
foldp env _ state@(Error _) = noEffects state


buildEnv :: forall eff. Eff (channel :: CHANNEL | eff) Env
buildEnv = do
  ethAccountChannel <- channel initialValue
  pure { ethAccountChannel }
  where
    initialValue = ProviderDetails { address: Nothing, externalProvider: false }


getInputs :: forall ev. Env -> Array (SG.Signal (Event ev))
getInputs { ethAccountChannel } =
  [ (ethAccountChannel # subscribe # SG.dropRepeats) SG.~> AccountChanged ]


viewTxResult :: State -> TxStatusGetter
viewTxResult state hash =
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
