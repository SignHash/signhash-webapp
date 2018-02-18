module App.State where

import Prelude

import App.Env (AppEnvConfig)
import App.Routing (Location(..))
import App.State.Contracts as Contracts
import App.State.FileInputs as FileInputs
import App.State.Files as Files
import App.State.IdentityManagement as IM
import App.State.Locations as Locations
import App.State.Signers as Signers
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.Event.Event as DOMEvent
import DOM.HTML.Types (HISTORY)
import Data.Either (Either(..))
import Data.Lens (Lens', Traversal', _Just, (.~), (^.))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (mempty)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence)
import Lib.Eth.Web3 (Address, TxHash, TxResult, TxStatus(TxOk), WEB3)
import Lib.Pux (mergeEffModels)
import Lib.SignHash.Contracts.SignHash as SignHash
import Lib.SignHash.Contracts.SignProof as SignProof
import Lib.SignHash.Types (Checksum, HashSigner(..))
import Lib.SignHash.Worker (WORKER)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, mapEffects, mapState, noEffects, onlyEffects)


data Event
  = Init InitEnv
  | Routing Locations.Event
  | Contract (Contracts.Event Event)
  | FileInput FileInputs.Event
  | File Files.Event
  | Signer Address Signers.Event
  | FileSignerFetched HashSigner
  | SignFile Checksum
  | SignFileTx TxResult
  | SignFileTxResult TxHash TxStatus
  | IdentityUI IM.Event
  | HandleTxResult
    { onIssued :: TxResult -> Array Event
    , onStatus :: TxHash -> TxStatus -> Array Event }
    TxResult
  | PreventDefault (Maybe Event) DOMEvent.Event


type State =
  { file :: Maybe Files.State
  , myAccount :: Contracts.ETHAccountState Address
  , signers :: Map.Map Address Signers.State
  , signingTx :: Maybe TxResult
  , contracts :: Contracts.State
  , defaults ::
       { network :: String }
  , location :: Locations.State
  , identityUI :: IM.State
  }


type InitEnv =
  { ethAccountChannel :: Contracts.ETHAccountChannel }


type Update = EffModel State Event AppEffects


init :: AppEnvConfig -> State
init { rpcUrl } =
  { file: Nothing
  , myAccount: Contracts.LoadingAccount
  , signers: Map.empty
  , signingTx: Nothing
  , contracts: Contracts.Loading
  , defaults: { network: rpcUrl }
  , location: Verify
  , identityUI: IM.init
  }


type AppEffects =
  ( console :: CONSOLE
  , dom :: DOM
  , history :: HISTORY
  , timer :: TIMER
  , now :: NOW
  , worker :: WORKER
  , ajax :: AJAX
  , random :: RANDOM
  , web3 :: WEB3
  )


foldp :: Event -> State -> Update

foldp (Init env) state =
  onlyEffects state
  [ pure $ Just $ Contract
    $ Contracts.Load state.defaults.network env.ethAccountChannel
  ]
foldp (PreventDefault next domEvent) state =
  onlyEffects state $
  [ do
       liftEff $ DOMEvent.preventDefault domEvent
       pure next
  ]
foldp (HandleTxResult { onIssued, onStatus } txResult) state =
  onlyEffects state $
  (pure <$> Just <$> onIssued txResult)
  <>
  [ pure case txResult of
       Right txHash ->
         Just $ Contract $ Contracts.PoolTx txHash (onStatus txHash)
       Left err -> Nothing
  ]
-- Contracts
foldp (Contract (Contracts.Signal signal)) state =
  case signal of
    Contracts.OnAccountChanged account ->
      let updateMyAccount s = noEffects $ s { myAccount = account }
      in case account of
        Contracts.Available address ->
          mergeEffModels updateMyAccount (loadSignerEffModel address) state
        otherwise -> updateMyAccount state
foldp (Contract (Contracts.Request req)) state =
  case req of
    Contracts.HandleTxResult txHash txStatus next ->
      onlyEffects state $ pure <$> Just <$> next txStatus
foldp (Contract event) state =
  Contracts.foldp event state.contracts
  # mapEffects Contract
  # mapState \s -> state { contracts  = s}
-- FileInput
foldp (FileInput (FileInputs.NewFile file)) state =
  { state: state { file = Just $ Files.init file
                 , signingTx = Nothing }
  , effects: [ pure $ Just $ File $ Files.CalculateHash ]
  }
foldp (FileInput FileInputs.NoFile) state =
  noEffects $ state { file = Nothing }
foldp (FileInput event) state =
  FileInputs.foldp event unit
  # mapEffects FileInput
  # mapState (const state)
-- File
foldp (File (Files.Signal (Files.OnHashCalculated result))) state =
  whenContractsLoaded state \c -> onlyEffects state $ [
    do
      signer <- SignHash.getSigner c.signHash result.hash
      pure $ Just $ FileSignerFetched signer
    ]
foldp (File event) state =
  case state.file of
    Nothing -> noEffects $ state
    Just fileState ->
      Files.foldp event fileState
      # mapEffects File
      # mapState \s -> state { file = Just s }
-- Signer
foldp (Signer address event) state =
  case state ^. lens of
    Nothing -> noEffects $ state
    Just signerState ->
      Signers.foldp event signerState
      # mapEffects (Signer address)
      # mapState \s -> (lens .~ (Just s) $ state)
  where
    lens = signerLens address
foldp (FileSignerFetched signer) state =
  case signer of
    NoSigner -> fileModel state
    HashSigner address ->
      mergeEffModels fileModel (loadSignerEffModel address) state
  where
    fileModel s =
      onlyEffects s $ [ pure $ Just $ File $ Files.SignerFetched signer ]
-- Routing
foldp (Routing event) state =
  Locations.foldp event (state ^. lens)
  # mapEffects Routing
  # mapState \s -> lens .~ s $ state
  where
    lens = prop (SProxy :: SProxy "location")
-- File signing
foldp (SignFile checksum) state =
  whenAccountLoaded state \address c ->
    onlyEffects state $
    [ do
         txResult <- SignHash.sign c.signHash checksum address
         pure $ Just $ HandleTxResult
           { onIssued: sequence [SignFileTx]
           , onStatus: \hash status -> [SignFileTxResult hash status]
           } txResult
    ]
foldp (SignFileTx result) state =
  noEffects $ state { signingTx = Just result }
foldp (SignFileTxResult txHash TxOk)
  state
  @{ file: Just loadedFile@{ signer: Just NoSigner }
   , myAccount: Contracts.Available address
   } =
    noEffects $ (fileSignerLens .~ Just (HashSigner address)) state
foldp (SignFileTxResult _ _) state = noEffects state
-- IdentityUI
foldp (IdentityUI (IM.Request request)) state = case request of
  (IM.Update method updateValue) ->
    whenAccountLoaded state \address c ->
    onlyEffects state $
    [ do
         let
           onIssued (Right txHash) =
             pure $ IdentityUI $
             IM.UpdateTxHash method updateValue txHash
           onIssued (Left err) = mempty
           onStatus hash status =
             [ Signer address $ Signers.UpdateProof method updateValue
             , IdentityUI $
               IM.UpdateTxStatus method updateValue hash status
             ]

         txResult <- SignProof.update c.signProof method updateValue address

         pure $ Just $ HandleTxResult { onIssued, onStatus } txResult
    ]
foldp (IdentityUI event) state =
  IM.foldp event state.identityUI
  # mapEffects IdentityUI
  # mapState \s -> state { identityUI = s }


loadSignerEffModel :: Address -> State -> Update
loadSignerEffModel address state =
  case state ^. lens of
    Just value -> noEffects state
    Nothing ->
      whenContractsLoaded state \c ->
       { state: lens .~ (Just $ Signers.init address) $ state
       , effects:
         [ pure $ Just $ Signer address $ Signers.FetchAll $ c.signProof ]
       }
  where
    lens = signerLens address


whenContractsLoaded ::
  State
  -> (Contracts.LoadedState -> Update)
  -> Update
whenContractsLoaded { contracts: Contracts.Loaded c } fun = fun c
whenContractsLoaded state _ = noEffects state


whenAccountLoaded ::
  State
  -> (Address -> Contracts.LoadedState -> Update)
  -> Update
whenAccountLoaded state fun =
  whenContractsLoaded state \c ->
    case state.myAccount of
      Contracts.Available address -> fun address c
      otherwise -> noEffects state


signerLens :: Address -> Lens' State (Maybe Signers.State)
signerLens address = prop (SProxy :: SProxy "signers") <<< at address


fileSignerLens :: Traversal' State (Maybe HashSigner)
fileSignerLens =
  prop (SProxy :: SProxy  "file")
  <<< _Just
  <<< prop (SProxy :: SProxy  "signer")
