module App.Events.Files where

import Prelude

import App.Hash.Files (calculateFileHash)
import App.Hash.Signers (fetchHashSigners)
import App.Hash.Types (HashSigner(..))
import App.Hash.Worker (WORKER)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import Data.Lens (Traversal', (.~))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Seconds)
import Lib.Files (FileMeta)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)


type State = {
  meta :: FileMeta,
  result :: Maybe FileHashResult,
  signer :: Maybe HashSigner
}


type FileHashResult = {
  hash :: String,
  elapsed :: Seconds
}


fileSigner :: Traversal' State (Maybe HashSigner)
fileSigner = prop (SProxy :: SProxy "signer")

fileResult :: Traversal' State (Maybe FileHashResult)
fileResult = prop (SProxy :: SProxy "result")


init :: FileMeta -> State
init file =
  { meta: file
  , result: Nothing
  , signer: Nothing
  }


data Event =
  CalculateHash |
  HashCalculated {
    hash :: String,
    elapsed :: Seconds
    } |
  SignerFetched HashSigner


type FileEffects eff =
  ( console :: CONSOLE
  , dom :: DOM
  , now :: NOW
  , worker :: WORKER
  , ajax :: AJAX
    | eff
  )


foldp ::
  forall eff.
  Event -> State -> EffModel State Event (FileEffects eff)

foldp CalculateHash state =
  onlyEffects state $ [
    do
      result <- calculateFileHash state.meta
      pure $ Just $ HashCalculated result
  ]

foldp (HashCalculated event) state =
  { state: (fileResult .~ Just event) state
  , effects: [
    do
      signer <- fetchHashSigners event.hash
      pure $ Just $ SignerFetched signer
    ]
  }

foldp (SignerFetched NoSigner) state =
  noEffects $ fileSigner .~ Just NoSigner $ state

foldp (SignerFetched (HashSigner address)) state =
  noEffects $ setFileSigner state
  where
    setFileSigner = (fileSigner .~ (Just $ HashSigner address))
