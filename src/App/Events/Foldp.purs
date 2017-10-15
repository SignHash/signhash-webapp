module App.Events.Foldp where

import Prelude

import App.Events.Effects (fetchSigners, processNewFile)
import App.Events.Signers as Signers
import App.Events.Types (Event(..))
import App.Hash.Types (HashSigner(HashSigner, NoSigner))
import App.Hash.Worker (WORKER)
import App.State (State, fileResult, fileSigner, signerProp)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, mapEffects, mapState, noEffects, onlyEffects)


type AppEffects =
  ( console :: CONSOLE
  , dom :: DOM
  , now :: NOW
  , worker :: WORKER
  , ajax :: AJAX
  , random :: RANDOM
  )


foldp ::
  Event ->
  State ->
  EffModel State Event AppEffects

foldp NoOp state = noEffects $ state

foldp (PreventDefault next event) state =
  { state
  , effects: [
    do
      liftEff $ preventDefault event
      pure $ Just $ next
    ]
  }

foldp NoFile state = noEffects $ state { file = Nothing }

foldp (NewFile file) state =
  { state: state {
       file = Just {
          meta: file
          , result: Nothing
          , signer: Nothing
          }
       , signer = Nothing
       }
  , effects: [ processNewFile file ]
  }

foldp (FileError err) state =
  onlyEffects state $ [ (traverse log err) *> pure Nothing ]

foldp (HashCalculated event) state =
  { state: (fileResult .~ Just event) state
  , effects: [ fetchSigners event.hash ]
  }

foldp (SignerFetched NoSigner) state =
  noEffects $ fileSigner .~ Just NoSigner $ state
foldp (SignerFetched (HashSigner address)) state =
  { state: initSigner <<< updateFileSigner $ state
  , effects: [pure $ Just $ Signer $ Signers.Init address]
  }
  where
    initSigner = signerProp .~ (Just $ Signers.init address)
    updateFileSigner = (fileSigner .~ (Just $ HashSigner address))

foldp (Signer event) state =
  case state.signer of
    Nothing -> noEffects $ state
    Just signerState ->
      Signers.foldp event signerState
      # mapEffects Signer
      # mapState \s -> state { signer = Just s }
