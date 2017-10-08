module App.Events.Foldp where

import Prelude

import App.Events.Effects (fetchSigners, processNewFile)
import App.Events.Types (Event(..))
import App.Hash.Proofs (fetchProof)
import App.Hash.Types (HashSigner(HashSigner, NoSigner), allProofMethods)
import App.Hash.Worker (WORKER)
import App.State (ProofState(Finished, Pending), State, fileResult, fileSigner, signerProofs, signerProp)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import Data.Lens ((%~), (.~))
import Data.Map (empty, insert, update)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)


type AppEffects = (
    console :: CONSOLE,
    dom :: DOM,
    now :: NOW,
    worker :: WORKER,
    ajax :: AJAX
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
  { state: updateSigner <<< updateFileSigner $ state
  , effects: pure <$> Just <$> (FetchProof address) <$> allProofMethods
  }
  where
    updateSigner = (signerProp .~ Just { address, proofs: empty })
    updateFileSigner = (fileSigner .~ (Just $ HashSigner address))

foldp (FetchProof address method) state =
  { state: signerProofs %~ insertProof $ state
  , effects: [fetchProofEffect]
  }
  where
    insertProof = insert method Pending
    fetchProofEffect = do
      proof <- fetchProof address method
      pure $ Just $ ProofFetched address method proof

foldp (ProofFetched address method proof) state =
  noEffects $ signerProofs %~ updateProof $ state
  where
    updateProof = update (\_ -> Just $ Finished proof) method
