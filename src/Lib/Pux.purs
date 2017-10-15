module Lib.Pux where

import Prelude
import Pux (EffModel)


mergeEffModels ::
  forall st ev fx.
  (st -> EffModel st ev fx) ->
  (st -> EffModel st ev fx) ->
  (st -> EffModel st ev fx)
mergeEffModels a b = merged
  where
    merged state = { state: finalState, effects: finalEffects }
      where
        modelA = a state
        modelB = b modelA.state
        finalState = modelB.state
        finalEffects = modelA.effects <> modelB.effects
