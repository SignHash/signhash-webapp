module Lib.SignHash.Worker where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import DOM.File.Types (File)
import Prelude (Unit)

foreign import data WORKER :: Effect
foreign import data HashWorker :: Type

type WorkerEff eff v = Eff (worker :: WORKER | eff) v

foreign import hashWorker :: forall eff. WorkerEff eff HashWorker
foreign import _calcHash ::
  forall eff.
  HashWorker ->
  File ->
  (String -> WorkerEff eff Unit) ->
  WorkerEff eff Unit

calcHash ::
  forall eff.
  HashWorker -> File -> Aff (worker :: WORKER | eff) String
calcHash worker file =
  makeAff (\error success -> _calcHash worker file success)
