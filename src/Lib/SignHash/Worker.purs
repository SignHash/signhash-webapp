module Lib.SignHash.Worker where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (Eff, kind Effect)
import DOM.File.Types (File)


type Checksum = String

foreign import data WORKER :: Effect
foreign import data HashWorker :: Type

foreign import hashWorker :: forall eff. Eff (worker :: WORKER | eff) HashWorker
foreign import _calcHash ::
  forall eff.
  HashWorker ->
  File ->
  EffFnAff (worker :: WORKER | eff) Checksum

calcHash ::
  forall eff. HashWorker -> File -> Aff (worker :: WORKER | eff) Checksum
calcHash worker file = fromEffFnAff $ _calcHash worker file
