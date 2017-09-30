module Lib.Hash where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)

foreign import data Sha256 :: Type
foreign import data SJCL :: Effect

type HashEff v = forall eff. Eff (sjcl :: SJCL | eff) v

foreign import sha256 :: HashEff Sha256
foreign import update :: Sha256 -> String -> HashEff Unit
foreign import finalize :: Sha256 -> HashEff String
