module Lib.SignHash.Blockies where


import Data.Function.Uncurried (Fn2, runFn2)
import Lib.SignHash.Types (Address)


type BlockieSpec =
  { size :: Int
  , scale :: Int }


foreign import _buildAddressBlockie :: Fn2 BlockieSpec Address String


buildAddressBlockie :: BlockieSpec -> Address -> String
buildAddressBlockie = runFn2 _buildAddressBlockie


standardAddressBlockie :: Address -> String
standardAddressBlockie = buildAddressBlockie { size: 8, scale: 4 }
