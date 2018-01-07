module Lib.SignHash.Types (
  module Lib.SignHash.Types,
  module Lib.Eth.Web3
) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Lib.Eth.Web3 (Address(..))


type Checksum = String


data HashSigner = HashSigner Address | NoSigner

derive instance eqHashSigner :: Eq HashSigner
derive instance genericHashSigner :: Generic HashSigner _

instance showHashSigner :: Show HashSigner where
  show = genericShow
