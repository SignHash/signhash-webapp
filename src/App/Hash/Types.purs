module App.Hash.Types where


import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

type Address = String

data HashSigner = HashSigner Address | NoSigner

derive instance eqHashSigner :: Eq HashSigner
derive instance genericHashSigner :: Generic HashSigner _

instance showHashSigner :: Show HashSigner where
  show = genericShow


data ProofMethod = HTTP | GitHub

derive instance eqProofMethod :: Eq ProofMethod
derive instance ordProofMethod :: Ord ProofMethod
derive instance genericProofMethod :: Generic ProofMethod _

instance showProofMethod :: Show ProofMethod where
  show = genericShow


data ProofVerification =
  Verified String |
  Unverified String |
  Unavailable


allProofMethods :: Array ProofMethod
allProofMethods = [HTTP, GitHub]
