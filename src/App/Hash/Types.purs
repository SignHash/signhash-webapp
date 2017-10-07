module App.Hash.Types where


import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

type Address = String

data HashSigner = HashSigner Address | NoSigner

data ProofMethod = HTTP | GitHub

derive instance eqProofMethod :: Eq ProofMethod
derive instance ordProofMethod :: Ord ProofMethod
derive instance genericProofMethod :: Generic ProofMethod _

instance showProofMethod :: Show ProofMethod where
  show = genericShow


data ProofVerification =
  Verified String |
  Unverified String


allProofMethods :: Array ProofMethod
allProofMethods = [HTTP, GitHub]
