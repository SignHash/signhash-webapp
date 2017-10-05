module App.Hash.Types where


import Prelude


type Address = String

data HashSigner = HashSigner Address | NoSigner

data ProofMethod = HTTP | GitHub

derive instance eqProofMethod :: Eq ProofMethod
derive instance ordProofMethod :: Ord ProofMethod


data ProofVerification =
  Verified String |
  Unverified String


allProofMethods :: Array ProofMethod
allProofMethods = [HTTP, GitHub]
