module Lib.SignHash.Proofs.Values (
  createProofValue,
  extract,
  ProofValueError(..),
  ProofValue(..)
) where


import Prelude

import Data.Either (Either(..), fromRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String.Regex (Regex, parseFlags, regex, test)
import Partial.Unsafe (unsafePartial)


newtype ProofValue = UnsafeProofValue String


derive instance eqProofValue :: Eq ProofValue
derive instance genericProofValue :: Generic ProofValue _


instance showProofValue :: Show ProofValue where
  show = genericShow


data ProofValueError = InvalidValue


derive instance eqProofValueError :: Eq ProofValueError
derive instance genericProofValueError :: Generic ProofValueError _


instance showProofValueError :: Show ProofValueError where
  show = genericShow


githubUsernameRegex :: Regex
githubUsernameRegex =
  unsafePartial $ fromRight $
  regex "^[a-z\\d-]{1,255}$" $ parseFlags "i"


createProofValue :: String -> Either ProofValueError ProofValue
createProofValue value
  | test githubUsernameRegex value = Right $ UnsafeProofValue value
  | otherwise = Left InvalidValue


extract :: ProofValue -> String
extract (UnsafeProofValue value) = value
