module Lib.SignHash.Proofs.Values (
  createProofValue,
  ProofValueError(..),
  ProofValue
) where


import Prelude

import Data.Either (Either(..), fromRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String.Regex (Regex, parseFlags, regex, test)
import Partial.Unsafe (unsafePartial)


type ProofValue = String


data ProofValueError = InvalidValue


derive instance eqProofValueError :: Eq ProofValueError
derive instance genericProofValueError :: Generic ProofValueError _


instance showProofValueError :: Show ProofValueError where
  show = genericShow


githubUsernameRegex :: Regex
githubUsernameRegex =
  unsafePartial $ fromRight $
  regex "^[a-z\\d-]{0,255}$" $ parseFlags "i"


createProofValue :: String -> Either ProofValueError ProofValue
createProofValue value =
  if test githubUsernameRegex value then
    Right value
  else
    Left InvalidValue
