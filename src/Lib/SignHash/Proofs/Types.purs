module Lib.SignHash.Proofs.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Lib.SignHash.Types (Address)


type ProofName = String


data ProofVerification =
  Verified ProofName |
  Unverified ProofName VerificationError |
  Unavailable


derive instance eqProofVerification :: Eq ProofVerification
derive instance genericProofVerification :: Generic ProofVerification _

instance showProofVerification :: Show ProofVerification where
  show = genericShow


data VerificationError =
  ParsingFailed ParsingError |
  UnconfirmedAddress Address (Array Address)


derive instance eqVerificationError :: Eq VerificationError
derive instance genericVerificationError :: Generic VerificationError _

instance showVerificationError :: Show VerificationError where
  show = genericShow


data ParsingError =
  InvalidPreambule String |
  InvalidAddress String


derive instance eqParsingError :: Eq ParsingError
derive instance genericParsingError :: Generic ParsingError _

instance showParsingError :: Show ParsingError where
  show = genericShow
