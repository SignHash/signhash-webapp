module Lib.SignHash.Proofs.Parsing where

import Prelude

import Data.Array (elemIndex)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split, stripPrefix, trim)
import Data.Traversable (traverse)
import Lib.SignHash.Proofs.Types (ParsingError(..), ProofVerification(..), VerificationError(..))


validPreambule :: String
validPreambule =
  "I hearby declare that I am affiliated with the following list of addresses for the purposes of SignHash identification:\n"


addressLength :: Int
addressLength = (length "0x") + 40


parseAddressList :: String -> Either ParsingError (Array String)
parseAddressList rawText =
  case trim rawText of
    "" -> Right []
    trimmed ->
       let addresses = split (Pattern "\n") trimmed
       in traverse toAddress addresses
  where
    toAddress a =
      if length a == addressLength then
        Right a
      else
        Left $ InvalidAddress rawText


parseProof :: String -> Either ParsingError (Array String)
parseProof proof =
  parsePreambule proof
  >>= parseAddressList
  where
    parsePreambule proof =
      case stripPrefix (Pattern validPreambule) proof of
        Nothing -> Left $ InvalidPreambule proof
        Just suffix -> Right $ suffix


validateProof :: String -> String -> ProofVerification
validateProof address =
  either (Unverified <<< ParsingFailed) verifyAddress <<< parseProof
  where
    verifyAddress addresses =
      case elemIndex address addresses of
        Nothing -> Unverified $ UnconfirmedAddress address addresses
        Just i -> Unverified $ UnconfirmedAddress address []
