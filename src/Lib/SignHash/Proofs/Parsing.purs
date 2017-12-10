module Lib.SignHash.Proofs.Parsing where

import Prelude

import Data.Array (elemIndex)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split, stripPrefix, trim)
import Data.Traversable (traverse)
import Lib.SignHash.Proofs.Types (ParsingError(..), VerificationError(..))
import Lib.SignHash.Types (Address(..))


validPreambule :: String
validPreambule =
  "I hereby declare that I am affiliated with the following list of addresses for the purposes of SignHash identification:\n"


addressLength :: Int
addressLength = (length "0x") + 40


parseAddressList :: String -> Either ParsingError (Array Address)
parseAddressList rawText =
  case trim rawText of
    "" -> Right []
    trimmed ->
       let addresses = split (Pattern "\n") trimmed
       in traverse toAddress addresses
  where
    toAddress a =
      if length a == addressLength then
        Right $ Address a
      else
        Left $ InvalidAddress rawText


parseProof :: String -> Either ParsingError (Array Address)
parseProof = parsePreambule >=> parseAddressList
  where
    parsePreambule proof =
      case stripPrefix (Pattern validPreambule) proof of
        Nothing -> Left $ InvalidPreambule proof
        Just suffix -> Right $ suffix


validateProofContent :: Address -> String -> Either VerificationError Unit
validateProofContent address =
  either (Left <<< ParsingFailed) verifyAddress <<< parseProof
  where
    verifyAddress addresses =
      case elemIndex address addresses of
        Nothing -> Left $ UnconfirmedAddress address addresses
        Just i -> Right unit
