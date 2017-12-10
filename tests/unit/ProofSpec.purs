module Tests.Unit.ProofSpec where

import Prelude

import Data.Either (Either(..))
import Lib.SignHash.Proofs.Parsing (parseAddressList, validPreambule, validateProofContent)
import Lib.SignHash.Proofs.Types (ParsingError(..), VerificationError(..))
import Lib.SignHash.Types (Address(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


parsingFailed :: ParsingError -> Either VerificationError Unit
parsingFailed = Left <<< ParsingFailed


spec :: forall r. Spec r Unit
spec =
  let
    gibberish = "my hovercraft is full of eels"
    validAddress = Address "0x0000000000000000000000000000000000000000"
    otherAddress = Address "0x0000000000000000000000000000000000000001"
    proofWithOtherAddress = proofWithAddress otherAddress
    proofWithValidAddress = proofWithAddress validAddress
    proofWithAddress address = validPreambule <> show address <> "\n"
  in
   describe "Reading proofs" do
     describe "Proofs validation integration" do
       it "is invalid when empty" do
         let proof = ""
         validateProofContent validAddress proof
           `shouldEqual` (parsingFailed $ InvalidPreambule "")

       it "is invalid if preambule is gibberish" do
         let proof = "my hovercraft is full of eels"
         validateProofContent validAddress proof
           `shouldEqual` (parsingFailed $ InvalidPreambule proof)

       it "is unverified if addresses are gibberish" do
         let proof = validPreambule <> gibberish

         validateProofContent validAddress proof
           `shouldEqual` (parsingFailed $ InvalidAddress gibberish)

       it "is invalid if only preambule matches" do
         let proof = validPreambule

         validateProofContent validAddress proof
           `shouldEqual`
           (Left $ UnconfirmedAddress validAddress [])

       it "is unverified if address is not in list" do
         let proof = proofWithOtherAddress

         validateProofContent validAddress proof
           `shouldEqual`
           (Left $ UnconfirmedAddress validAddress [otherAddress])

       it "is verified if address is in the list" do
         let proof = proofWithValidAddress

         validateProofContent validAddress proof
           `shouldEqual` Right unit

     describe "Parsing address list" do
       it "is invalid if encounters gibberish" do
         parseAddressList gibberish `shouldEqual`
           (Left $ InvalidAddress gibberish)
       it "can be empty" do
         parseAddressList "" `shouldEqual` Right []
       it "has a single element" do
         (parseAddressList $ show validAddress <> "\n")
           `shouldEqual` Right [validAddress]
