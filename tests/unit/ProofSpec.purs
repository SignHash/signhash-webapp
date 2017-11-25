module App.Tests.ProofSpec where

import Prelude

import Data.Either (Either(..))
import Lib.SignHash.Proofs.Parsing (parseAddressList, validPreambule, validateProof)
import Lib.SignHash.Proofs.Types (ParsingError(..), ProofVerification(..), VerificationError(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


parsingFailed :: ParsingError -> ProofVerification
parsingFailed = Unverified <<< ParsingFailed


spec :: forall r. Spec r Unit
spec =
  let
    gibberish = "my hovercraft is full of eels"
    validAddress = "0x0000000000000000000000000000000000000000"
    otherAddress = "0x0000000000000000000000000000000000000001"
    proofWithOtherAddress =
      validPreambule <> otherAddress <> "\n"
  in
   describe "Reading proofs" do
     describe "Proofs validation integration" do
       it "is invalid when empty" do
         let proof = ""
         validateProof validAddress proof
           `shouldEqual` (parsingFailed $ InvalidPreambule "")

       it "is invalid if preambule is gibberish" do
         let proof = "my hovercraft is full of eels"
         validateProof validAddress proof
           `shouldEqual` (parsingFailed $ InvalidPreambule proof)

       it "is unverified if addresses are gibberish" do
         let proof = validPreambule <> gibberish

         validateProof validAddress proof
           `shouldEqual` (parsingFailed $ InvalidAddress gibberish)

       it "is invalid if only preambule matches" do
         let proof = validPreambule

         validateProof validAddress proof
           `shouldEqual`
           (Unverified $ UnconfirmedAddress validAddress [])

       it "is unverified if address is not in list" do
         let proof = proofWithOtherAddress

         validateProof validAddress proof
           `shouldEqual`
           (Unverified $ UnconfirmedAddress validAddress [otherAddress])

     describe "Parsing address list" do
       it "is invalid if encounters gibberish" do
         parseAddressList gibberish `shouldEqual`
           (Left $ InvalidAddress gibberish)
       it "can be empty" do
         parseAddressList "" `shouldEqual` Right []
       it "has a single element" do
         (parseAddressList $ validAddress <> "\n")
           `shouldEqual` Right [validAddress]
