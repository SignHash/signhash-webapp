module Tests.Unit.ProofValuesSpec where

import Prelude

import Data.Array (range)
import Data.Either (Either(..))
import Data.Traversable (foldl, for_)
import Lib.SignHash.Proofs.Values (ProofValueError(..), createProofValue)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)



spec :: forall r. Spec r Unit
spec =
  let
    validUsernames =
      [ "biern"
      , "jstefanski"
      , "foobar32167"
      , "foo-13"
      , "aAa"
      , "A-A-A"
      ]
    invalidUsernames =
      [ "foo/.."
      , "foo/../bar"
      , "fo_oo"
      , "-foo"
      , "foo-"
      , foldl (\b a -> b <> "x") "" (0 `range` 39)
      ]
  in
  describe "Proof value validation" do
    describe "GitHub" do
      for_ ["/", ".", "\\"] \char -> do
        it ("can't contain '" <> char <> "'") do
          (createProofValue "foobar/")
            `shouldEqual`
            (Left InvalidValue)
      for_ validUsernames \username -> do
        it ("Validates '" <> username <> "' username") do
          createProofValue username `shouldEqual` Right username
      for_ invalidUsernames \username -> do
        it ("Invalidates '" <> username <> "' username") do
          createProofValue username `shouldEqual` Left InvalidValue
