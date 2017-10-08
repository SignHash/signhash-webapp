module Test.Main where

import Prelude

import App.Hash.Types (HashSigner(..))
import App.State (State, fileResult, fileSigner)
import Control.Monad.Eff (Eff)
import DOM.File.Types (File)
import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Lens (_Just, preview, (.~))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Seconds(..))
import Lib.Files (FileMeta)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)


ref :: File
ref = unsafeFromForeign $ toForeign "mock-file"


main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "App state" do
    let
      fileMeta :: FileMeta
      fileMeta = { name: "Name", size: 100, ref }
      basicFileState :: State
      basicFileState =
        { file: Just { meta: fileMeta, result: Nothing, signer: Nothing }
        , signer: Nothing
        }

    describe "Lenses" do
      describe "fileResult" do
        it "Sets successful result" do
          let
            result = { hash: "some-hash", elapsed: Seconds 13.0 }
            state = (fileResult .~ Just result) basicFileState
            getHash = fileResult <<< _Just <<< prop (SProxy :: SProxy "hash")

          preview getHash state `shouldEqual` Just "some-hash"

      describe "fileSigner" do
        it "Sets successful signer" do
          let
            state = (fileSigner .~ Just NoSigner) basicFileState

          preview (fileSigner <<< _Just) state `shouldEqual` Just NoSigner
