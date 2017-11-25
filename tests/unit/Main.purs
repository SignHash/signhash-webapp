module App.Tests.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Node.FS (FS)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects (fs :: FS)) Unit
main = discover "Tests\\.Unit\\..*Spec" >>= run [consoleReporter]
