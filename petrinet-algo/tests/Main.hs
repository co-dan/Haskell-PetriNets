module Main where

import Control.Monad (when)
import System.Exit   (exitFailure)
import Test.HUnit    as H

import CtlTests
import LivenessTests

main :: IO ()
main = do
  c <- H.runTestTT $ H.TestList [ctlTests, livenessTests]
  when (H.errors c > 0 || H.failures c > 0) exitFailure
