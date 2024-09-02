module Main (main) where

import Keuringsdienst.HelpersSpec
import KeuringsdienstSpec
import System.Exit qualified as Exit
import Test.HUnit

main :: IO ()
main = do
  result <- runTestTT (TestList [keuringsdienstSpec, keuringsdienstHelpersSpec])
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
