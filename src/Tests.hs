module Main where

import Debian.Debianize.Tests (tests)
import Test.HUnit (runTestTT)

main :: IO ()
main = runTestTT tests >>= putStrLn . show
