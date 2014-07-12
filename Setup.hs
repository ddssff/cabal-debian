#!/usr/bin/runhaskell

import Control.Monad (when)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import Distribution.Simple.Program
import System.Process
import System.Directory
import System.Exit

main =
    copyFile "debian/changelog" "changelog" >>
    defaultMainWithHooks simpleUserHooks
       {- { postBuild = \ _ _ _ lbi -> when (buildDir lbi /= "dist-ghc/build") (runTestScript lbi)
          , runTests = \ _ _ _ lbi -> runTestScript lbi } -}

runTestScript lbi =
    system (buildDir lbi ++ "/cabal-debian-tests/cabal-debian-tests") >>= \ code ->
    if code == ExitSuccess then return () else error "unit test failure"


