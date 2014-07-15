#!/usr/bin/runhaskell

import Control.Monad (when)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import Distribution.Simple.Program
import System.Process
import System.Directory
import System.Exit

main =
    defaultMainWithHooks simpleUserHooks
       { preSDist = \ a b -> copyFile "debian/changelog" "changelog" >> preSDist simpleUserHooks a b }

runTestScript lbi =
    system (buildDir lbi ++ "/cabal-debian-tests/cabal-debian-tests") >>= \ code ->
    if code == ExitSuccess then return () else error "unit test failure"


