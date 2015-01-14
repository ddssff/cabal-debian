#!/usr/bin/runhaskell

import Control.Exception (try)
import Control.Monad (when)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import Distribution.Simple.Program
import GHC.IO.Exception (IOException(ioe_type), IOErrorType(InvalidArgument, NoSuchThing))
import System.Posix.Files (readSymbolicLink, createSymbolicLink)
import System.Process
import System.Directory
import System.Exit

main =
    ensureSymbolicLink "../debian-haskell" "debian-haskell" >>
    defaultMainWithHooks simpleUserHooks
       { preSDist = \ a b -> copyFile "debian/changelog" "changelog" >> preSDist simpleUserHooks a b }

runTestScript lbi =
    system (buildDir lbi ++ "/cabal-debian-tests/cabal-debian-tests") >>= \ code ->
    if code == ExitSuccess then return () else error "unit test failure"

-- Do all we can to create or update a symbolic link - remove any
-- existing file or directory and verify the link contents.
ensureSymbolicLink :: FilePath -> FilePath -> IO ()
ensureSymbolicLink destination location = do
  result <- try (readSymbolicLink location)
  case result of
    Right destination' | destination' == destination -> return ()
    Right _ -> removeAndRepeat
    Left e -> case ioe_type e of
                InvalidArgument -> removeAndRepeat
                NoSuchThing -> createSymbolicLink destination location
                _ -> error $ "ensureSymbolicLink " ++ show destination ++ " " ++ show location ++ " -> " ++ show e
    where
      removeAndRepeat :: IO ()
      removeAndRepeat =
          do result <- try remove
             case (result :: Either IOError ()) of
               Left e -> error $ "Unable to remove " ++ show location ++ ": " ++ show e
               Right () -> ensureSymbolicLink destination location
      remove =
          do fileExists <- doesFileExist location
             case fileExists of
               True -> removeFile location
               False -> do
                 dirExists <- doesDirectoryExist location
                 case dirExists of
                   True -> removeDirectoryRecursive location
                   False -> return ()
