-- | Wrappers around the debianization function to perform various
-- tasks - output, describe, validate a debianization, run an external
-- script to produce a debianization.

{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

module Debian.Debianize.Output
    ( doDebianizeAction
    , runDebianizeScript
    , writeDebianization
    , describeDebianization
    , compareDebianization
    , validateDebianization
    ) where

import Control.Category ((.))
import Control.Exception as E (throw)
import Control.Monad.State (get)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Algorithm.Diff.Context (contextDiff)
import Data.Algorithm.Diff.Pretty (prettyDiff)
import Data.Lens.Lazy (getL)
import Data.Map as Map (elems, toList)
import Data.Maybe (fromMaybe)
import Data.Text as Text (split, Text, unpack)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.Types.Atoms (EnvSet)
import Debian.Debianize.Files (debianizationFileMap)
import Debian.Debianize.Input (inputDebianization)
import Debian.Debianize.Monad (DebT, Atoms, evalDebT)
import Debian.Debianize.Options (putEnvironmentArgs)
import Debian.Debianize.Prelude (indent, replaceFile, zipMaps)
import qualified Debian.Debianize.Types as T
import qualified Debian.Debianize.Types.BinaryDebDescription as B (package)
import qualified Debian.Debianize.Types.SourceDebDescription as S (source)
import Debian.Pretty (Pretty(pretty))
import Prelude hiding (unlines, writeFile, (.))
import System.Directory (createDirectoryIfMissing, doesFileExist, getPermissions, Permissions(executable), setPermissions)
--import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode, showCommandForUser)

-- | Run the script in @debian/Debianize.hs@ with the given command
-- line arguments.  Returns @True@ if the script exists and succeeds.
-- In this case it may be assumed that a debianization was created (or
-- updated) in the debian subdirectory of the current directory.  In
-- this way we can include a script in a package to produce a
-- customized debianization more sophisticated than the one that would
-- be produced by the cabal-debian executable.  An example is included
-- in the debian subdirectory of this library.
runDebianizeScript :: [String] -> IO Bool
runDebianizeScript args =
    -- getEnv "HOME" >>= \ home ->
    doesFileExist "debian/Debianize.hs" >>= \ exists ->
    case exists of
      False -> return False
      True -> do
        let args' = ["debian/Debianize.hs"] ++ args
        putEnvironmentArgs args
        hPutStrLn stderr (showCommandForUser "runhaskell" args')
        result <- readProcessWithExitCode "runhaskell" args' ""
        case result of
          (ExitSuccess, _, _) -> return True
          (code, out, err) -> error ("runDebianizeScript: " ++ showCommandForUser "runhaskell" args' ++ " -> " ++ show code ++
                                     "\n stdout: " ++ show out ++"\n stderr: " ++ show err)

-- | Depending on the options in @atoms@, either validate, describe,
-- or write the generated debianization.
doDebianizeAction :: (MonadIO m, Functor m) => EnvSet -> DebT m ()
doDebianizeAction envset =
    do new <- get
       case () of
         _ | getL T.validate new ->
               do inputDebianization envset
                  old <- get
                  return $ validateDebianization old new
         _ | getL T.dryRun new ->
               do inputDebianization envset
                  old <- get
                  diff <- liftIO $ compareDebianization old new
                  liftIO $ putStr ("Debianization (dry run):\n" ++ diff)
         _ -> writeDebianization

-- | Write the files of the debianization @d@ to ./debian
writeDebianization :: (MonadIO m, Functor m) => DebT m ()
writeDebianization =
    do files <- debianizationFileMap
       liftIO $ mapM_ (uncurry doFile) (Map.toList files)
       liftIO $ getPermissions "debian/rules" >>= setPermissions "debian/rules" . (\ p -> p {executable = True})
    where
      doFile path text =
          do createDirectoryIfMissing True (takeDirectory path)
             replaceFile path (unpack text)

-- | Return a string describing the debianization - a list of file
-- names and their contents in a somewhat human readable format.
describeDebianization :: (MonadIO m, Functor m) => DebT m String
describeDebianization =
    debianizationFileMap >>= return . concatMap (\ (path, text) -> path ++ ": " ++ indent " > " (unpack text)) . Map.toList

-- | Compare the old and new debianizations, returning a string
-- describing the differences.
compareDebianization :: Atoms -> Atoms -> IO String
compareDebianization old new =
    do oldFiles <- evalDebT debianizationFileMap old
       newFiles <- evalDebT debianizationFileMap new
       return $ concat $ Map.elems $ zipMaps doFile oldFiles newFiles
    where
      doFile :: FilePath -> Maybe Text -> Maybe Text -> Maybe String
      doFile path (Just _) Nothing = Just (path ++ ": Deleted\n")
      doFile path Nothing (Just n) = Just (path ++ ": Created\n" ++ indent " | " (unpack n))
      doFile path (Just o) (Just n) =
          if o == n
          then Nothing -- Just (path ++ ": Unchanged\n")
          else Just (show (prettyDiff ("old" </> path) ("new" </> path) (contextDiff 2 (split (== '\n') o) (split (== '\n') n))))
      doFile _path Nothing Nothing = error "Internal error in zipMaps"

-- | Make sure the new debianization matches the existing
-- debianization in several ways - specifically, version number, and
-- the names of the source and binary packages.  Some debian packages
-- come with a skeleton debianization that needs to be filled in, this
-- can be used to make sure the debianization we produce is usable.
validateDebianization :: Atoms -> Atoms -> ()
validateDebianization old new =
    case () of
      _ | oldVersion /= newVersion -> throw (userError ("Version mismatch, expected " ++ show (pretty oldVersion) ++ ", found " ++ show (pretty newVersion)))
        | oldSource /= newSource -> throw (userError ("Source mismatch, expected " ++ show (pretty oldSource) ++ ", found " ++ show (pretty newSource)))
        | oldPackages /= newPackages -> throw (userError ("Package mismatch, expected " ++ show (map pretty oldPackages) ++ ", found " ++ show (map pretty newPackages)))
        | True -> ()
    where
      oldVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (getL T.changelog old))))
      newVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (getL T.changelog new))))
      oldSource = getL (S.source . T.control) old
      newSource = getL (S.source . T.control) new
      oldPackages = map (getL B.package) $ getL T.binaryPackages old
      newPackages = map (getL B.package) $ getL T.binaryPackages new
      unChangeLog :: ChangeLog -> [ChangeLogEntry]
      unChangeLog (ChangeLog x) = x
