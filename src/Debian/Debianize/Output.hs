-- | Wrappers around the debianization function to perform various
-- tasks - output, describe, validate a debianization, run an external
-- script to produce a debianization.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances, RankNTypes #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

module Debian.Debianize.Output
    ( finishDebianization
    , runDebianizeScript
    , writeDebianization
    , describeDebianization
    , compareDebianization
    , validateDebianization
    , performDebianization
    , performDebianizationOfWebsite
    , performDebianizationWith
    ) where

import Control.Exception as E (throw)
import Control.Lens
import Control.Monad.State (get, put, StateT)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)
import Data.Map as Map (elems, toList)
import Data.Maybe (fromMaybe)
import Data.Text as Text (split, Text, unpack)
import Debian.Debianize.CabalInfo (newCabalInfo)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.BasicInfo (dryRun, validate, upgrade, roundtrip)
import Debian.Debianize.CabalInfo (CabalInfo, debInfo)
import qualified Debian.Debianize.DebInfo as D
import Debian.Debianize.Files (debianizationFileMap)
import Debian.Debianize.InputDebian (inputDebianization)
import Debian.Debianize.Goodies (expandWebsite)
import Debian.Debianize.Monad (DebianT, CabalT, evalDebian, evalCabalT)
import Debian.Debianize.Prelude (indent, replaceFile, zipMaps)
import Debian.Debianize.Finalize (debianizeWith)
import Debian.Debianize.Optparse
import Debian.Debianize.BinaryDebDescription as B (canonical, package)
import qualified Debian.Debianize.SourceDebDescription as S
import Debian.Pretty (ppShow, ppPrint)
import Prelude hiding (unlines, writeFile)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, getPermissions, Permissions(executable), setPermissions)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode, showCommandForUser)
import Text.PrettyPrint.HughesPJClass (text)
-- import System.Posix.Env (setEnv)

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
    getCurrentDirectory >>= \here ->
    doesFileExist "debian/Debianize.hs" >>= \ exists ->
    case exists of
      False -> return False
      True -> do
        -- By default runhaskell looks for source in ., we will also look
        -- in src.  Better would be to see where the cabal file looks.
        let args' = ["-i.:src", "debian/Debianize.hs"] ++ args
        hPutStrLn stderr ("running external debianization script in " ++ show here ++ ":\n  " ++ showCommandForUser "runhaskell" args')
        result <- readProcessWithExitCode "runhaskell" args' ""
        case result of
          (ExitSuccess, _, _) -> return True
          (code, out, err) -> error (" external debianization script failed:\n  " ++ showCommandForUser "runhaskell" args' ++ " -> " ++ show code ++
                                     "\n stdout: " ++ show out ++"\n stderr: " ++ show err)

-- | Perform whole debianization. You provide your customization,
-- this function does everything else.
performDebianization :: CabalT IO () -> IO ()
performDebianization = performDebianizationWith (pure ())

performDebianizationOfWebsite :: CabalT IO () -> IO ()
performDebianizationOfWebsite = performDebianizationWith expandWebsite

performDebianizationWith :: CabalT IO () -> CabalT IO () -> IO ()
performDebianizationWith goodies custom =
  parseProgramArguments >>= \CommandLineOptions {..} -> do
    -- _ <- try (readProcessWithExitCode "apt-get" ["install", "-y", "--force-yes", hcDeb (view compilerFlavor _flags)] "")
    newCabalInfo _flags >>= either
                               (error . ("peformDebianization - " ++))
                               (evalCabalT $ do
                                handleBehaviorAdjustment _adjustment
                                debianizeWith goodies custom
                                finishDebianization)

-- hcDeb :: CompilerFlavor -> String
-- hcDeb GHC = "ghc"
-- hcDeb GHCJS = "ghcjs"
-- hcDeb flavor = error $ "hcDeb - unexpected CompilerFlavor: " ++ show flavor

-- | Depending on the options in @atoms@, either validate, describe,
-- or write the generated debianization.
finishDebianization :: forall m. (MonadIO m, Functor m) => StateT CabalInfo m ()
finishDebianization = zoom debInfo $
    do new <- get
       case () of
         _ | view (D.flags . validate) new ->
               do inputDebianization
                  old <- get
                  return $ validateDebianization old new
         _ | view (D.flags . dryRun) new ->
               do inputDebianization
                  old <- get
                  let diff = compareDebianization old new
                  liftIO $ putStrLn ("Debianization (dry run):\n" ++ if null diff then "  No changes\n" else show diff)
         _ | view (D.flags . upgrade) new ->
               do inputDebianization
                  old <- get
                  let merged = mergeDebianization old new
                  put merged
                  writeDebianization
         _ | view (D.flags . roundtrip) new ->
               do inputDebianization
                  writeDebianization
         _ -> writeDebianization


-- | Write the files of the debianization @d@ to ./debian
writeDebianization :: (MonadIO m, Functor m) => DebianT m ()
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
describeDebianization :: (MonadIO m, Functor m) => DebianT m String
describeDebianization =
    debianizationFileMap >>= return . concatMap (\ (path, text) -> path ++ ": " ++ indent " > " (unpack text)) . Map.toList

-- | Do only the usual maintenance changes when upgrading to a new version
-- and avoid changing anything that is usually manually maintained.
mergeDebianization :: D.DebInfo -> D.DebInfo -> D.DebInfo
mergeDebianization old new =
    override (D.control . S.buildDepends) .
    override (D.control . S.buildDependsIndep) .
    override (D.control . S.homepage) .
    override (D.control . S.vcsFields) $
    old
  where
    override :: forall b. Lens' D.DebInfo b -> (D.DebInfo -> D.DebInfo)
    override lens = set lens (new ^. lens)


-- | Compare the old and new debianizations, returning a string
-- describing the differences.
compareDebianization :: D.DebInfo -> D.DebInfo -> [String]
compareDebianization old new =
    let oldFiles = evalDebian debianizationFileMap (canonical old)
        newFiles = evalDebian debianizationFileMap (canonical new) in
    elems $ zipMaps doFile oldFiles newFiles
    where
      doFile :: FilePath -> Maybe Text -> Maybe Text -> Maybe String
      doFile path (Just _) Nothing = Just (path ++ ": Deleted\n")
      doFile path Nothing (Just n) = Just (path ++ ": Created\n" ++ indent " | " (unpack n))
      doFile path (Just o) (Just n) =
          if o == n
          then Nothing -- Just (path ++ ": Unchanged\n")
          else Just (show (prettyContextDiff (text ("old" </> path)) (text ("new" </> path)) (text . unpack) (getContextDiff 2 (split (== '\n') o) (split (== '\n') n))))
      doFile _path Nothing Nothing = error "Internal error in zipMaps"

-- | Make sure the new debianization matches the existing
-- debianization in several ways - specifically, version number, and
-- the names of the source and binary packages.  Some debian packages
-- come with a skeleton debianization that needs to be filled in, this
-- can be used to make sure the debianization we produce is usable.
validateDebianization :: D.DebInfo -> D.DebInfo -> ()
validateDebianization old new =
    case () of
      _ | oldVersion /= newVersion -> throw (userError ("Version mismatch, expected " ++ ppShow oldVersion ++ ", found " ++ ppShow newVersion))
        | oldSource /= newSource -> throw (userError ("Source mismatch, expected " ++ ppShow oldSource ++ ", found " ++ ppShow newSource))
        | oldPackages /= newPackages -> throw (userError ("Package mismatch, expected " ++ show (map ppPrint oldPackages) ++ ", found " ++ show (map ppPrint newPackages)))
        | True -> ()
    where
      oldVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (view D.changelog old))))
      newVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (view D.changelog new))))
      oldSource = view (D.control . S.source) old
      newSource = view (D.control . S.source) new
      oldPackages = map (view B.package) $ view (D.control . S.binaryPackages) old
      newPackages = map (view B.package) $ view (D.control . S.binaryPackages) new
      unChangeLog :: ChangeLog -> [ChangeLogEntry]
      unChangeLog (ChangeLog x) = x
