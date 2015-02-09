-- | Input the Cabal package description.
{-# LANGUAGE CPP, DeriveDataTypeable, TemplateHaskell #-}
module Debian.Debianize.InputCabal
    ( inputCabalization
    ) where

import Control.Category ((.))
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Lens.Common (getL)
import Data.Set as Set (Set, toList)
import Debian.Debianize.BasicInfo (Flags, buildEnv, dependOS, verbosity, compilerFlavor, cabalFlagAssignments)
import Debian.Debianize.Prelude (intToVerbosity')
import Debian.GHC (newestAvailableCompilerId)
import Debian.Orphans ()
#if MIN_VERSION_Cabal(1,22,0)
import Distribution.Compiler (AbiTag(NoAbiTag), unknownCompilerInfo)
#endif
import Distribution.Compiler (CompilerId)
import Distribution.Package (Dependency, Package(packageId))
import Distribution.PackageDescription as Cabal (FlagName, PackageDescription)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Utils (defaultPackageDesc, die, setupMessage)
import Distribution.System as Cabal (buildArch, Platform(..))
import qualified Distribution.System as Cabal (buildOS)
import Distribution.Verbosity (Verbosity)
import Prelude hiding ((.), break, lines, log, null, readFile, sum)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.Posix.Files (setFileCreationMask)
import System.Process (system)

-- | Load a PackageDescription using the information in the Flags record -
-- in particular, using the dependency environment in the EnvSet, find
-- the newest available compiler of the requested compiler flavor and
-- use that information load the configured PackageDescription.
inputCabalization :: Flags -> IO PackageDescription
inputCabalization flags =
    do let root = dependOS $ getL buildEnv flags
       let cid = newestAvailableCompilerId root (getL compilerFlavor flags)
       ePkgDesc <- inputCabalization' (intToVerbosity' $ getL verbosity flags) (getL cabalFlagAssignments flags) cid
       either (\ deps -> getCurrentDirectory >>= \ here ->
                         error $ "Missing dependencies in cabal package at " ++ here ++ ": " ++ show deps)
              return
              ePkgDesc

-- | Load a GenericPackageDescription from the current directory and
-- from that create a finalized PackageDescription for the given
-- CompilerId.
inputCabalization' :: Verbosity -> Set (FlagName, Bool) -> CompilerId -> IO (Either [Dependency] PackageDescription)
inputCabalization' vb flags cid = do
  genPkgDesc <- defaultPackageDesc vb >>= readPackageDescription vb
  let cid' =
#if MIN_VERSION_Cabal(1,22,0)
             unknownCompilerInfo cid NoAbiTag
#else
             cid
#endif
  let finalized = finalizePackageDescription (toList flags) (const True) (Platform buildArch Cabal.buildOS) cid' [] genPkgDesc
  either (return . Left)
         (\ (pkgDesc, _) -> do bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> autoreconf vb pkgDesc
                               return (Right pkgDesc))
         finalized

-- | Run the package's configuration script.
autoreconf :: Verbosity -> Cabal.PackageDescription -> IO ()
autoreconf verbose pkgDesc = do
    ac <- doesFileExist "configure.ac"
    when ac $ do
        c <- doesFileExist "configure"
        when (not c) $ do
            setupMessage verbose "Running autoreconf" (packageId pkgDesc)
            ret <- system "autoreconf"
            case ret of
              ExitSuccess -> return ()
              ExitFailure n -> die ("autoreconf failed with status " ++ show n)
