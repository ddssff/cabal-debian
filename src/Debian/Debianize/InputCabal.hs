-- | Input the Cabal package description.
{-# LANGUAGE CPP, DeriveDataTypeable, ScopedTypeVariables, TemplateHaskell #-}
module Debian.Debianize.InputCabal
    ( inputCabalization
    ) where

import Control.Exception (bracket)
import Control.Lens (view)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Set as Set (toList)
import Debian.Debianize.BasicInfo (Flags, verbosity, compilerFlavor, cabalFlagAssignments)
import Debian.Debianize.Prelude (intToVerbosity')
#if MIN_VERSION_Cabal(1,22,0)
import Debian.GHC (getCompilerInfo)
#else
import Debian.GHC (newestAvailableCompilerId)
#endif
import Debian.Orphans ()
#if MIN_VERSION_Cabal(1,22,0)
import Distribution.Compiler (CompilerInfo)
#else
import Distribution.Compiler (CompilerId)
#endif
import Distribution.Package (Package(packageId))
import Distribution.PackageDescription as Cabal (PackageDescription)
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec(ComponentRequestedSpec))
#else
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.PackageDescription.Parse (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parse (readPackageDescription)
#endif
#endif
import Distribution.Simple.Utils (defaultPackageDesc, die', setupMessage)
import Distribution.System as Cabal (buildArch, Platform(..))
import qualified Distribution.System as Cabal (buildOS)
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.Types.GenericPackageDescription (mkFlagAssignment)
#endif
import Distribution.Verbosity (Verbosity)
import Prelude hiding (break, lines, log, null, readFile, sum)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.Posix.Files (setFileCreationMask)
import System.Process (system)

#if !MIN_VERSION_Cabal(1,22,0)
type CompilerInfo = CompilerId
#endif

-- | Load a PackageDescription using the information in the Flags record -
-- in particular, using the dependency environment in the EnvSet, find
-- the newest available compiler of the requested compiler flavor and
-- use that information load the configured PackageDescription.
inputCabalization :: forall m. (MonadIO m) => Flags -> m (Either String PackageDescription)
inputCabalization flags =
    getCompInfo flags >>= either (return . Left) (\cinfo -> Right <$> doCompInfo cinfo)
    where
      doCompInfo :: CompilerInfo -> m PackageDescription
      doCompInfo cinfo = do
        -- Load a GenericPackageDescription from the current directory
        -- and from that create a finalized PackageDescription for the
        -- given CompilerId.
#if MIN_VERSION_Cabal(2,0,0)
        genPkgDesc <- liftIO $ defaultPackageDesc vb >>= readGenericPackageDescription vb
#else
        genPkgDesc <- liftIO $ defaultPackageDesc vb >>= readPackageDescription vb
#endif
#if MIN_VERSION_Cabal(2,2,0)
        let finalized = finalizePD (mkFlagAssignment (toList fs)) (ComponentRequestedSpec True False) (const True) (Platform buildArch Cabal.buildOS) cinfo [] genPkgDesc
#else
        let finalized = finalizePackageDescription (toList fs) (const True) (Platform buildArch Cabal.buildOS) cinfo [] genPkgDesc
#endif
        ePkgDesc <- either (return . Left)
                           (\ (pkgDesc, _) -> do liftIO $ bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> autoreconf vb pkgDesc
                                                 return (Right pkgDesc))
                           finalized
        either (\ deps -> liftIO getCurrentDirectory >>= \ here ->
                          error $ "Missing dependencies in cabal package at " ++ here ++ ": " ++ show deps)
               return
               ePkgDesc
      vb = intToVerbosity' $ view verbosity flags
      fs = view cabalFlagAssignments flags

getCompInfo :: MonadIO m => Flags -> m (Either String CompilerInfo)
getCompInfo flags =
#if MIN_VERSION_Cabal(1,22,0)
              getCompilerInfo (view compilerFlavor flags)
#else
              return $ newestAvailableCompilerId (view compilerFlavor flags)
#endif

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
              ExitFailure n -> die' verbose ("autoreconf failed with status " ++ show n)
