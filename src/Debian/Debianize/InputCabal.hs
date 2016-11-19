-- | Input the Cabal package description.
{-# LANGUAGE CPP, DeriveDataTypeable, TemplateHaskell #-}
module Debian.Debianize.InputCabal
    ( inputCabalization
    ) where


import Control.Exception (bracket)
import Control.Lens
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Set as Set (toList)
import Debian.Debianize.BasicInfo (Flags, buildEnv, dependOS, verbosity, compilerChoice, cabalFlagAssignments)
import Debian.Debianize.Prelude (intToVerbosity')
import Debian.GHC (hcFlavor)
#if MIN_VERSION_Cabal(1,22,0)
import Debian.GHC (getCompilerInfo)
#else
import Debian.GHC (newestAvailableCompilerId)
#endif
import Debian.Orphans ()
import Distribution.Package (Package(packageId))
import Distribution.PackageDescription as Cabal (PackageDescription)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Utils (defaultPackageDesc, die, setupMessage)
import Distribution.System as Cabal (buildArch, Platform(..))
import qualified Distribution.System as Cabal (buildOS)
import Distribution.Verbosity (Verbosity)
import Prelude hiding (break, lines, log, null, readFile, sum)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.Posix.Files (setFileCreationMask)
import System.Process (system)
import System.Unix.Mount (WithProcAndSys)

-- | Load a PackageDescription using the information in the Flags record -
-- in particular, using the dependency environment in the EnvSet, find
-- the newest available compiler of the requested compiler flavor and
-- use that information load the configured PackageDescription.
inputCabalization :: MonadIO m => Flags -> WithProcAndSys m (Either String PackageDescription)
inputCabalization flags =
    getCompInfo >>= either (return . Left) (\cinfo -> Right <$> doCompInfo cinfo)
    where
      doCompInfo cinfo = do
        -- Load a GenericPackageDescription from the current directory
        -- and from that create a finalized PackageDescription for the
        -- given CompilerId.
        genPkgDesc <- liftIO $ defaultPackageDesc vb >>= readPackageDescription vb
        let finalized = finalizePackageDescription (toList fs) (const True) (Platform buildArch Cabal.buildOS) cinfo [] genPkgDesc
        ePkgDesc <- either (return . Left)
                           (\ (pkgDesc, _) -> do liftIO $ bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> autoreconf vb pkgDesc
                                                 return (Right pkgDesc))
                           finalized
        either (\ deps -> liftIO getCurrentDirectory >>= \ here ->
                          error $ "Missing dependencies in cabal package at " ++ here ++ ": " ++ show deps)
               return
               ePkgDesc

      getCompInfo =
#if MIN_VERSION_Cabal(1,22,0)
              getCompilerInfo root (view (compilerChoice . hcFlavor) flags)
#else
              return $ newestAvailableCompilerId root (view (compilerChoice . hcFlavor) flags)
#endif
      root = dependOS $ view buildEnv flags
      vb = intToVerbosity' $ view verbosity flags
      fs = view cabalFlagAssignments flags

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
