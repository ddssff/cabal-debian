-- | Input the Cabal package description.
{-# LANGUAGE CPP #-}
module Debian.Debianize.InputCabalPackageDescription
    ( inputCabalization
    , inputCabalization'
    ) where

import Control.Applicative ((<$>))
import Control.Category ((.))
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Lens.Lazy (access)
import Data.Set as Set (Set, toList)
import qualified Debian.Debianize.Types.Atoms as T (compilerFlavor)
import Debian.Debianize.Types.Atoms (verbosity, cabalFlagAssignments, buildEnv, packageDescription)
import Debian.Debianize.Monad (DebT)
import Debian.Debianize.Prelude (intToVerbosity', (~=))
import Debian.Debianize.Types.Atoms (EnvSet(dependOS))
import Debian.GHC (newestAvailableCompilerId)
import Debian.Orphans ()
import Distribution.Compiler (CompilerId)
#if MIN_VERSION_Cabal(1,22,0)
import Distribution.Compiler (unknownCompilerInfo, AbiTag(NoAbiTag))
#endif
import Distribution.Package (Package(packageId), Dependency)
import qualified Distribution.PackageDescription as Cabal (PackageDescription)
import Distribution.PackageDescription as Cabal (PackageDescription, FlagName)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Utils (defaultPackageDesc, die, setupMessage)
import Distribution.System (Platform(..), buildOS, buildArch)
import Distribution.Verbosity (Verbosity)
import Prelude hiding (readFile, lines, words, break, null, log, sum, (.))
-- import qualified Prelude (lines)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.Posix.Files (setFileCreationMask)
import System.Process (system)
-- import System.Unix.Chroot (useEnv)
-- import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

inputCabalization :: (MonadIO m, Functor m) => DebT m ()
inputCabalization =
    do vb <- access verbosity >>= return . intToVerbosity'
       flags <- access cabalFlagAssignments
       root <- dependOS <$> access buildEnv
       hc <- access T.compilerFlavor
       let cid = newestAvailableCompilerId root hc
       ePkgDesc <- liftIO $ inputCabalization' vb flags cid
       either (\ deps -> liftIO getCurrentDirectory >>= \ here ->
                         error $ "Missing dependencies in cabal package at " ++ here ++ ": " ++ show deps)
              (\ pkgDesc -> packageDescription ~= Just pkgDesc)
                 -- This will contain either the contents of the file given in
                 -- the license-file: field or the contents of the license:
                 -- field.

              ePkgDesc

-- | Load a GenericPackageDescription from the current directory and
-- from that create a finalized PackageDescription for the given
-- CompilerId.
inputCabalization' :: Verbosity -> Set (FlagName, Bool) -> CompilerId -> IO (Either [Dependency] PackageDescription)
inputCabalization' vb flags cid = do
  genPkgDesc <- defaultPackageDesc vb >>= readPackageDescription vb
#if MIN_VERSION_Cabal(1,22,0)
  let cid' = unknownCompilerInfo cid NoAbiTag
#else
  let cid' = cid
#endif
  let finalized = finalizePackageDescription (toList flags) (const True) (Platform buildArch buildOS) cid' [] genPkgDesc
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
