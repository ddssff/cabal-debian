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
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set as Set (Set, toList)
import Data.Text (Text, pack, strip, null)
import qualified Debian.Debianize.Types.Atoms as T (compilerFlavors)
import Debian.Debianize.Types.CopyrightDescription (CopyrightDescription(..), FilesOrLicenseDescription(..))
import Debian.Debianize.Types.Atoms (verbosity, cabalFlagAssignments, buildEnv, packageDescription, copyright)
import Debian.Debianize.Monad (DebT)
import Debian.Debianize.Prelude (readFileMaybe, intToVerbosity', (~=), (%=))
import Debian.Debianize.Types.Atoms (EnvSet(dependOS))
import Debian.GHC (newestAvailableCompilerId)
import Debian.Orphans ()
import Debian.Policy (fromCabalLicense)
import Distribution.Compiler (CompilerId)
#if MIN_VERSION_Cabal(1,22,0)
import Distribution.Compiler (unknownCompilerInfo, AbiTag(NoAbiTag))
#endif
import Distribution.Package (Package(packageId), Dependency)
import qualified Distribution.PackageDescription as Cabal (PackageDescription(license, copyright))
#if MIN_VERSION_Cabal(1,19,0)
import qualified Distribution.PackageDescription as Cabal (PackageDescription(licenseFiles))
#else
import qualified Distribution.PackageDescription as Cabal (PackageDescription(licenseFile))
#endif
import Distribution.PackageDescription as Cabal (GenericPackageDescription, PackageDescription, FlagName)
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
       hcs <- access T.compilerFlavors
       let cids = map (newestAvailableCompilerId root) (toList hcs)
       ePkgDescs <- liftIO $ inputCabalization' vb flags cids
       mapM_ (either (\ deps -> liftIO getCurrentDirectory >>= \ here ->
                                error $ "Missing dependencies in cabal package at " ++ here ++ ": " ++ show deps)
                     (\ pkgDesc -> do
                        packageDescription ~= Just pkgDesc
                        -- This will contain either the contents of the file given in
                        -- the license-file: field or the contents of the license:
                        -- field.
                        licenseFiles <- mapM (\ path -> liftIO (readFileMaybe path) >>= \ text -> return (path, text))
#if MIN_VERSION_Cabal(1,19,0)
                                             (Cabal.licenseFiles pkgDesc)
#else
                                             (case Cabal.licenseFile pkgDesc of
                                                "" -> []
                                                path -> [path])
#endif
                        -- It is possible we might interpret the license file path
                        -- as a license name, so I hang on to it here.
                        let licenseFiles' = mapMaybe (\ (path, text) -> maybe Nothing (\ t -> Just (path, t)) text) licenseFiles
                        copyright %= cabalToCopyrightDescription pkgDesc licenseFiles'
                      ))
             ePkgDescs

cabalToCopyrightDescription :: PackageDescription -> [(FilePath, Text)] -> CopyrightDescription -> CopyrightDescription
cabalToCopyrightDescription pkgDesc licenseFiles cdesc =
    let triples = zip3 (repeat (nothingIf (null . strip) (pack (Cabal.copyright pkgDesc))))
                       (repeat (Cabal.license pkgDesc))
                       (case licenseFiles of
                          [] -> [Nothing]
                          xs -> map (Just. snd) xs)
        fnls = map (\ (copyrt, license, comment) ->
                         FilesDescription
                                {_filesPattern = "*"
                                , _filesCopyright = fromMaybe (pack "(No copyright field in cabal file)") copyrt
                                , _filesLicense = fromCabalLicense license
                                , _filesComment = comment }) triples in
     cdesc { _filesAndLicenses = fnls }

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p x = if p x then Nothing else Just x

-- | Load a GenericPackageDescription from the current directory and
-- finalize it for each element of the CompilerId list.
inputCabalization' :: Verbosity -> Set (FlagName, Bool) -> [CompilerId] -> IO [Either [Dependency] PackageDescription]
inputCabalization' vb flags cids = do
  genPkgDesc <- defaultPackageDesc vb >>= readPackageDescription vb
  mapM (inputCabalization'' vb flags genPkgDesc) cids

-- | Given a GenericPackageDescription (loaded from the current
-- directory), create a finalized PackageDescription for the given
-- CompilerId.
inputCabalization'' :: Verbosity -> Set (FlagName, Bool) -> GenericPackageDescription -> CompilerId -> IO (Either [Dependency] PackageDescription)
inputCabalization'' vb flags genPkgDesc cid = do
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
