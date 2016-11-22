{-# LANGUAGE CPP, DeriveDataTypeable, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.GHC
    ( withCompilerVersion
    , newestAvailable
    , compilerIdFromDebianVersion
    , compilerFlavorOption
    , newestAvailableCompilerId
    -- , ghcNewestAvailableVersion'
    -- , ghcNewestAvailableVersion
    -- , compilerIdFromDebianVersion
    , CompilerVendor (Debian, HVR)
    , hvrCabalVersion
    , hvrHappyVersion
    , hvrAlexVersion
    , compilerPATH
    , withCompilerPATH
    , withModifiedPATH
    , CompilerChoice(..), hcVendor, hcFlavor
    , compilerPackageName
#if MIN_VERSION_Cabal(1,22,0)
    , getCompilerInfo
#endif
    ) where

import Control.DeepSeq (force)
import Control.Exception (SomeException, throw, try)
import Control.Lens (makeLenses)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (isSpace, {-toLower,-} toUpper)
import Data.Function.Memoize (deriveMemoizable, memoize2)
import Data.Generics (Data, Typeable)
import Data.List (intercalate)
import Data.Version (showVersion, Version(..), parseVersion)
import Debian.Debianize.BinaryDebDescription (PackageType(..))
import Debian.Relation (BinPkgName(BinPkgName))
import Debian.Version (DebianVersion, parseDebianVersion')
import Distribution.Compiler (CompilerFlavor(..), CompilerId(CompilerId))
#if MIN_VERSION_Cabal(1,22,0)
import Distribution.Compiler (CompilerInfo(..), unknownCompilerInfo, AbiTag(NoAbiTag))
#endif
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(..))
import System.Directory (doesDirectoryExist)
import System.Environment (getEnv, setEnv)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
-- import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess, showCommandForUser, readProcessWithExitCode)
import System.Unix.Chroot (useEnv, fchroot)
import System.Unix.Mount (WithProcAndSys)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Read (readMaybe)

$(deriveMemoizable ''CompilerFlavor)
$(deriveMemoizable ''Version)
$(deriveMemoizable ''BinPkgName)
deriving instance Data CompilerVendor
deriving instance Typeable CompilerVendor

-- | Up until now this system only worked with Debian's or Ubuntu's
-- ghc source package, which has binary package names ghc, ghc-prof,
-- ghc-doc, etc.  This type is intended to add support for Herbert
-- Valerio Riedel's (hvr's) repository of several different versions
-- of ghc and supporting tools happy, alex and cabal.  These have
-- different binary package names, and the packages put the
-- executables in different locations than the Debian (and Ubuntu)
-- packages.  This option is activated by the --hvr-version option to
-- cabal-debian.
data CompilerChoice =
    CompilerChoice { _hcVendor :: CompilerVendor
                   , _hcFlavor :: CompilerFlavor
                   } deriving (Eq, Ord, Show, Data, Typeable)
data CompilerVendor = Debian | HVR Version deriving (Eq, Ord, Show)

withCompilerVersion :: FilePath -> CompilerChoice -> (Either String DebianVersion -> a) -> a
withCompilerVersion root hc f = f (newestAvailableCompiler root hc)

withCompilerPATH :: MonadIO m => CompilerVendor -> m a -> m a
withCompilerPATH vendor action = withModifiedPATH (compilerPATH vendor) action

compilerPATH :: CompilerVendor -> String -> String
compilerPATH vendor path0 = do
  case vendor of
    Debian -> path0
    HVR v -> (intercalate ":" ["/opt/ghc/" ++ showVersion v ++ "/bin",
                               "/opt/cabal/" ++ showVersion (hvrCabalVersion v) ++ "/bin",
                               "/opt/happy/" ++ showVersion (hvrHappyVersion v) ++ "/bin",
                               "/opt/alex/" ++ showVersion (hvrAlexVersion v) ++ "/bin",
                               path0])

-- | What version of Cabal goes with this version of GHC?
hvrCabalVersion :: Version -> Version
hvrCabalVersion (Version (m : n : _) _) | (m == 7 && n <= 7) || m < 7 = Version [1,16] []
hvrCabalVersion (Version (7 : n : _) _) | n <= 9 = Version [1,18] []
hvrCabalVersion (Version (7 : _) _) = Version [1,22] []
hvrCabalVersion _ = Version [1,24] []

-- | What version of Happy goes with this version of GHC?
hvrHappyVersion :: Version -> Version
hvrHappyVersion (Version (m : n : _) _) | (m == 7 && n <= 3) || m < 7 = Version [1,19,3] []
hvrHappyVersion (Version (7 : n : _) _) | n <= 2 = Version [1,19,3] []
hvrHappyVersion _ = Version [1,19,5] []

-- | What version of Alex goes with this version of GHC?
hvrAlexVersion :: Version -> Version
hvrAlexVersion _ = Version [3,1,7] []

withModifiedPATH :: MonadIO m => (String -> String) -> m a -> m a
withModifiedPATH f action = do
  path0 <- liftIO $ getEnv "PATH"
  liftIO $ setEnv "PATH" (f path0)
  -- liftIO $ hPutStrLn stderr $ "*** withCompilerPath vendor=" ++ show vendor
  -- liftIO $ hPutStrLn stderr $ "*** Setting $PATH to " ++ show path
  r <- action
  -- liftIO $ hPutStrLn stderr $ "*** Resetting $PATH to " ++ show path0
  liftIO $ setEnv "PATH" path0
  return r

-- | Memoized version of newestAvailable'
newestAvailable :: FilePath -> BinPkgName -> Either String DebianVersion
newestAvailable root pkg =
    memoize2 f pkg root
    where
      f :: BinPkgName -> FilePath -> Either String DebianVersion
      f pkg' root' = unsafePerformIO (newestAvailable' root' pkg')

-- | Look up the newest version of a deb available in the given changeroot.
newestAvailable' :: FilePath -> BinPkgName -> IO (Either String DebianVersion)
newestAvailable' root (BinPkgName name) = do
  exists <- doesDirectoryExist root
  case exists of
    False -> return $ Left $ "newestAvailable: no such environment: " ++ show root
    True -> do
      versions <- try $ chroot root $
                    (readProcess "apt-cache" ["showpkg", name] "" >>=
                    return . dropWhile (/= "Versions: ") . lines) :: IO (Either SomeException [String])
      case versions of
        Left e -> return $ Left $ "newestAvailable failed in " ++ show root ++ ": " ++ show e
        Right (_ : versionLine : _) -> return . Right . parseDebianVersion' . takeWhile (/= ' ') $ versionLine
        Right x -> return $ Left $ "Unexpected result from apt-cache showpkg: " ++ show x
        where
          chroot "/" = id
          chroot _ = useEnv root (return . force)

newestAvailableCompiler :: FilePath -> CompilerChoice -> Either String DebianVersion
newestAvailableCompiler root hc = newestAvailable root (compilerPackageName hc Development)

newestAvailableCompilerId :: FilePath -> CompilerChoice -> Either String CompilerId
newestAvailableCompilerId root hc@(CompilerChoice _ flavor) = fmap (compilerIdFromDebianVersion flavor) (newestAvailableCompiler root hc)

{-
-- | The IO portion of ghcVersion.  For there to be no version of ghc
-- available is an exceptional condition, it has been standard in
-- Debian and Ubuntu for a long time.
ghcNewestAvailableVersion :: CompilerFlavor -> FilePath -> IO DebianVersion
ghcNewestAvailableVersion hc root = do
  exists <- doesDirectoryExist root
  when (not exists) (error $ "ghcVersion: no such environment: " ++ show root)
  versions <- try $ chroot $
                (readProcess "apt-cache" ["showpkg", map toLower (show hc)] "" >>=
                return . dropWhile (/= "Versions: ") . lines) :: IO (Either SomeException [String])
  case versions of
    Left e -> error $ "ghcNewestAvailableVersion failed in " ++ show root ++ ": " ++ show e
    Right (_ : versionLine : _) -> return . parseDebianVersion . takeWhile (/= ' ') $ versionLine
    _ -> error $ "No version of ghc available in " ++ show root
    where
      chroot = case root of
                 "/" -> id
                 _ -> useEnv root (return . force)

-- | Memoize the CompilerId built for the newest available version of
-- the compiler package so we don't keep running apt-cache showpkg
-- over and over.
ghcNewestAvailableVersion' :: CompilerFlavor -> FilePath -> CompilerId
ghcNewestAvailableVersion' hc root =
    memoize f (hc, root)
    where
      f :: (CompilerFlavor, FilePath) -> CompilerId
      f (hc', root) = unsafePerformIO (g hc' root)
      g hc root = do
        ver <- ghcNewestAvailableVersion hc root
        let cid = compilerIdFromDebianVersion ver
        -- hPutStrLn stderr ("GHC Debian version: " ++ show ver ++ ", Compiler ID: " ++ show cid)
        return cid
-}

compilerIdFromDebianVersion :: CompilerFlavor -> DebianVersion -> CompilerId
compilerIdFromDebianVersion hc debVersion =
    let (Version ds ts) = greatestLowerBound debVersion (map (\ d -> Version [d] []) [0..]) in
    CompilerId hc (greatestLowerBound debVersion (map (\ d -> Version (ds ++ [d]) ts) [0..]))
    where
      greatestLowerBound :: DebianVersion -> [Version] -> Version
      greatestLowerBound b xs = last $ takeWhile (\ v -> parseDebianVersion' (showVersion v) < b) xs

-- | General function to build a command line option that reads most
-- of the possible values for CompilerFlavor.
compilerFlavorOption :: forall a. (CompilerFlavor -> a -> a) -> OptDescr (a -> a)
compilerFlavorOption f =
    Option [] ["hc", "compiler-flavor"] (ReqArg readHC "COMPILER") "Build packages using this Haskell compiler"
    where
      -- Most of the constructors in CompilerFlavor are arity zero and
      -- all caps, though two are capitalized - Hugs and Helium.  This
      -- won't read those, and it won't read HaskellSuite String or
      -- OtherCompiler String
      readHC :: String -> a -> a
      readHC s = maybe (error $ "Invalid CompilerFlavor: " ++ show s) f (readMaybe (map toUpper s))

{-
debName :: CompilerFlavor -> Maybe BinPkgName
debName hc =
    case map toLower (show hc) of
      s | any isSpace s -> Nothing
      s -> Just (BinPkgName s)
-}

compilerPackageName :: CompilerChoice -> PackageType -> BinPkgName
compilerPackageName (CompilerChoice Debian GHC) Documentation = BinPkgName "ghc-doc"
compilerPackageName (CompilerChoice Debian GHC) Profiling = BinPkgName "ghc-prof"
compilerPackageName (CompilerChoice Debian GHC) Development = BinPkgName "ghc"
compilerPackageName (CompilerChoice Debian GHC) _ = BinPkgName "ghc" -- whatevs
compilerPackageName (CompilerChoice (HVR v) GHC) Documentation = BinPkgName ("ghc-" ++ showVersion v ++ "-htmldocs")
compilerPackageName (CompilerChoice (HVR v) GHC) Profiling = BinPkgName ("ghc-" ++ showVersion v ++ "-prof")
compilerPackageName (CompilerChoice (HVR v) GHC) Development = BinPkgName ("ghc-" ++ showVersion v)
compilerPackageName (CompilerChoice (HVR v) GHC) _ = BinPkgName ("ghc-" ++ showVersion v)
#if MIN_VERSION_Cabal(1,22,0)
compilerPackageName (CompilerChoice _ GHCJS) Documentation = BinPkgName "ghcjs"
compilerPackageName (CompilerChoice _ GHCJS) Profiling = error "Profiling not supported for GHCJS"
compilerPackageName (CompilerChoice _ GHCJS) Development = BinPkgName "ghcjs"
compilerPackageName (CompilerChoice _ GHCJS) _ = BinPkgName "ghcjs" -- whatevs
#endif
compilerPackageName hc _ = error $ "Unsupported compiler flavor: " ++ show hc

#if MIN_VERSION_Cabal(1,22,0)
-- | IO based alternative to newestAvailableCompilerId - install the
-- compiler into the chroot if necessary and ask it for its version
-- number.  This has the benefit of working for ghcjs, which doesn't
-- make the base ghc version available in the version number.
getCompilerInfo :: MonadIO m => FilePath -> CompilerFlavor -> WithProcAndSys m (Either String CompilerInfo)
getCompilerInfo "/" flavor = liftIO $ getCompilerInfo' flavor
getCompilerInfo root flavor = liftIO $ fchroot root $ getCompilerInfo' flavor

getCompilerInfo' :: CompilerFlavor -> IO (Either String CompilerInfo)
getCompilerInfo' flavor = do
  r <- try $ readProcessWithExitCode (hcCommand flavor) ["--numeric-version"] ""
  case r of
    Left e | isDoesNotExistError e -> return $ Left $ "getCompilerInfo - " ++ show e
    Left e -> throw e
    Right r'@(ExitFailure _, _, _) ->
        error $ processErrorMessage "getCompilerInfo" (hcCommand flavor) ["--numeric-version"] r'
    Right (_, out, _) -> do
      let compilerId = maybe (error $ "Parse error in version string: " ++ show out) (CompilerId flavor) (toVersion out)
      compilerCompat <- case flavor of
                          GHCJS -> do
                            (r' :: Either IOError (ExitCode, String, String)) <- try $ readProcessWithExitCode (hcCommand flavor) ["--numeric-ghc-version"] ""
                            case r' of
                              Right (ExitSuccess, out', _) ->
                                  maybe (error $ "getCompilerInfo - parse error in version string: " ++ show out') (return . Just . (: []) . CompilerId GHC) (toVersion out')
                              _ -> error "getCompilerInfo - failure computing compilerCompat"
                          _ -> return Nothing
      return $ Right $ (unknownCompilerInfo compilerId NoAbiTag) {compilerInfoCompat = compilerCompat}

toVersion :: String -> Maybe Version
toVersion s = case filter (all isSpace . snd) (readP_to_S parseVersion s) of
                [(v, _)] -> Just v
                _ -> Nothing

processErrorMessage :: String -> String -> [String] -> (ExitCode, String, String) -> String
processErrorMessage msg cmd args (ExitFailure n, out, err) =
    msg ++ " - " ++ showCommandForUser cmd args ++ " -> " ++ show n ++ "\n stdout: " ++ indent out ++ "\n stderr: " ++ indent err
    where
      indent :: String -> String
      indent = intercalate "\n         " . lines

hcCommand :: CompilerFlavor -> String
hcCommand GHC = "ghc"
hcCommand GHCJS = "ghcjs"
hcCommand flavor = error $ "hcCommand - unexpected CompilerFlavor: " ++ show flavor
#endif

$(makeLenses ''CompilerChoice)
$(deriveMemoizable ''CompilerVendor)
$(deriveMemoizable ''CompilerChoice)
