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
    , hvrCabalVersion
    , hvrHappyVersion
    , hvrAlexVersion
    , hvrCompilerPATH
    , isHVRCompilerPackage
    , withModifiedPATH
    -- , CompilerChoice(..), hcVendor, hcFlavor
    , compilerPackageName
#if MIN_VERSION_Cabal(1,22,0)
    , getCompilerInfo
#endif
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.DeepSeq (force)
import Control.Exception (SomeException, throw, try)
import Control.Lens (_2, over)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (isSpace, toLower, toUpper)
import Data.Function.Memoize (deriveMemoizable, Memoizable, memoize, memoize2, memoizeFinite)
import Data.List (intercalate, isPrefixOf)
import Debian.Debianize.BinaryDebDescription (PackageType(..))
import Debian.Relation (BinPkgName(BinPkgName))
import Debian.Version (DebianVersion, parseDebianVersion')
import Distribution.Compiler (CompilerFlavor(..), CompilerId(CompilerId))
#if MIN_VERSION_Cabal(1,22,0)
import Distribution.Compiler (CompilerInfo(..), unknownCompilerInfo, AbiTag(NoAbiTag))
#endif
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Pretty (prettyShow)
import Distribution.Version (mkVersion', mkVersion, Version, versionNumbers)
import Data.Version (parseVersion)
import Data.Word (Word64)
#else
import Data.Function.Memoize (deriveMemoizable, memoize, memoize2)
import Data.Version (showVersion, Version(..), parseVersion)
#endif
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(..))
import System.Directory (doesDirectoryExist)
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
-- import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess, showCommandForUser, readProcessWithExitCode)
import System.Posix.Env (setEnv)
import System.Unix.Chroot (useEnv, fchroot)
import System.Unix.Mount (WithProcAndSys)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

#if MIN_VERSION_Cabal(2,0,0)
instance Memoizable Word64 where memoize = memoizeFinite
#endif
$(deriveMemoizable ''CompilerFlavor)
$(deriveMemoizable ''Version)
$(deriveMemoizable ''BinPkgName)

-- | Up until now cabal-debian only worked with Debian's or Ubuntu's
-- ghc debs, which have binary package names ghc, ghc-prof, ghc-doc,
-- etc.  This type is intended to add support for Herbert Valerio
-- Riedel's (hvr's) repository of several different versions of ghc
-- and supporting tools happy, alex and cabal.  These have different
-- binary package names, and the packages put the executables in
-- different locations than the Debian (and Ubuntu) packages.  This
-- option is activated when a directory such as /opt/ghc/8.0.1/bin is
-- present in $PATH and a ghc executable is found there.
--
-- This function decides whether a deb name is that of one of
-- debian/ubuntu's ghc packages or one of hvr's.  If it is an hvr
-- package it returns the version number embedded in its name.
isHVRCompilerPackage :: CompilerFlavor -> BinPkgName -> Maybe Version
isHVRCompilerPackage hc (BinPkgName name) =
    case isPrefixOf prefix name of
      True -> toVersion (takeWhile (/= '-') (drop (length prefix) name))
      False -> Nothing
      where
        prefix = map toLower (show hc) ++ "-"

toVersion :: String -> Maybe Version
toVersion s = case filter (all isSpace . snd) (readP_to_S parseVersion s) of
#if MIN_VERSION_Cabal(2,0,0)
                [(v, _)] -> Just (mkVersion' v)
#else
                [(v, _)] -> Just v
#endif
                _ -> Nothing

withCompilerVersion :: FilePath -> CompilerFlavor -> (DebianVersion -> a) -> Either String a
withCompilerVersion root hc f = either Left (\v -> Right (f v)) (newestAvailableCompiler root hc)

-- | Return the a string containing the PATH environment variable value
-- suitable for using some version of ghc from hvr's compiler repo.
hvrCompilerPATH :: Version -> String -> String
hvrCompilerPATH v path0 =
    intercalate ":" ["/opt/ghc/" ++ prettyShow v ++ "/bin",
                     "/opt/cabal/" ++ prettyShow (hvrCabalVersion v) ++ "/bin",
                     "/opt/happy/" ++ prettyShow (hvrHappyVersion v) ++ "/bin",
                     "/opt/alex/" ++ prettyShow (hvrAlexVersion v) ++ "/bin",
                     path0]

-- | What version of Cabal goes with this version of GHC?
hvrCabalVersion :: Version -> Version
#if MIN_VERSION_Cabal(2,0,0)
hvrCabalVersion v =
  case versionNumbers v of
    (m : n : _) | (m == 7 && n <= 7) || m < 7 -> mkVersion [1,16]
    (7 : n : _) | n <= 9 -> mkVersion [1,18]
    (7 : _) -> mkVersion [1,22]
    _ -> mkVersion [1,24]
#else
hvrCabalVersion (Version (m : n : _) _) | (m == 7 && n <= 7) || m < 7 = Version [1,16] []
hvrCabalVersion (Version (7 : n : _) _) | n <= 9 = Version [1,18] []
hvrCabalVersion (Version (7 : _) _) = Version [1,22] []
hvrCabalVersion _ = Version [1,24] []
#endif

-- | What version of Happy goes with this version of GHC?
hvrHappyVersion :: Version -> Version
#if MIN_VERSION_Cabal(2,0,0)
hvrHappyVersion v =
    case versionNumbers v of
      (m : n : _) | (m == 7 && n <= 3) || m < 7 -> mkVersion [1,19,3]
      (7 : n : _) | n <= 2 -> mkVersion [1,19,3]
      _ -> mkVersion [1,19,5]
#else
hvrHappyVersion (Version (m : n : _) _) | (m == 7 && n <= 3) || m < 7 = Version [1,19,3] []
hvrHappyVersion (Version (7 : n : _) _) | n <= 2 = Version [1,19,3] []
hvrHappyVersion _ = Version [1,19,5] []
#endif

-- | What version of Alex goes with this version of GHC?
hvrAlexVersion :: Version -> Version
#if MIN_VERSION_Cabal(2,0,0)
hvrAlexVersion _ = mkVersion [3,1,7]
#else
hvrAlexVersion _ = Version [3,1,7] []
#endif

withModifiedPATH :: MonadIO m => (String -> String) -> m a -> m a
withModifiedPATH f action = do
  path0 <- liftIO $ getEnv "PATH"
  liftIO $ setEnv "PATH" (f path0) True
  -- liftIO $ hPutStrLn stderr $ "*** withCompilerPath vendor=" ++ show vendor
  -- liftIO $ hPutStrLn stderr $ "*** Setting $PATH to " ++ show path
  r <- action
  -- liftIO $ hPutStrLn stderr $ "*** Resetting $PATH to " ++ show path0
  liftIO $ setEnv "PATH" path0 True
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

newestAvailableCompiler :: FilePath -> CompilerFlavor -> Either String DebianVersion
newestAvailableCompiler root hc = maybe (Left "No compiler package") (newestAvailable root) (compilerPackageName hc Development)

newestAvailableCompilerId :: FilePath -> CompilerFlavor -> Either String CompilerId
newestAvailableCompilerId root hc = either Left (Right . compilerIdFromDebianVersion hc) (newestAvailableCompiler root hc)

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
#if MIN_VERSION_Cabal(2,0,0)
    let ds = versionNumbers (greatestLowerBound debVersion (map (\ d -> mkVersion [d]) [0..])) in
    CompilerId hc (greatestLowerBound debVersion (map (\ d -> mkVersion (ds ++ [d])) [0..]))
#else
    let (Version ds ts) = greatestLowerBound debVersion (map (\ d -> Version [d] []) [0..]) in
    CompilerId hc (greatestLowerBound debVersion (map (\ d -> Version (ds ++ [d]) ts) [0..]))
#endif
    where
      greatestLowerBound :: DebianVersion -> [Version] -> Version
      greatestLowerBound b xs = last $ takeWhile (\ v -> parseDebianVersion' (prettyShow v) < b) xs

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

-- | Compute the compiler package names by finding out what package
-- contains the corresponding executable.
compilerPackageName :: CompilerFlavor -> PackageType -> Maybe BinPkgName
compilerPackageName hc typ =
    maybe Nothing (Just . finish) (compilerPackage hc)
    where
      finish (BinPkgName hcname) =
          let isDebian = map toLower (show hc) == hcname in
          -- hcname is the package that contains the compiler
          -- executable.  This will be ghc or ghcjs for Debian
          -- packages, anything else is an hvr package.
          case (hc, typ, isDebian) of
            -- Debian puts the .haddock files in ghc-doc
            (GHC, Documentation, True) -> BinPkgName (hcname ++ "-doc")
            -- In HVR repo the .haddock files required to buid html
            -- are in the main compiler package.  However, the html
            -- files in ghc-<version>-htmldocs are also needed to
            -- create links.
            (GHC, Documentation, False) -> BinPkgName (hcname ++ "-htmldocs")
            (GHC, Profiling, _) -> BinPkgName (hcname ++ "-prof")
            _ -> BinPkgName hcname

compilerPackage :: CompilerFlavor -> Maybe BinPkgName
compilerPackage GHC = filePackage "ghc"
#if MIN_VERSION_Cabal(1,22,0)
compilerPackage GHCJS = filePackage "ghcjs"
#endif
compilerPackage x = error $ "compilerPackage - unsupported CompilerFlavor: " ++ show x

{-
compilerExecutable :: CompilerFlavor -> String
compilerExecutable GHC = "ghc"
#if MIN_VERSION_Cabal(1,22,0)
compilerExecutable GHCJS = "ghcjs"
#endif
compilerExecutable x = error $ "compilerExecutable - unexpected flavor: " ++ show x
-}

filePackage :: FilePath -> Maybe BinPkgName
filePackage = memoize f
    where
      f :: FilePath -> Maybe BinPkgName
      f p = unsafePerformIO (which p >>= maybe (return Nothing) (\x -> package <$> readProcess "dpkg-query" ["-S", x] ""))
      package :: String -> Maybe BinPkgName
      package s =
          case s =~ "^(.*): .*$" :: (String, String, String, [String]) of
            (_, _, _, [name]) -> Just (BinPkgName name)
            _ -> Nothing

which :: String -> IO (Maybe FilePath)
which bin = do
  (toPath . over _2 lines) <$> readProcessWithExitCode "which" [bin] ""
    where
      toPath :: (ExitCode, [String], String) -> Maybe String
      toPath (ExitSuccess, [path], _) = Just path
      toPath _ = Nothing

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
#if MIN_VERSION_Cabal(1,22,0)
                          GHCJS -> do
                            (r' :: Either IOError (ExitCode, String, String)) <- try $ readProcessWithExitCode (hcCommand flavor) ["--numeric-ghc-version"] ""
                            case r' of
                              Right (ExitSuccess, out', _) ->
                                  maybe (error $ "getCompilerInfo - parse error in version string: " ++ show out') (return . Just . (: []) . CompilerId GHC) (toVersion out')
                              _ -> error "getCompilerInfo - failure computing compilerCompat"
#endif
                          _ -> return Nothing
      return $ Right $ (unknownCompilerInfo compilerId NoAbiTag) {compilerInfoCompat = compilerCompat}

processErrorMessage :: String -> String -> [String] -> (ExitCode, String, String) -> String
processErrorMessage msg cmd args (ExitFailure n, out, err) =
    msg ++ " - " ++ showCommandForUser cmd args ++ " -> " ++ show n ++ "\n stdout: " ++ indent out ++ "\n stderr: " ++ indent err
    where
      indent :: String -> String
      indent = intercalate "\n         " . lines
processErrorMessage _msg _cmd _args (ExitSuccess, _out, _err) = ""

hcCommand :: CompilerFlavor -> String
hcCommand GHC = "ghc"
#if MIN_VERSION_Cabal(1,22,0)
hcCommand GHCJS = "ghcjs"
#endif
hcCommand flavor = error $ "hcCommand - unexpected CompilerFlavor: " ++ show flavor
#endif
