{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables #-}
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
import Control.Exception (SomeException, throw, try)
import Control.Lens (_2, over)
import Control.Monad ((<=<))
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (isSpace, toLower, toUpper)
import Data.List (intercalate)
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
#else
import Data.Version (showVersion, Version(..), parseVersion)
#endif
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(..))
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
-- import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Process (readProcess, showCommandForUser, readProcessWithExitCode)
import System.Posix.Env (setEnv)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import UnliftIO.Memoize (memoizeMVar, runMemoized, Memoized)

toVersion :: String -> Maybe Version
toVersion s = case filter (all isSpace . snd) (readP_to_S parseVersion s) of
#if MIN_VERSION_Cabal(2,0,0)
                [(v, _)] -> Just (mkVersion' v)
#else
                [(v, _)] -> Just v
#endif
                _ -> Nothing

withCompilerVersion :: CompilerFlavor -> (DebianVersion -> a) -> IO (Either String a)
withCompilerVersion hc f = newestAvailableCompiler hc >>= \nac -> return (fmap f nac)

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
newestAvailable :: BinPkgName -> IO (Memoized (Either String DebianVersion))
newestAvailable pkg = memoizeMVar (f pkg)
    where
      f :: BinPkgName -> IO (Either String DebianVersion)
      f = newestAvailable'

-- | Look up the newest version of a deb available
newestAvailable' :: BinPkgName -> IO (Either String DebianVersion)
newestAvailable' (BinPkgName name) = do
      versions <- try $ dropWhile (/= "Versions: ") . lines <$> readProcess "apt-cache" ["showpkg", name] "" :: IO (Either SomeException [String])
      case versions of
        Left e -> return $ Left $ "newestAvailable failed: " ++ show e
        Right (_ : versionLine : _) -> return . Right . parseDebianVersion' . takeWhile (/= ' ') $ versionLine
        Right x -> return $ Left $ "Unexpected result from apt-cache showpkg: " ++ show x

newestAvailableCompiler :: CompilerFlavor -> IO (Either String DebianVersion)
newestAvailableCompiler hc = maybe (return (Left "No compiler package")) (runMemoized <=< newestAvailable) =<< compilerPackageName hc Development

newestAvailableCompilerId :: CompilerFlavor -> IO (Either String CompilerId)
newestAvailableCompilerId hc = fmap (compilerIdFromDebianVersion hc) <$> newestAvailableCompiler hc

{-
-- | The IO portion of ghcVersion.  For there to be no version of ghc
-- available is an exceptional condition, it has been standard in
-- Debian and Ubuntu for a long time.
ghcNewestAvailableVersion :: CompilerFlavor -> IO DebianVersion
ghcNewestAvailableVersion hc = do
  versions <- try $ chroot $
                (readProcess "apt-cache" ["showpkg", map toLower (show hc)] "" >>=
                return . dropWhile (/= "Versions: ") . lines) :: IO (Either SomeException [String])
  case versions of
    Left e -> error $ "ghcNewestAvailableVersion failed in: " ++ show e
    Right (_ : versionLine : _) -> return . parseDebianVersion . takeWhile (/= ' ') $ versionLine
    _ -> error $ "No version of ghc available"

-- | Memoize the CompilerId built for the newest available version of
-- the compiler package so we don't keep running apt-cache showpkg
-- over and over.
ghcNewestAvailableVersion' :: CompilerFlavor -> CompilerId
ghcNewestAvailableVersion' hc =
    memoize f hc
    where
      f :: (CompilerFlavor, FilePath) -> CompilerId
      f hc' = unsafePerformIO (g hc')
      g hc = do
        ver <- ghcNewestAvailableVersion hc
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
compilerPackageName :: CompilerFlavor -> PackageType -> IO (Maybe BinPkgName)
compilerPackageName hc typ = do
    mcp <- compilerPackage hc
    return $ fmap finish mcp
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

compilerPackage :: CompilerFlavor -> IO (Maybe BinPkgName)
compilerPackage GHC = filePackage "ghc" >>= runMemoized
#if MIN_VERSION_Cabal(1,22,0)
compilerPackage GHCJS = filePackage "ghcjs" >>= runMemoized
#endif
compilerPackage x = error $ "compilerPackage - unsupported CompilerFlavor: " ++ show x

filePackage :: FilePath -> IO (Memoized (Maybe BinPkgName))
filePackage = memoizeMVar . f
    where
      f :: FilePath -> IO (Maybe BinPkgName)
      f p = which p >>= maybe (return Nothing) (\x -> package <$> readProcess "dpkg-query" ["-S", x] "")
      package :: String -> Maybe BinPkgName
      package s =
          case s =~ "^(.*): .*$" :: (String, String, String, [String]) of
            (_, _, _, [name]) -> Just (BinPkgName name)
            _ -> Nothing

which :: String -> IO (Maybe FilePath)
which bin = toPath . over _2 lines <$> readProcessWithExitCode "which" [bin] ""
    where
      toPath :: (ExitCode, [String], String) -> Maybe String
      toPath (ExitSuccess, [path], _) = Just path
      toPath _ = Nothing

#if MIN_VERSION_Cabal(1,22,0)
-- | IO based alternative to newestAvailableCompilerId - install the
-- compiler into the chroot if necessary and ask it for its version
-- number.  This has the benefit of working for ghcjs, which doesn't
-- make the base ghc version available in the version number.
getCompilerInfo :: MonadIO m => CompilerFlavor -> m (Either String CompilerInfo)
getCompilerInfo flavor = liftIO $ getCompilerInfo' flavor

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
