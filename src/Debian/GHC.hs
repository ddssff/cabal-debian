{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables, TemplateHaskell #-}
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
    , compilerPackageName
#if MIN_VERSION_Cabal(1,22,0)
    , getCompilerInfo
#endif
    ) where

import Control.DeepSeq (force)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char ({-isSpace, toLower,-} toUpper)
import Data.Function.Memoize (deriveMemoizable, memoize2)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion, Version(Version), parseVersion)
import Debian.Debianize.BinaryDebDescription (PackageType(..))
import Debian.Relation (BinPkgName(BinPkgName))
import Debian.Version (DebianVersion, parseDebianVersion)
import Distribution.Compiler (CompilerFlavor(..), CompilerId(CompilerId))
#if MIN_VERSION_Cabal(1,22,0)
import Distribution.Compiler (CompilerInfo(..), unknownCompilerInfo, AbiTag(NoAbiTag))
#endif
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(..))
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode(ExitFailure))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess, showCommandForUser, readProcessWithExitCode)
import System.Unix.Chroot (useEnv, fchroot)
import System.Unix.Mount (WithProcAndSys)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Read (readMaybe)

$(deriveMemoizable ''CompilerFlavor)
$(deriveMemoizable ''BinPkgName)

withCompilerVersion :: FilePath -> CompilerFlavor -> (DebianVersion -> a) -> a
withCompilerVersion root hc f = f (newestAvailableCompiler root hc)

-- | Memoized version of newestAvailable'
newestAvailable :: FilePath -> BinPkgName -> Maybe DebianVersion
newestAvailable root pkg =
    memoize2 f pkg root
    where
      f :: BinPkgName -> FilePath -> Maybe DebianVersion
      f pkg' root' = unsafePerformIO (newestAvailable' root' pkg')

-- | Look up the newest version of a deb available in the given changeroot.
newestAvailable' :: FilePath -> BinPkgName -> IO (Maybe DebianVersion)
newestAvailable' root (BinPkgName name) = do
  exists <- doesDirectoryExist root
  when (not exists) (error $ "newestAvailable: no such environment: " ++ show root)
  versions <- try $ chroot root $
                (readProcess "apt-cache" ["showpkg", name] "" >>=
                return . dropWhile (/= "Versions: ") . lines) :: IO (Either SomeException [String])
  case versions of
    Left e -> error $ "newestAvailable failed in " ++ show root ++ ": " ++ show e
    Right (_ : versionLine : _) -> return . Just . parseDebianVersion . takeWhile (/= ' ') $ versionLine
    _ -> return Nothing
    where
      chroot "/" = id
      chroot _ = useEnv root (return . force)

newestAvailableCompiler :: FilePath -> CompilerFlavor -> DebianVersion
newestAvailableCompiler root hc =
    fromMaybe (error $ "newestAvailableCompiler - No versions of " ++ show hc ++ " available in " ++ show root)
              (newestAvailable root (compilerPackageName hc Development))

newestAvailableCompilerId :: FilePath -> CompilerFlavor -> CompilerId
newestAvailableCompilerId root hc = compilerIdFromDebianVersion hc (newestAvailableCompiler root hc)

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
      greatestLowerBound b xs = last $ takeWhile (\ v -> parseDebianVersion (showVersion v) < b) xs

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

compilerPackageName :: CompilerFlavor -> PackageType -> BinPkgName
compilerPackageName GHC Documentation = BinPkgName "ghc-doc" -- "ghc-7.10.1-htmldocs"
compilerPackageName GHC Profiling = BinPkgName "ghc-prof" -- "ghc-7.10.1-prof"
compilerPackageName GHC Development = BinPkgName "ghc" -- "ghc-7.10.1"
compilerPackageName GHC _ = BinPkgName "ghc" -- "ghc-7.10.1" -- whatevs
#if MIN_VERSION_Cabal(1,22,0)
compilerPackageName GHCJS Documentation = BinPkgName "ghcjs"
compilerPackageName GHCJS Profiling = error "Profiling not supported for GHCJS"
compilerPackageName GHCJS Development = BinPkgName "ghcjs"
compilerPackageName GHCJS _ = BinPkgName "ghcjs" -- whatevs
#endif
compilerPackageName x _ = error $ "Unsupported compiler flavor: " ++ show x

#if MIN_VERSION_Cabal(1,22,0)
-- | IO based alternative to newestAvailableCompilerId - install the
-- compiler into the chroot if necessary and ask it for its version
-- number.  This has the benefit of working for ghcjs, which doesn't
-- make the base ghc version available in the version number.
--
-- Assumes the compiler executable is already installed in the root
-- environment.
getCompilerInfo :: MonadIO m => FilePath -> CompilerFlavor -> WithProcAndSys m CompilerInfo
getCompilerInfo "/" flavor = liftIO $ getCompilerInfo' flavor
getCompilerInfo root flavor = liftIO $ fchroot root $ getCompilerInfo' flavor

getCompilerInfo' :: CompilerFlavor -> IO CompilerInfo
getCompilerInfo' flavor = do
    compilerId <- runVersionCommand >>= toCompilerId flavor
    compilerCompat <- case flavor of
                        GHCJS -> readProcessWithExitCode "ghcjs" ["--numeric-ghc-version"] "" >>= toCompilerId GHC >>= return . Just . (: [])
                        _ -> return Nothing
    return $ (unknownCompilerInfo compilerId NoAbiTag) {compilerInfoCompat = compilerCompat}
    where
      runVersionCommand :: IO (ExitCode, String, String)
      runVersionCommand = readProcessWithExitCode versionCommand ["--numeric-version"] ""
      versionCommand = case flavor of GHC -> "ghc"; GHCJS -> "ghcjs"; _ -> error $ "Flavor " ++ show flavor

      toCompilerId :: CompilerFlavor -> (ExitCode, String, String) -> IO CompilerId
      toCompilerId _ (ExitFailure n, _, err) =
          error $ showCommandForUser versionCommand ["--numeric-version"] ++ " -> " ++ show n ++ ", stderr: " ++ show err
      toCompilerId flavor' (_, out, _) =
          case filter ((== "\n") . snd) (readP_to_S parseVersion out) of
            [(v, _)] -> return $ CompilerId flavor' v
            _ -> error $ "Parse failure for version string: " ++ show out
#endif
