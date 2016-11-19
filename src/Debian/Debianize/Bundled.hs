-- | Determine whether a specific version of a Haskell package is
-- bundled with into this particular version of the given compiler.
-- This is done by getting the "Provides" field from the output of
-- "apt-cache showpkg ghc" (run in the appropriate changeroot) and
-- converting the debian package names back to Cabal package names.
-- *That* is done using the debianNameMap of CabalInfo, which is
-- built using the mapCabal, splitCabal, and remapCabal functions.

{-# LANGUAGE CPP, FlexibleContexts, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell #-}
module Debian.Debianize.Bundled
    ( builtIn
    -- * Utilities
    , aptCacheShowPkg
    , aptCacheProvides
    , aptCacheDepends
    , aptCacheConflicts
    , aptVersions
    , hcVersion
    , parseVersion'
    , tests
    ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (force, NFData)
import Control.Exception (SomeException, try)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans (MonadIO)
import Data.Char (isAlphaNum, toLower)
import Data.Function.Memoize (memoize2, memoize3)
import Data.List (groupBy, intercalate, isPrefixOf, stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set as Set (difference, fromList)
import Data.Version (makeVersion, parseVersion, Version(..))
import Debian.GHC (CompilerChoice(..), CompilerVendor(..))
import Debian.Relation (BinPkgName(..))
import Debian.Relation.ByteString ()
import Debian.Version (DebianVersion, parseDebianVersion', prettyDebianVersion)
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.Simple.Compiler (CompilerFlavor(..))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess, showCommandForUser)
import System.Unix.Chroot (useEnv)
import Test.HUnit (assertEqual, Test(TestList, TestCase))
import Text.ParserCombinators.ReadP (char, endBy1, munch1, ReadP, readP_to_S)
import Text.Regex.TDFA ((=~))

-- | Find out what version, if any, of a cabal library is built into
-- the newest version of haskell compiler hc in environment root.
-- This is done by looking for .conf files beneath a package.conf.d
-- directory and parsing the name.  (Probably better to actually read
-- the .conf file.)
builtIn :: CompilerChoice -> FilePath -> [PackageIdentifier]
builtIn hc root =
  let Just hcname = (hcExecutablePath root hc >>= hcBinPkgName root) in
  aptCacheProvides hcname root

-- | Convert CompilerFlavor to an executable name in a way that works
-- for at least the cases we are interested in.  This might need to be
-- fudged or replaced as more cases become interesting.
hcExecutable :: CompilerChoice -> String
hcExecutable = map toLower . show . _hcFlavor

-- | Use which(1) to determine full path name to haskell compiler executable
hcExecutablePath :: FilePath -> CompilerChoice -> Maybe FilePath
hcExecutablePath = memoize2 $ \root hc ->
  listToMaybe $ lines $ unsafePerformIO $ chroot root (readProcess "which" [hcExecutable hc] "")

hcVersion :: FilePath -> CompilerChoice -> Maybe Version
hcVersion root hc =
    let Just hcpath = hcExecutablePath root hc in
    maybe Nothing parseVersion' $
     listToMaybe $
     lines $
     unsafePerformIO . chroot root $
     readProcess hcpath
                 [case _hcFlavor hc of
#if MIN_VERSION_Cabal(1,22,0)
                    GHCJS -> "--numeric-ghc-version"
#endif
                    _ -> "--numeric-version"]
                 ""

-- | Use dpkg -S to convert the executable path to a debian binary
-- package name.
hcBinPkgName :: FilePath -> FilePath -> Maybe BinPkgName
hcBinPkgName = memoize2 $ \root path ->
  let s = unsafePerformIO (chroot root (readProcess "dpkg" ["-S", path] "")) in
  case map (takeWhile (/= ':')) (lines s) of
    [] -> Nothing
    [name] -> Just (BinPkgName name)
    _ -> error $ "Unexpected output from " ++ showCommandForUser "dpkg" ["-S", path] ++ ": ++ " ++ show s

-- | What built in libraries does this haskell compiler provide?
aptCacheProvides :: BinPkgName -> FilePath -> [PackageIdentifier]
aptCacheProvides = memoize2 aptCacheProvides'
    where
      aptCacheProvides' hcname root =
        {-trace ("aptCacheProvides " ++ show hcname ++ " in " ++ root ++ " -> " ++ show pis)-}
          packageIdentifiers root hcname

packageIdentifiers :: String -> BinPkgName -> [PackageIdentifier]
packageIdentifiers root hcname =
    (mapMaybe parsePackageIdentifier' .
     mapMaybe (dropRequiredSuffix ".conf") .
     map last .
     filter (elem "package.conf.d") .
     map (groupBy (\a b -> (a == '/') == (b == '/')))) (binPkgFiles root hcname)

dropRequiredSuffix :: String -> String -> Maybe String
dropRequiredSuffix suff x =
    let (x', suff') = splitAt (length x - length suff) x in if suff == suff' then Just x' else Nothing

binPkgFiles :: String -> BinPkgName -> [FilePath]
binPkgFiles root hcname = lines $ unsafePerformIO (chroot root (readProcess "dpkg" ["-L", unBinPkgName hcname] ""))

{-
takeBetween :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeBetween startPred endPred = takeWhile (not . endPred) . dropWhile startPred . dropWhile (not . startPred)
-}

aptCacheConflicts :: FilePath -> String -> DebianVersion -> [BinPkgName]
aptCacheConflicts root hcname ver =
    either (\ _ -> []) (mapMaybe doLine . lines) (aptCacheDepends root hcname (show (prettyDebianVersion ver)))
    where
      doLine s = case s =~ "^[ ]*Conflicts:[ ]*<(.*)>$" :: (String, String, String, [String]) of
                   (_, _, _, [name]) -> Just (BinPkgName name)
                   _ -> Nothing

aptCacheDepends :: FilePath -> String -> String -> Either SomeException String
aptCacheDepends =
    memoize3 (\ root hcname ver -> unsafePerformIO (try (chroot root (readProcess "apt-cache" ["depends", hcname ++ "=" ++ ver] ""))))

aptVersions :: FilePath -> BinPkgName -> [DebianVersion]
aptVersions root hcname =
    either (\ _ -> []) (map parseDebianVersion' . filter (/= "") . map (takeWhile (/= ' ')) . takeWhile (not . isPrefixOf "Reverse Depends:") . drop 1 . dropWhile (not . isPrefixOf "Versions:") . lines) (aptCacheShowPkg root hcname)

aptCacheShowPkg :: FilePath -> BinPkgName -> Either SomeException String
aptCacheShowPkg =
    memoize2 (\ root hcname -> unsafePerformIO (try (chroot root (readProcess "apt-cache" ["showpkg", unBinPkgName hcname] ""))))


chroot :: (NFData a, MonadIO m, MonadMask m) => String -> m a -> m a
chroot "/" = id
chroot root = useEnv root (return . force)

-- | A package identifier is a package name followed by a dash and
-- then a version number.  A package name, according to the cabal
-- users guide "can use letters, numbers and hyphens, but not spaces."
-- So be it.
parsePackageIdentifier :: ReadP PackageIdentifier
parsePackageIdentifier = do
  makeId <$> ((,) <$> endBy1 (munch1 isAlphaNum) (char '-') <*> parseVersion)
    where
      makeId :: ([String], Version) -> PackageIdentifier
      makeId (xs, v) = PackageIdentifier {pkgName = PackageName (intercalate "-" xs), pkgVersion = v}

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe p = listToMaybe . map fst . filter ((== "") . snd) . readP_to_S p

parseVersion' :: String -> Maybe Version
parseVersion' = parseMaybe parseVersion

parsePackageIdentifier' :: String -> Maybe PackageIdentifier
parsePackageIdentifier' = parseMaybe parsePackageIdentifier

tests :: Test
tests = TestList [ TestCase (assertEqual "Bundled1"
                               (Just (PackageIdentifier (PackageName "HUnit") (makeVersion [1,2,3])))
                               (parseMaybe parsePackageIdentifier "HUnit-1.2.3"))
                 , TestCase (assertEqual "Bundled2"
                               Nothing
                               (parseMaybe parsePackageIdentifier "HUnit-1.2.3 "))
                 , TestCase $ do
                     ghc <- (head . lines) <$> readProcess "which" ["ghc"] ""
                     let ver = fmap (takeWhile (/= '/')) (stripPrefix "/opt/ghc/" ghc)
                     case ver of
                       Just s -> do
                           let expected =
                                   (Set.fromList
                                           -- This is the package list for ghc-7.10.3
                                           ["array", "base", "binary", "bin-package-db", "bytestring", "Cabal",
                                            "containers", "deepseq", "directory", "filepath", "ghc", "ghc-prim",
                                            "haskeline", "hoopl", "hpc", "integer-gmp", "pretty", "process",
                                            "template-haskell", "terminfo", "time", "transformers", "unix", "xhtml"])
                               actual = Set.fromList (map (unPackageName . pkgName) (aptCacheProvides (BinPkgName ("ghc-" ++ s)) "/"))
                           assertEqual "Bundled4" (mempty, mempty) (Set.difference expected actual, Set.difference actual expected)
                 ]
