-- | Determine whether a specific version of a Haskell package is
-- bundled with into this particular version of the given compiler.
-- This is done by getting the "Provides" field from the output of
-- "apt-cache showpkg ghc" and
-- converting the debian package names back to Cabal package names.
-- *That* is done using the debianNameMap of CabalInfo, which is
-- built using the mapCabal, splitCabal, and remapCabal functions.

{-# LANGUAGE CPP, FlexibleContexts, ScopedTypeVariables #-}
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

import Control.Exception (SomeException, try)
import Control.Monad ((<=<))
import Data.Char (isAlphaNum, toLower)
import Data.List (groupBy, intercalate, isPrefixOf, stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set as Set (difference, fromList)
import Debian.GHC ({-instance Memoizable CompilerFlavor-})
import Debian.Relation (BinPkgName(..))
import Debian.Relation.ByteString ()
import Debian.Version (DebianVersion, parseDebianVersion', prettyDebianVersion)
import Distribution.Package (mkPackageName, PackageIdentifier(..), unPackageName)
import Data.Version (parseVersion)
import Distribution.Version(mkVersion, mkVersion', Version)
import Distribution.Simple.Compiler (CompilerFlavor(GHCJS))
import System.Process (readProcess, showCommandForUser)
import Test.HUnit (assertEqual, Test(TestList, TestCase))
import Text.ParserCombinators.ReadP (char, endBy1, munch1, ReadP, readP_to_S)
import Text.Regex.TDFA ((=~))
import UnliftIO.Memoize (memoizeMVar, Memoized, runMemoized)

-- | Find out what version, if any, of a cabal library is built into
-- the newest version of haskell compiler hc in environment root.
-- This is done by looking for .conf files beneath a package.conf.d
-- directory and parsing the name.  (Probably better to actually read
-- the .conf file.)
builtIn :: CompilerFlavor -> IO [PackageIdentifier]
builtIn hc = do
  Just hep <- hcExecutablePath hc >>= runMemoized
  Just hcname <- hcBinPkgName hep >>= runMemoized
  runMemoized =<< aptCacheProvides hcname

-- | Convert CompilerFlavor to an executable name in a way that works
-- for at least the cases we are interested in.  This might need to be
-- fudged or replaced as more cases become interesting.
hcExecutable :: CompilerFlavor -> String
hcExecutable = map toLower . show

-- | Use which(1) to determine full path name to haskell compiler executable
hcExecutablePath :: CompilerFlavor -> IO (Memoized (Maybe FilePath))
hcExecutablePath hc = memoizeMVar (listToMaybe . lines <$> readProcess "which" [hcExecutable hc] "")

hcVersion :: CompilerFlavor -> IO (Maybe Version)
hcVersion hc = do
    Just hcpath <- runMemoized =<< hcExecutablePath hc
    ver <- readProcess hcpath
                 [case hc of
                    GHCJS -> "--numeric-ghc-version"
                    _ -> "--numeric-version"]
                 ""
    return $ maybe Nothing parseVersion' (listToMaybe (lines ver))

-- | Use dpkg -S to convert the executable path to a debian binary
-- package name.
hcBinPkgName :: FilePath -> IO (Memoized (Maybe BinPkgName))
hcBinPkgName path = memoizeMVar $ do
  s <- readProcess "dpkg" ["-S", path] ""
  return $ case map (takeWhile (/= ':')) (lines s) of
    [] -> Nothing
    [name] -> Just (BinPkgName name)
    _ -> error $ "Unexpected output from " ++ showCommandForUser "dpkg" ["-S", path] ++ ": ++ " ++ show s

-- | What built in libraries does this haskell compiler provide?
aptCacheProvides :: BinPkgName -> IO (Memoized [PackageIdentifier])
aptCacheProvides = memoizeMVar . packageIdentifiers

packageIdentifiers :: BinPkgName -> IO [PackageIdentifier]
packageIdentifiers hcname =
    mapMaybe parsePackageIdentifier' .
    mapMaybe (dropRequiredSuffix ".conf" . last) .
    filter (elem "package.conf.d") .
    map (groupBy (\a b -> (a == '/') == (b == '/'))) <$> binPkgFiles hcname

dropRequiredSuffix :: String -> String -> Maybe String
dropRequiredSuffix suff x =
    let (x', suff') = splitAt (length x - length suff) x in if suff == suff' then Just x' else Nothing

-- | A list of the files in a binary deb
binPkgFiles :: BinPkgName -> IO [FilePath]
binPkgFiles hcname = lines <$> readProcess "dpkg" ["-L", unBinPkgName hcname] ""

aptCacheConflicts :: String -> DebianVersion -> IO [BinPkgName]
aptCacheConflicts hcname ver =
    either (const []) (mapMaybe doLine . lines) <$> (runMemoized =<< aptCacheDepends hcname (show (prettyDebianVersion ver)))
    where
      doLine s = case s =~ "^[ ]*Conflicts:[ ]*<(.*)>$" :: (String, String, String, [String]) of
                   (_, _, _, [name]) -> Just (BinPkgName name)
                   _ -> Nothing

aptCacheDepends :: String -> String -> IO (Memoized (Either SomeException String))
aptCacheDepends hcname ver =
    memoizeMVar (try (readProcess "apt-cache" ["depends", hcname ++ "=" ++ ver] ""))

aptVersions :: BinPkgName -> IO [DebianVersion]
aptVersions =
    return . either (const []) (map parseDebianVersion' . filter (/= "") . map (takeWhile (/= ' ')) . takeWhile (not . isPrefixOf "Reverse Depends:") . drop 1 . dropWhile (not . isPrefixOf "Versions:") . lines) <=< (runMemoized <=< aptCacheShowPkg)

aptCacheShowPkg :: BinPkgName -> IO (Memoized (Either SomeException String))
aptCacheShowPkg hcname =
    memoizeMVar (try (readProcess "apt-cache" ["showpkg", unBinPkgName hcname] ""))

-- | A package identifier is a package name followed by a dash and
-- then a version number.  A package name, according to the cabal
-- users guide "can use letters, numbers and hyphens, but not spaces."
-- So be it.
parsePackageIdentifier :: ReadP PackageIdentifier
parsePackageIdentifier =
  makeId <$> ((,) <$> endBy1 (munch1 isAlphaNum) (char '-') <*> parseCabalVersion)
    where
      makeId :: ([String], Version) -> PackageIdentifier
      makeId (xs, v) = PackageIdentifier {pkgName = mkPackageName (intercalate "-" xs), pkgVersion = v}

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe p = listToMaybe . map fst . filter ((== "") . snd) . readP_to_S p

parseVersion' :: String -> Maybe Version
parseVersion' = parseMaybe parseCabalVersion

parseCabalVersion :: ReadP Version
parseCabalVersion = fmap mkVersion' parseVersion

parsePackageIdentifier' :: String -> Maybe PackageIdentifier
parsePackageIdentifier' = parseMaybe parsePackageIdentifier

tests :: Test
tests = TestList [ TestCase (assertEqual "Bundled1"
                               (Just (PackageIdentifier (mkPackageName "HUnit") (mkVersion [1,2,3])))
                               (parseMaybe parsePackageIdentifier "HUnit-1.2.3"))
                 , TestCase (assertEqual "Bundled2"
                               Nothing
                               (parseMaybe parsePackageIdentifier "HUnit-1.2.3 "))
                 , TestCase $ do
                     ghc <- head . lines <$> readProcess "which" ["ghc"] ""
                     let ver = fmap (takeWhile (/= '/')) (stripPrefix "/opt/ghc/" ghc)
                     acp <- runMemoized =<< aptCacheProvides (BinPkgName ("ghc" ++ maybe "" ("-" ++) ver))
                     let expected = Set.fromList
                                -- This is the package list for ghc-7.10.3
                                ["array", "base", "binary", "bin-package-db", "bytestring", "Cabal",
                                 "containers", "deepseq", "directory", "filepath", "ghc", "ghc-prim",
                                 "haskeline", "hoopl", "hpc", "integer-gmp", "pretty", "process",
                                 "template-haskell", "terminfo", "time", "transformers", "unix", "xhtml"]
                         actual = Set.fromList (map (unPackageName . pkgName) acp)
                         missing (Just "8.0.1") = Set.fromList ["bin-package-db"]
                         missing (Just "8.0.2") = Set.fromList ["bin-package-db"]
                         missing _ = mempty
                         extra (Just "7.8.4") = Set.fromList ["haskell2010","haskell98","old-locale","old-time"]
                         extra (Just "8.0.1") = Set.fromList ["ghc-boot","ghc-boot-th","ghci"]
                         extra (Just "8.0.2") = Set.fromList ["ghc-boot","ghc-boot-th","ghci"]
                         extra _ = mempty
                     assertEqual "Bundled4"
                       (missing ver, extra ver)
                       (Set.difference expected actual, Set.difference actual expected)
                 ]
