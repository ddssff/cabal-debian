-- | Determine whether a specific version of a Haskell package is
-- bundled with into this particular version of the given compiler.
-- This is done by getting the "Provides" field from the output of
-- "apt-cache showpkg ghc" and
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

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Exception (SomeException, try)
import Data.Char (isAlphaNum, toLower)
import Data.Function.Memoize (memoize, memoize2)
import Data.List (groupBy, intercalate, isPrefixOf, stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set as Set (difference, fromList)
import Debian.GHC ({-instance Memoizable CompilerFlavor-})
import Debian.Relation (BinPkgName(..))
import Debian.Relation.ByteString ()
import Debian.Version (DebianVersion, parseDebianVersion', prettyDebianVersion)
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Package (mkPackageName, PackageIdentifier(..), unPackageName)
import Data.Version (parseVersion)
import Distribution.Version(mkVersion, mkVersion', Version)
#else
import Data.Version (parseVersion, Version(..))
import Distribution.Package (PackageIdentifier(..), PackageName(..))
#endif
#if MIN_VERSION_Cabal(1,22,0)
import Distribution.Simple.Compiler (CompilerFlavor(GHCJS))
#else
import Distribution.Compiler (CompilerFlavor)
#endif
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess, showCommandForUser)
import Test.HUnit (assertEqual, Test(TestList, TestCase))
import Text.ParserCombinators.ReadP (char, endBy1, munch1, ReadP, readP_to_S)
import Text.Regex.TDFA ((=~))

#if MIN_VERSION_base(4,8,0)
#if !MIN_VERSION_Cabal(2,0,0)
import Data.Version (makeVersion)
#else
#endif
#else
import Data.Monoid (mempty)

#if !MIN_VERSION_Cabal(1,22,0)
unPackageName :: PackageName -> String
unPackageName (PackageName s) = s
#endif

makeVersion :: [Int] -> Version
makeVersion ns = Version ns []
#endif

-- | Find out what version, if any, of a cabal library is built into
-- the newest version of haskell compiler hc in environment root.
-- This is done by looking for .conf files beneath a package.conf.d
-- directory and parsing the name.  (Probably better to actually read
-- the .conf file.)
builtIn :: CompilerFlavor -> [PackageIdentifier]
builtIn hc =
  let Just hcname = (hcExecutablePath hc >>= hcBinPkgName) in
  aptCacheProvides hcname

-- | Convert CompilerFlavor to an executable name in a way that works
-- for at least the cases we are interested in.  This might need to be
-- fudged or replaced as more cases become interesting.
hcExecutable :: CompilerFlavor -> String
hcExecutable = map toLower . show

-- | Use which(1) to determine full path name to haskell compiler executable
hcExecutablePath :: CompilerFlavor -> Maybe FilePath
hcExecutablePath = memoize $ \hc ->
  listToMaybe $ lines $ unsafePerformIO $ readProcess "which" [hcExecutable hc] ""

hcVersion :: CompilerFlavor -> Maybe Version
hcVersion hc =
    let Just hcpath = hcExecutablePath hc in
    maybe Nothing parseVersion' $
     listToMaybe $
     lines $
     unsafePerformIO $
     readProcess hcpath
                 [case hc of
#if MIN_VERSION_Cabal(1,22,0)
                    GHCJS -> "--numeric-ghc-version"
#endif
                    _ -> "--numeric-version"]
                 ""

-- | Use dpkg -S to convert the executable path to a debian binary
-- package name.
hcBinPkgName :: FilePath -> Maybe BinPkgName
hcBinPkgName = memoize $ \path ->
  let s = unsafePerformIO (readProcess "dpkg" ["-S", path] "") in
  case map (takeWhile (/= ':')) (lines s) of
    [] -> Nothing
    [name] -> Just (BinPkgName name)
    _ -> error $ "Unexpected output from " ++ showCommandForUser "dpkg" ["-S", path] ++ ": ++ " ++ show s

-- | What built in libraries does this haskell compiler provide?
aptCacheProvides :: BinPkgName -> [PackageIdentifier]
aptCacheProvides = memoize aptCacheProvides'
    where
      aptCacheProvides' hcname =
        {-trace ("aptCacheProvides " ++ show hcname ++ " -> " ++ show pis)-}
          packageIdentifiers hcname

packageIdentifiers :: BinPkgName -> [PackageIdentifier]
packageIdentifiers hcname =
    (mapMaybe parsePackageIdentifier' .
     mapMaybe (dropRequiredSuffix ".conf") .
     map last .
     filter (elem "package.conf.d") .
     map (groupBy (\a b -> (a == '/') == (b == '/')))) (binPkgFiles hcname)

dropRequiredSuffix :: String -> String -> Maybe String
dropRequiredSuffix suff x =
    let (x', suff') = splitAt (length x - length suff) x in if suff == suff' then Just x' else Nothing

-- | A list of the files in a binary deb
binPkgFiles :: BinPkgName -> [FilePath]
binPkgFiles hcname = lines $ unsafePerformIO (readProcess "dpkg" ["-L", unBinPkgName hcname] "")

{-
takeBetween :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeBetween startPred endPred = takeWhile (not . endPred) . dropWhile startPred . dropWhile (not . startPred)
-}

aptCacheConflicts :: String -> DebianVersion -> [BinPkgName]
aptCacheConflicts hcname ver =
    either (\ _ -> []) (mapMaybe doLine . lines) (aptCacheDepends hcname (show (prettyDebianVersion ver)))
    where
      doLine s = case s =~ "^[ ]*Conflicts:[ ]*<(.*)>$" :: (String, String, String, [String]) of
                   (_, _, _, [name]) -> Just (BinPkgName name)
                   _ -> Nothing

aptCacheDepends :: String -> String -> Either SomeException String
aptCacheDepends =
    memoize2 (\hcname ver -> unsafePerformIO (try (readProcess "apt-cache" ["depends", hcname ++ "=" ++ ver] "")))

aptVersions :: BinPkgName -> [DebianVersion]
aptVersions hcname =
    either (\ _ -> []) (map parseDebianVersion' . filter (/= "") . map (takeWhile (/= ' ')) . takeWhile (not . isPrefixOf "Reverse Depends:") . drop 1 . dropWhile (not . isPrefixOf "Versions:") . lines) (aptCacheShowPkg hcname)

aptCacheShowPkg :: BinPkgName -> Either SomeException String
aptCacheShowPkg =
    memoize (\hcname -> unsafePerformIO (try (readProcess "apt-cache" ["showpkg", unBinPkgName hcname] "")))

-- | A package identifier is a package name followed by a dash and
-- then a version number.  A package name, according to the cabal
-- users guide "can use letters, numbers and hyphens, but not spaces."
-- So be it.
parsePackageIdentifier :: ReadP PackageIdentifier
parsePackageIdentifier = do
#if MIN_VERSION_Cabal(2,0,0)
  makeId <$> ((,) <$> endBy1 (munch1 isAlphaNum) (char '-') <*> parseCabalVersion)
    where
      makeId :: ([String], Version) -> PackageIdentifier
      makeId (xs, v) = PackageIdentifier {pkgName = mkPackageName (intercalate "-" xs), pkgVersion = v}
#else
  makeId <$> ((,) <$> endBy1 (munch1 isAlphaNum) (char '-') <*> parseVersion)
    where
      makeId :: ([String], Version) -> PackageIdentifier
      makeId (xs, v) = PackageIdentifier {pkgName = PackageName (intercalate "-" xs), pkgVersion = v}
#endif

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe p = listToMaybe . map fst . filter ((== "") . snd) . readP_to_S p

parseVersion' :: String -> Maybe Version
#if MIN_VERSION_Cabal(2,0,0)
parseVersion' = parseMaybe parseCabalVersion

parseCabalVersion :: ReadP Version
parseCabalVersion = fmap mkVersion' parseVersion
#else
parseVersion' = parseMaybe parseVersion
#endif

parsePackageIdentifier' :: String -> Maybe PackageIdentifier
parsePackageIdentifier' = parseMaybe parsePackageIdentifier

tests :: Test
tests = TestList [ TestCase (assertEqual "Bundled1"
#if MIN_VERSION_Cabal(2,0,0)
                               (Just (PackageIdentifier (mkPackageName "HUnit") (mkVersion [1,2,3])))
#else
                               (Just (PackageIdentifier (PackageName "HUnit") (makeVersion [1,2,3])))
#endif
                               (parseMaybe parsePackageIdentifier "HUnit-1.2.3"))
                 , TestCase (assertEqual "Bundled2"
                               Nothing
                               (parseMaybe parsePackageIdentifier "HUnit-1.2.3 "))
                 , TestCase $ do
                     ghc <- (head . lines) <$> readProcess "which" ["ghc"] ""
                     let ver = fmap (takeWhile (/= '/')) (stripPrefix "/opt/ghc/" ghc)
                     let expected =
                             (Set.fromList
                                -- This is the package list for ghc-7.10.3
                                ["array", "base", "binary", "bin-package-db", "bytestring", "Cabal",
                                 "containers", "deepseq", "directory", "filepath", "ghc", "ghc-prim",
                                 "haskeline", "hoopl", "hpc", "integer-gmp", "pretty", "process",
                                 "template-haskell", "terminfo", "time", "transformers", "unix", "xhtml"])
                         actual = Set.fromList (map (unPackageName . pkgName) (aptCacheProvides (BinPkgName ("ghc" ++ maybe "" ("-" ++) ver))))
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
