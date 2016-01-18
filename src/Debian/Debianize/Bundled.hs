-- | Determine whether a specific version of a Haskell package is
-- bundled with into this particular version of the given compiler.
-- This is done by getting the "Provides" field from the output of
-- "apt-cache showpkg ghc" (run in the appropriate changeroot) and
-- converting the debian package names back to Cabal package names.
-- *That* is done using the debianNameMap of CabalInfo, which is
-- built using the mapCabal, splitCabal, and remapCabal functions.

{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell #-}
module Debian.Debianize.Bundled
    ( builtIn
    -- * Utilities
    , aptCacheShowPkg
    , aptCacheProvides
    , aptCacheDepends
    , aptCacheConflicts
    , aptVersions
    , hcVersion
    ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (force, NFData)
import Control.Exception (SomeException, try)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans (MonadIO)
import Data.Char (toLower)
import Data.Function.Memoize (memoize2, memoize3)
import Data.List (groupBy, isPrefixOf, isSuffixOf)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Version (parseVersion, Version)
import Debian.GHC ()
import Debian.Relation (BinPkgName(..))
import Debian.Relation.ByteString ()
import Debian.Version (DebianVersion, parseDebianVersion', prettyDebianVersion)
import Debug.Trace (trace)
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.Simple.Compiler (CompilerFlavor(..))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess, showCommandForUser)
import System.Unix.Chroot (useEnv)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Regex.TDFA ((=~))

-- | Find out what version, if any, of a cabal library is built into
-- the newest version of haskell compiler hc in environment root.
-- This is done by looking for .conf files beneath a package.conf.d
-- directory and parsing the name.  (Probably better to actually read
-- the .conf file.)
builtIn :: CompilerFlavor -> FilePath -> [PackageIdentifier]
builtIn hc root =
  let Just hcname = (hcExecutablePath root hc >>= hcBinPkgName root) in
  aptCacheProvides hcname root

parseVersion' :: String -> Maybe Version
parseVersion' = listToMaybe . map fst . filter ((== "") . snd) . readP_to_S parseVersion

-- | Convert CompilerFlavor to an executable name in a way that works
-- for at least the cases we are interested in.  This might need to be
-- fudged or replaced as more cases become interesting.
hcExecutable :: CompilerFlavor -> String
hcExecutable = map toLower . show

-- | Use which(1) to determine full path name to haskell compiler executable
hcExecutablePath :: FilePath -> CompilerFlavor -> Maybe FilePath
hcExecutablePath = memoize2 $ \root hc ->
  listToMaybe $ lines $ unsafePerformIO $ chroot root (readProcess "which" [hcExecutable hc] "")

hcVersion :: FilePath -> CompilerFlavor -> Maybe Version
hcVersion root hc =
    let Just hcpath = hcExecutablePath root hc in
    maybe Nothing parseVersion' $
     listToMaybe $
     lines $
     unsafePerformIO . chroot root $
     readProcess hcpath
                 [case hc of
                    GHCJS -> "--numeric-ghc-version"
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
          trace ("aptCacheProvides " ++ show hcname ++ " in " ++ root ++ " -> " ++ show pis) pis
              where
                pis = (catMaybes .
                       map parseLib .
                       filter (isSuffixOf ".conf") .
                       map last .
                       filter (elem "package.conf.d") .
                       map (groupBy (\a b -> (a == '/') == (b == '/')))) lns
                lns = lines $ unsafePerformIO (chroot root (readProcess "dpkg" ["-L", unBinPkgName hcname] ""))
                parseLib :: String -> Maybe PackageIdentifier
                parseLib s =
                    case s =~ ("(.*)-([0-9.]*)-([a-f0-9]*).conf$") :: (String, String, String, [String]) of
                      (_, _, _, [cabalName, ver, _sum]) ->
                          case parseVersion' ver of
                            Just v -> Just (PackageIdentifier (PackageName cabalName) v)
                            _ -> Nothing
                      _ -> Nothing

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
    memoize2 (\ root hcname -> tr root hcname $ unsafePerformIO (try (chroot root (readProcess "apt-cache" ["showpkg", unBinPkgName hcname] ""))))
    where
      tr root hcname x  = trace ("aptCacheShowPkg " ++ show hcname ++ " in " ++ show root ++ " -> " ++ show x) x


chroot :: (NFData a, MonadIO m, MonadMask m) => String -> m a -> m a
chroot "/" = id
chroot root = useEnv root (return . force)
