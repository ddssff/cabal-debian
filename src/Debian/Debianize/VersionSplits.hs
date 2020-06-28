-- | Convert between cabal and debian package names based on version
-- number ranges.
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
module Debian.Debianize.VersionSplits
    ( DebBase(DebBase, unDebBase)
    -- * Combinators for VersionSplits
    , VersionSplits(..)
    , makePackage
    , insertSplit
    -- * Operators on VersionSplits
    , cabalFromDebian
    , cabalFromDebian'
    , debianFromCabal
    , packageRangesFromVersionSplits
    , doSplits
    ) where

import Data.Generics (Data, Typeable)
import Data.Map as Map (elems, Map, mapMaybeWithKey)
import Data.Set as Set (fromList, Set, toList)
import Debian.Debianize.Interspersed (foldTriples, Interspersed(leftmost, pairs, foldInverted))
import Debian.Orphans ()
import qualified Debian.Relation as D (VersionReq(..))
import Debian.Version (DebianVersion, parseDebianVersion')
import Distribution.Package (PackageIdentifier(..), PackageName)
import Distribution.Package (mkPackageName)
import Distribution.Pretty (prettyShow)
import Distribution.Version (Version)
import Distribution.Version (anyVersion, earlierVersion, intersectVersionRanges, orLaterVersion, VersionRange)
import Prelude hiding (init, log, unlines)

-- | The base of a debian binary package name, the string that appears
-- between "libghc-" and "-dev".
newtype DebBase = DebBase {unDebBase :: String} deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | Describes a mapping from cabal package name and version to debian
-- package names.  For example, versions of the cabal QuickCheck
-- package less than 2 are mapped to "quickcheck1", while version 2 or
-- greater is mapped to "quickcheck2".
data VersionSplits
    = VersionSplits {
        oldestPackage :: DebBase
      -- ^ The Debian name given to versions older than the oldest split.
      , splits :: [(Version, DebBase)]
      -- ^ Each pair is The version where the split occurs, and the
      -- name to use for versions greater than or equal to that
      -- version.  This list assumed to be in (must be kept in)
      -- descending version number order, newest to oldest
      } deriving (Eq, Ord, Data, Typeable)

instance Show VersionSplits where
    show s = foldr (\ (v, b) r -> ("insertSplit (" ++ show v ++ ") (" ++ show b ++ ") (" ++ r ++ ")")) ("makePackage (" ++ show (oldestPackage s) ++ ")") (splits s)

instance Interspersed VersionSplits DebBase Version where
    leftmost (VersionSplits {oldestPackage = p}) = p
    pairs (VersionSplits {splits = xs}) = xs

-- | Create a version split database that assigns a single debian
-- package name base to all cabal versions.
makePackage :: DebBase -> VersionSplits
makePackage name = VersionSplits {oldestPackage = name, splits = []}

-- | Split the version range and give the older packages a new name.
insertSplit :: Version -- ^ Where to split the version range
            -> DebBase -- ^ The name to use for versions older than the split
            -> VersionSplits
            -> VersionSplits
insertSplit ver ltname sp@(VersionSplits {}) =
    -- (\ x -> trace ("insertSplit " ++ show (ltname, ver, sp) ++ " -> " ++ show x) x) $
    case splits sp of
      -- This is the oldest split, change oldestPackage and insert a new head pair
      (ver', _) : _ | ver' > ver -> sp {oldestPackage = ltname, splits = (ver, oldestPackage sp) : splits sp}
      (ver', name) : _ | ver' == ver && name == ltname -> sp
      [] -> sp {oldestPackage = ltname, splits = [(ver, oldestPackage sp)]}
      -- Not the oldest split, insert it in its proper place.
      _ -> sp {splits = reverse (insert (reverse (splits sp)))}
    where
      -- Insert our new split into the reversed list
      insert ((ver', name') : more) =
          if ver' < ver
          then (ver, name') : (ver', ltname) : more
          else (ver', name') : insert more
      -- ver' is older, change oldestPackage
      insert [] = [(ver, oldestPackage sp)]
      -- ltname = base ++ "-" ++ (show (last ns - 1))

packageRangesFromVersionSplits :: VersionSplits -> [(DebBase, VersionRange)]
packageRangesFromVersionSplits s =
    foldInverted (\ older dname newer more ->
                      (dname, intersectVersionRanges (maybe anyVersion orLaterVersion older) (maybe anyVersion earlierVersion newer)) : more)
                 []
                 s

debianFromCabal :: VersionSplits -> PackageIdentifier -> DebBase
debianFromCabal s p =
    doSplits s (Just (D.EEQ debVer))
    where debVer = parseDebianVersion' (prettyShow (pkgVersion p))

cabalFromDebian' :: Map PackageName VersionSplits -> DebBase -> Version -> PackageIdentifier
cabalFromDebian' mp base ver =
    PackageIdentifier (cabalFromDebian mp base dver) ver
    where dver = parseDebianVersion' (prettyShow ver)

-- | Brute force implementation - I'm assuming this is not a huge map.
cabalFromDebian :: Map PackageName VersionSplits -> DebBase -> DebianVersion -> PackageName
cabalFromDebian mp base@(DebBase name) ver =
    case Set.toList pset of
      [x] -> x
      [] -> mkPackageName name
      l -> error $ "Error, multiple cabal package names associated with " ++ show base ++ ": " ++ show l
    where
      -- Look for splits that involve the right DebBase and return the
      -- associated Cabal package name.  It is unlikely that more than
      -- one Cabal name will be returned - if so throw an exception.
      pset :: Set PackageName
      pset = Set.fromList $ Map.elems $
             Map.mapMaybeWithKey
                (\ p s -> if doSplits s (Just (D.EEQ ver)) == base then Just p else Nothing)
                mp

-- | Given a version split database, turn the debian version
-- requirements into a debian package name base that ought to satisfy
-- them.
doSplits :: VersionSplits -> Maybe D.VersionReq -> DebBase
doSplits s version =
    foldTriples' (\ ltName v geName _ ->
                           let split = parseDebianVersion' (prettyShow v) in
                                case version of
                                  Nothing -> geName
                                  Just (D.SLT v') | v' <= split -> ltName
                                  -- Otherwise use ltName only when the split is below v'
                                  Just (D.EEQ v') | v' < split -> ltName
                                  Just (D.LTE v') | v' < split -> ltName
                                  Just (D.GRE v') | v' < split -> ltName
                                  Just (D.SGR v') | v' < split -> ltName
                                  _ -> geName)
                 (oldestPackage s)
                 s
    where
      foldTriples' :: (DebBase -> Version -> DebBase -> DebBase -> DebBase) -> DebBase -> VersionSplits -> DebBase
      foldTriples' = foldTriples
