-- | Convert between cabal and debian package names based on version
-- number ranges.
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
module Debian.Debianize.VersionSplits
    ( DebBase(DebBase)
    -- * Combinators for VersionSplits
    , VersionSplits
    , makePackage
    , insertSplit
    -- * Operators on VersionSplits
    , cabalFromDebian
    , cabalFromDebian'
    , debianFromCabal
    , packageRangesFromVersionSplits
    , doSplits
    ) where

import Data.Map as Map (Map, mapMaybeWithKey, elems)
import Data.Set as Set (Set, toList, fromList)
import Data.Version (Version(Version), showVersion)
import Debian.Debianize.Interspersed (Interspersed(leftmost, pairs, foldInverted), foldTriples)
import Debian.Orphans ()
import qualified Debian.Relation as D
import Debian.Version (DebianVersion, parseDebianVersion)
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.Version (VersionRange, anyVersion, intersectVersionRanges, earlierVersion, orLaterVersion)
import Prelude hiding (init, unlines, log)

-- | The base of a debian binary package name, the string that appears
-- between "libghc-" and "-dev".
newtype DebBase = DebBase String deriving (Eq, Ord, Read, Show)

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
      -- ascending version number order.
      } deriving (Eq, Ord)

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
insertSplit :: Version -> DebBase -> VersionSplits -> VersionSplits
insertSplit ver@(Version _ _) ltname sp@(VersionSplits {}) =
    -- (\ x -> trace ("insertSplit " ++ show (ltname, ver, sp) ++ " -> " ++ show x) x) $
    case splits sp of
      -- This is the oldest split, change oldestPackage and insert a new head pair
      (ver', _) : _ | ver' > ver -> sp {oldestPackage = ltname, splits = (ver, oldestPackage sp) : splits sp}
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
    where debVer = parseDebianVersion (showVersion (pkgVersion p))

cabalFromDebian' :: Map PackageName VersionSplits -> DebBase -> Version -> PackageIdentifier
cabalFromDebian' mp base ver =
    PackageIdentifier (cabalFromDebian mp base dver) ver
    where dver = parseDebianVersion (showVersion ver)

-- | Brute force implementation - I'm assuming this is not a huge map.
cabalFromDebian :: Map PackageName VersionSplits -> DebBase -> DebianVersion -> PackageName
cabalFromDebian mp base@(DebBase name) ver =
    case Set.toList pset of
      [x] -> x
      [] -> PackageName name
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
                           let split = parseDebianVersion (showVersion v) in
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
