{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.Debianize.DebianName
    ( debianName             -- Used in Debian.Debianize.Files and
    , mkPkgName
    , mkPkgName'
    , mapCabal
    , splitCabal
    ) where

import Data.Char (toLower)
import Data.Lens.Lazy (access)
import Data.Map as Map (lookup, alter)
import Data.Version (Version, showVersion)
import Debian.Debianize.Types.BinaryDebDescription as Debian (PackageType(..))
import Debian.Debianize.Types.Atoms as T (debianNameMap, packageDescription, compilerFlavor)
import Debian.Debianize.Monad (DebT)
import Debian.Debianize.Prelude ((%=))
import Debian.Debianize.VersionSplits (DebBase(DebBase), insertSplit, doSplits, VersionSplits, makePackage)
import Debian.Orphans ()
import Debian.Relation (PkgName(..), Relations)
import qualified Debian.Relation as D (VersionReq(EEQ))
import Debian.Version (parseDebianVersion)
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(PackageName))
import qualified Distribution.PackageDescription as Cabal
import Prelude hiding (unlines)

data Dependency_
  = BuildDepends Dependency
  | BuildTools Dependency
  | PkgConfigDepends Dependency
  | ExtraLibs Relations
    deriving (Eq, Show)

-- | Build the Debian package name for a given package type.
debianName :: (Monad m, PkgName name) => PackageType -> DebT m name
debianName typ =
    do cfl <- access compilerFlavor
       Just pkgDesc <- access packageDescription
       let pkgId = Cabal.package pkgDesc
       nameMap <- access T.debianNameMap
       return $ debianName' cfl (Map.lookup (pkgName pkgId) nameMap) typ pkgId

-- | Function that applies the mapping from cabal names to debian
-- names based on version numbers.  If a version split happens at v,
-- this will return the ltName if < v, and the geName if the relation
-- is >= v.
debianName' :: (PkgName name) => CompilerFlavor -> Maybe VersionSplits -> PackageType -> PackageIdentifier -> name
debianName' cfl msplits typ pkgId =
    case msplits of
      Nothing -> mkPkgName cfl pname typ
      Just splits -> (\ s -> mkPkgName' cfl s typ) $ doSplits splits version
    where
      -- def = mkPkgName cfl pname typ
      pname@(PackageName _) = pkgName pkgId
      version = (Just (D.EEQ (parseDebianVersion (showVersion (pkgVersion pkgId)))))

-- | Build a debian package name from a cabal package name and a
-- debian package type.  Unfortunately, this does not enforce the
-- correspondence between the PackageType value and the name type, so
-- it can return nonsense like (SrcPkgName "libghc-debian-dev").
mkPkgName :: PkgName name => CompilerFlavor -> PackageName -> PackageType -> name
mkPkgName cfl pkg typ = mkPkgName' cfl (debianBaseName pkg) typ

mkPkgName' :: PkgName name => CompilerFlavor -> DebBase -> PackageType -> name
mkPkgName' cfl (DebBase base) typ =
    pkgNameFromString $
             case typ of
                Documentation -> prefix ++ base ++ "-doc"
                Development -> prefix ++ base ++ "-dev"
                Profiling -> prefix ++ base ++ "-prof"
                Utilities -> "haskell-" ++ base ++ "-utils"
                Exec -> base
                Source' -> "haskell-" ++ base ++ ""
                Cabal -> base
    where prefix = "lib" ++ map toLower (show cfl) ++ "-"

debianBaseName :: PackageName -> DebBase
debianBaseName (PackageName name) =
    DebBase (map (fixChar . toLower) name)
    where
      -- Underscore is prohibited in debian package names.
      fixChar :: Char -> Char
      fixChar '_' = '-'
      fixChar c = toLower c

-- | Map all versions of Cabal package pname to Debian package dname.
-- Not really a debian package name, but the name of a cabal package
-- that maps to the debian package name we want.  (Should this be a
-- SrcPkgName?)
mapCabal :: Monad m => PackageName -> DebBase -> DebT m ()
mapCabal pname dname =
    debianNameMap %= Map.alter f pname
    where
      f :: Maybe VersionSplits -> Maybe VersionSplits
      f Nothing = Just (makePackage dname)
      f (Just sp) = error $ "mapCabal " ++ show pname ++ " " ++ show dname ++ ": - already mapped: " ++ show sp

-- | Map versions less than ver of Cabal Package pname to Debian package ltname
splitCabal :: Monad m => PackageName -> DebBase -> Version -> DebT m ()
splitCabal pname ltname ver =
    debianNameMap %= Map.alter f pname
    where
      f :: Maybe VersionSplits -> Maybe VersionSplits
      f Nothing = error $ "splitCabal - not mapped: " ++ show pname
      f (Just sp) = Just (insertSplit ver ltname sp)
