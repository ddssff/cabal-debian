-- | This module holds a long list of lenses that access the Atoms
-- record, the record that holds the input data from which the
-- debianization is to be constructed.
{-# LANGUAGE CPP, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.Atoms
    ( Atoms
    , newAtoms
    -- , makeAtoms
    , PackageInfo(PackageInfo, cabalName, devDeb, docDeb, profDeb)
    , debianNameMap
    , debInfo
    , packageDescription
    , epochMap
    , packageInfo
    , showAtoms
    ) where

import Data.Generics (Data, Typeable)
import Data.Lens.Template (nameMakeLens)
import Data.List (init)
import Data.Map as Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set as Set (Set)
import Data.Text (Text)
import Debian.Debianize.DebInfo (DebInfo, makeDebInfo)
import Debian.Debianize.InputCabalPackageDescription (Flags, inputCabalization)
import Debian.Debianize.BinaryDebDescription (Canonical(canonical))
import Debian.Debianize.VersionSplits (DebBase, VersionSplits)
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures, PackagePriority, Section)
import Debian.Relation (BinPkgName, Relations, SrcPkgName)
import Debian.Version (DebianVersion)
import Distribution.Package (PackageName)
import Distribution.PackageDescription as Cabal (PackageDescription)
import Prelude hiding ((.), init, init, log, log)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

-- This enormous record is a mistake - instead it should be an Atom
-- type with lots of constructors, and the Atoms type is a set of
-- these.  Then we can cruise through the atom set converting the
-- elements into other simpler elements until they elements are all
-- simple enough to convert directly into a debianization.  At the
-- moment I really need this for the Install atoms, so I will try to
-- convert just that portion of the type to this new scheme.

-- | Bits and pieces of information about the mapping from cabal package
-- names and versions to debian package names and versions.  In essence,
-- an 'Atoms' value represents a package's debianization.  The lenses in
-- this module are used to get and set the values hidden in this Atoms
-- value.  Many of the values should be left alone to be set when the
-- debianization is finalized.
data Atoms
    = Atoms
      { packageDescription_ :: PackageDescription
      -- ^ The result of reading a cabal configuration file.
      , debInfo_ :: DebInfo
      -- ^ Information required to represent a non-cabal debianization.
      , debianNameMap_ :: Map PackageName VersionSplits
      -- ^ Mapping from cabal package name and version to debian source
      -- package name.  This allows different ranges of cabal versions to
      -- map to different debian source package names.
      , epochMap_ :: Map PackageName Int
      -- ^ Specify epoch numbers for the debian package generated from a
      -- cabal package.  Example: @EpochMapping (PackageName "HTTP") 1@.
      , packageInfo_ :: Map PackageName PackageInfo
      -- ^ Supply some info about a cabal package.
      } deriving (Show, Data, Typeable)

instance Canonical Atoms where
    canonical x = x {debInfo_ = canonical (debInfo_ x)}

newAtoms :: Flags -> IO Atoms
newAtoms flags' = do
  pkgDesc <- inputCabalization flags'
  return $ makeAtoms flags' pkgDesc

makeAtoms :: Flags -> PackageDescription -> Atoms
makeAtoms fs pkgDesc =
    Atoms
      { packageDescription_ = pkgDesc
      , epochMap_ = mempty
      , packageInfo_ = mempty
      , debianNameMap_ = mempty
      , debInfo_ = makeDebInfo fs
      }

data PackageInfo = PackageInfo { cabalName :: PackageName
                               , devDeb :: Maybe (BinPkgName, DebianVersion)
                               , profDeb :: Maybe (BinPkgName, DebianVersion)
                               , docDeb :: Maybe (BinPkgName, DebianVersion) } deriving (Eq, Ord, Show, Data, Typeable)

showAtoms :: Atoms -> IO ()
showAtoms x = putStrLn ("\nTop: " ++ show x ++ "\n")

$(let f s = case s of
              (_ : _) | last s == '_' -> Just (init s)
              _ -> Nothing in
  nameMakeLens ''Atoms f)
