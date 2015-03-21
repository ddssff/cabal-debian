-- | This module holds a long list of lenses that access the Atoms
-- record, the record that holds the input data from which the
-- debianization is to be constructed.
{-# LANGUAGE CPP, DeriveDataTypeable, OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.CabalInfo
    ( -- * Types
      CabalInfo
    , PackageInfo(PackageInfo, cabalName, devDeb, docDeb, profDeb)
      -- * Lenses
    , packageDescription
    , debInfo
    , debianNameMap
    , epochMap
    , packageInfo
      -- * Builder
    , newCabalInfo
    ) where

import OldLens (access)

import Control.Category ((.))
import Control.Lens.TH (makeLenses)
import Control.Monad (unless, when)
import Control.Monad.State (execStateT)
import Control.Monad.Trans (liftIO)
import Data.Generics (Data, Typeable)
import Data.List as List (null)
import Data.Map as Map (Map)
import Data.Monoid (Monoid(..))
import Data.Text as Text (null, pack, strip)
import Debian.Debianize.BasicInfo (Flags)
import Debian.Debianize.DebInfo as D (control, copyright, DebInfo, makeDebInfo, noTestSuite, noTestSuiteRun, rulesSettings)
import Debian.Debianize.BinaryDebDescription (Canonical(canonical))
import Debian.Debianize.CopyrightDescription (defaultCopyrightDescription)
import Debian.Debianize.InputCabal (inputCabalization)
import Debian.Debianize.Prelude ((~=), (%=))
import Debian.Debianize.SourceDebDescription as S (homepage)
import Debian.Debianize.VersionSplits (VersionSplits)
import Debian.Orphans ()
import Debian.Relation (BinPkgName)
import Debian.Version (DebianVersion)
import Distribution.Package (PackageName)
import Distribution.PackageDescription as Cabal (PackageDescription(homepage, testSuites))
import Prelude hiding ((.), init, init, log, log, null)

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
data CabalInfo
    = CabalInfo
      { _packageDescription :: PackageDescription
      -- ^ The result of reading a cabal configuration file.
      , _debInfo :: DebInfo
      -- ^ Information required to represent a non-cabal debianization.
      , _debianNameMap :: Map PackageName VersionSplits
      -- ^ Mapping from cabal package name and version to debian source
      -- package name.  This allows different ranges of cabal versions to
      -- map to different debian source package names.
      , _epochMap :: Map PackageName Int
      -- ^ Specify epoch numbers for the debian package generated from a
      -- cabal package.  Example: @EpochMapping (PackageName "HTTP") 1@.
      , _packageInfo :: Map PackageName PackageInfo
      -- ^ Supply some info about a cabal package.
      } deriving (Show, Data, Typeable)

data PackageInfo = PackageInfo { cabalName :: PackageName
                               , devDeb :: Maybe (BinPkgName, DebianVersion)
                               , profDeb :: Maybe (BinPkgName, DebianVersion)
                               , docDeb :: Maybe (BinPkgName, DebianVersion) } deriving (Eq, Ord, Show, Data, Typeable)

$(makeLenses ''CabalInfo)

instance Canonical CabalInfo where
    canonical x = x {_debInfo = canonical (_debInfo x)}

-- | Given the 'Flags' value read the cabalization and build a new
-- 'CabalInfo' record.
newCabalInfo :: Flags -> IO CabalInfo
newCabalInfo flags' = do
  pkgDesc <- inputCabalization flags'
  copyrt <- liftIO $ defaultCopyrightDescription pkgDesc
  execStateT
    (do (debInfo . copyright) ~= Just copyrt
        (debInfo . control . S.homepage) ~= case strip (pack (Cabal.homepage pkgDesc)) of
                                              x | Text.null x -> Nothing
                                              x -> Just x
        noTests <- access (debInfo . noTestSuite)
        unless (List.null (Cabal.testSuites pkgDesc) || noTests)
               (do (debInfo . rulesSettings) %= (++ ["DEB_ENABLE_TESTS = yes"])
                   noRun <- access (debInfo . noTestSuiteRun)
                   when noRun $ (debInfo . rulesSettings) %= (++ ["DEB_BUILD_OPTIONS += nocheck"])
               ))
    (makeCabalInfo flags' pkgDesc)

makeCabalInfo :: Flags -> PackageDescription -> CabalInfo
makeCabalInfo fs pkgDesc =
    CabalInfo
      { _packageDescription = pkgDesc
      , _epochMap = mempty
      , _packageInfo = mempty
      , _debianNameMap = mempty
      , _debInfo = makeDebInfo fs
      }
