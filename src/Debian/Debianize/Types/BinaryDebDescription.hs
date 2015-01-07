{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS -Wall #-}
module Debian.Debianize.Types.BinaryDebDescription
    ( Canonical(canonical)
    , BinaryDebDescription
    , newBinaryDebDescription
    , package
    , description
    , packageType
    , architecture
    , binarySection
    , binaryPriority
    , essential
    , relations

    , PackageType(..)

    , PackageRelations
    , newPackageRelations
    , depends
    , recommends
    , suggests
    , preDepends
    , breaks
    , conflicts
    , provides
    , replaces
    , builtUsing
    ) where

import Data.Function (on)
import Data.Generics (Data, Typeable)
import Data.Lens.Template (makeLenses)
import Data.List (sort, sortBy)
import Data.Monoid (Monoid(..))
import Data.Text (Text)
import Debian.Policy (PackageArchitectures, PackagePriority, Section)
import Debian.Relation (BinPkgName, Relations)
import Prelude hiding ((.))

class Canonical a where
    canonical :: a -> a

-- | This type represents a section of the control file other than the
-- first, which in turn represent one of the binary packages or debs
-- produced by this debianization.
data BinaryDebDescription
    = BinaryDebDescription
      { _package :: BinPkgName
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Package>
      , _packageType :: Maybe PackageType
      , _architecture :: Maybe PackageArchitectures
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Architecture>
      , _binarySection :: Maybe Section
      , _binaryPriority :: Maybe PackagePriority
      , _essential :: Maybe Bool
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Essential>
      , _description :: Maybe Text
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Description>
      , _relations :: PackageRelations
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
      } deriving (Eq, Ord, Read, Show, Data, Typeable)

-- ^ The different types of binary debs we can produce from a haskell package
data PackageType
    = Development   -- ^ The libghc-foo-dev package.
    | Profiling     -- ^ The libghc-foo-prof package.
    | Documentation -- ^ The libghc-foo-doc package.
    | Exec          -- ^ A package related to a particular executable, perhaps
                    -- but not necessarily a server.
    | Utilities     -- ^ A package that holds the package's data files
                    -- and any executables not assigned to other
                    -- packages.
    | Source        -- ^ The source package (not a binary deb actually.)
    | HaskellSource -- ^ The source package of a haskell library (add
                    -- prefix haskell- to source package name.)
    | Cabal         -- ^ This is used to construct the value for
                    -- DEB_CABAL_PACKAGE in the rules file
    deriving (Eq, Ord, Show, Read, Data, Typeable)

-- ^ Package interrelationship information.
data PackageRelations
    = PackageRelations
      { _depends :: Relations
      , _recommends :: Relations
      , _suggests :: Relations
      , _preDepends :: Relations
      , _breaks :: Relations
      , _conflicts :: Relations
      , _provides :: Relations
      , _replaces :: Relations
      , _builtUsing :: Relations
      } deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Canonical [BinaryDebDescription] where
    canonical xs = sortBy (compare `on` _package) (map canonical xs)

instance Canonical BinaryDebDescription where
    canonical x = x {_relations = canonical (_relations x)}

instance Canonical PackageRelations where
    canonical x = x { _depends = canonical (_depends x)
                    , _recommends = canonical (_recommends x)
                    , _suggests = canonical (_suggests x)
                    , _preDepends = canonical (_preDepends x)
                    , _breaks = canonical (_breaks x)
                    , _conflicts = canonical (_conflicts x)
                    , _provides = canonical (_provides x)
                    , _replaces = canonical (_replaces x)
                    , _builtUsing = canonical (_builtUsing x) }

instance Canonical Relations where
    canonical xss = sort xss

newBinaryDebDescription :: BinPkgName -> BinaryDebDescription
newBinaryDebDescription name =
    BinaryDebDescription
      { _package = name
      , _packageType = Nothing
      , _architecture = Nothing
      , _binarySection = Nothing
      , _binaryPriority = Nothing
      , _essential = Nothing
      , _description = mempty
      , _relations = newPackageRelations }

newPackageRelations :: PackageRelations
newPackageRelations =
    PackageRelations
      { _depends = []
      , _recommends = []
      , _suggests = []
      , _preDepends = []
      , _breaks = []
      , _conflicts = []
      , _provides = []
      , _replaces = []
      , _builtUsing = [] }

$(makeLenses [''BinaryDebDescription, ''PackageRelations])
