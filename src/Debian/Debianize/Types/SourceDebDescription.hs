{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TemplateHaskell, TypeSynonymInstances #-}
module Debian.Debianize.Types.SourceDebDescription
    ( SourceDebDescription
    , newSourceDebDescription
    , newSourceDebDescription'
    , source
    , maintainer
    , changedBy
    , uploaders
    , dmUploadAllowed
    , priority
    , section
    , buildDepends
    , buildConflicts
    , buildDependsIndep
    , buildConflictsIndep
    , standardsVersion
    , homepage
    , vcsFields
    , xFields
    , xDescription
    , binaryPackages
    , VersionControlSpec(..)
    , XField(..)
    , XFieldDest(..)
    ) where

import Data.Generics (Data, Typeable)
import Data.Lens.Template (makeLenses)
import Data.Set as Set (empty, Set)
import Data.Text (Text)
import Debian.Debianize.Types.BinaryDebDescription (Canonical(canonical), BinaryDebDescription)
import Debian.Orphans ()
import Debian.Policy (PackagePriority, Section, StandardsVersion)
import Debian.Relation (Relations, SrcPkgName)
import Prelude hiding (init, init, log, log, unlines)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

-- | This type represents the debian/control file, which is the core
-- of the source package debianization.  It includes the information
-- that goes in the first, or source, section, and then a list of the
-- succeeding binary package sections.
data SourceDebDescription
    = SourceDebDescription
      { _source :: Maybe SrcPkgName
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Source>
      , _maintainer :: Maybe NameAddr
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Maintainer>
      , _changedBy :: Maybe NameAddr
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Changed-By>
      , _uploaders :: [NameAddr]
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Uploaders>
      , _dmUploadAllowed :: Bool
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-DM-Upload-Allowed>
      , _priority :: Maybe PackagePriority
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Priority>
      , _section :: Maybe Section
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Section>
      , _standardsVersion :: Maybe StandardsVersion
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Standards-Version>
      , _homepage :: Maybe Text
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Homepage>
      , _vcsFields :: Set VersionControlSpec
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-VCS-fields>
      , _xFields :: Set XField
      -- ^ <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.7>
      , _buildDepends :: Relations
      , _buildConflicts :: Relations
      , _buildDependsIndep :: Relations
      , _buildConflictsIndep :: Relations
      , _xDescription :: Maybe Text
      , _binaryPackages :: [BinaryDebDescription]
      -- ^ The binary debs.  This should be a map, but we may need to preserve the order
      } deriving (Eq, Ord, Show, Data, Typeable)

instance Canonical SourceDebDescription where
    canonical x = x { _binaryPackages = canonical (_binaryPackages x)
                    , _buildDepends = canonical (_buildDepends x)
                    , _buildConflicts = canonical (_buildConflicts x)
                    , _buildDependsIndep = canonical (_buildDependsIndep x)
                    , _buildConflictsIndep = canonical (_buildConflictsIndep x)
                    }

newSourceDebDescription :: SourceDebDescription
newSourceDebDescription =
    SourceDebDescription
      { _source = Nothing
      , _maintainer = Nothing
      , _changedBy = Nothing
      , _uploaders = []
      , _dmUploadAllowed = False
      , _priority = Nothing
      , _section = Nothing
      , _buildDepends = []
      , _buildConflicts = []
      , _buildDependsIndep  = []
      , _buildConflictsIndep  = []
      , _standardsVersion = Nothing
      , _homepage = Nothing
      , _vcsFields = Set.empty
      , _xFields = Set.empty
      , _xDescription = Nothing -- Quick hack, I should maybe put this into _xFields
      , _binaryPackages = [] }

newSourceDebDescription' :: SrcPkgName -> NameAddr -> SourceDebDescription
newSourceDebDescription' src who =
    newSourceDebDescription
      { _source = Just src
      , _maintainer = Just who }

data VersionControlSpec
    = VCSBrowser Text
    | VCSArch Text
    | VCSBzr Text
    | VCSCvs Text
    | VCSDarcs Text
    | VCSGit Text
    | VCSHg Text
    | VCSMtn Text
    | VCSSvn Text
    deriving (Eq, Ord, Show, Data, Typeable)

-- | User defined fields.  Parse the line "XBS-Comment: I stand
-- between the candle and the star." to get XField (fromList "BS")
-- "Comment" " I stand between the candle and the star."
data XField
    = XField (Set XFieldDest) Text Text
    deriving (Eq, Ord, Show, Data, Typeable)

data XFieldDest
    = B -- ^ Field will be copied to the binary packgae control files
    | S -- ^ Field will be copied to the source packgae control files
    | C -- ^ Field will be copied to the upload control (.changes) file
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(makeLenses [''SourceDebDescription])
