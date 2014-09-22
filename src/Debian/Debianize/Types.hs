{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Debian.Debianize.Types
    (
    -- * Modes of operation
      verbosity
    , dryRun
    , debAction
    , cabalFlagAssignments

    -- * Cabal package info
    , packageDescription

    -- * Repository info
    , execMap
    , epochMap
    , missingDependencies
    , extraLibMap
    , debianNameMap

    -- * Source Package Info
    , sourcePackageName
    , revision
    , debVersion
    , maintainer
    , copyright
    , license
    , licenseFile
    , sourceArchitectures
    , sourcePriority
    , sourceSection
    , compat
    , sourceFormat
    , changelog
    , comments
    , standardsVersion
    , rulesHead
    , rulesFragments
    , official
    , noProfilingLibrary
    , noDocumentationLibrary
    , utilsPackageNameBase
    , buildDir
    , watch
    , xDescription

    -- * Source Package Build Dependencies
    , omitLTDeps
    -- , compilerVersion

    -- * Binary Package Info
    , binaryArchitectures
    , executable
    , serverInfo -- askServers = serverInfo
    , website
    , backups
    , apacheSite
    , extraDevDeps
    , postInst
    , postRm
    , preInst
    , preRm
    , binaryPriority
    , binarySection
    , installInit
    , packageType
    , debianDescription
    , essential

    , depends
    , recommends
    , suggests
    , preDepends
    , breaks
    , conflicts
    , provides
    , replaces
    , builtUsing

{-
    -- * Binary Package Dependencies
    , depends
    , conflicts
    , replaces
    , provides
-}

    -- * Binary package Files
    , link
    , install
    , installTo
    , installData
    , file
    , installCabalExec
    , installCabalExecTo
    , installDir
    , logrotateStanza

    -- * Unknown, obsolete, internal
    , flags
    , validate
    , warning -- no-op?
    , intermediateFiles
    , packageInfo
    , control -- obsolete
    , source
    , changedBy
    , uploaders
    , dmUploadAllowed
    , homepage
    , vcsFields
    , xFields
    , buildDepends
    , buildConflicts
    , buildDependsIndep
    , buildConflictsIndep
    , binaryPackages
    ) where

import Control.Category ((.))
import Data.Lens.Lazy (Lens, iso, getL)
import Data.Set as Set (Set)
import Data.Text (Text)
import Debian.Debianize.Prelude (maybeLens, listElemLens)
import Debian.Debianize.Types.Atoms
import qualified Debian.Debianize.Types.BinaryDebDescription as B
import qualified Debian.Debianize.Types.SourceDebDescription as S
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures, PackagePriority, Section, StandardsVersion)
import Debian.Relation (BinPkgName, Relations, SrcPkgName)
import Prelude hiding (init, init, log, log, unlines, (.))
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

-- | Not exported - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Package>
binaryDebDescription :: BinPkgName -> Lens Atoms B.BinaryDebDescription
binaryDebDescription b = maybeLens (B.newBinaryDebDescription b) (iso id id) . listElemLens ((== b) . getL B.package) . S.binaryPackages . control

-- | Lens onto one of several 'B.PackageType' values of which we have
-- specific knowledge how to package.
packageType :: BinPkgName -> Lens Atoms (Maybe B.PackageType)
packageType b = B.packageType . binaryDebDescription b

-- | Lens into the description field of a BinaryDebDescription.
debianDescription :: BinPkgName -> Lens Atoms (Maybe Text)
debianDescription b = B.description . binaryDebDescription b

-- | <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Essential>
essential :: BinPkgName -> Lens Atoms (Maybe Bool)
essential b = B.essential . binaryDebDescription b

-- | <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
-- relations :: BinPkgName -> Lens Atoms B.PackageRelations
-- relations b = B.relations . binaryDebDescription b

-- | The Depends: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
depends :: BinPkgName -> Lens Atoms Relations
depends b = B.depends . B.relations . binaryDebDescription b

-- | The Recommends: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
recommends :: BinPkgName -> Lens Atoms Relations
recommends b = B.recommends . B.relations . binaryDebDescription b

-- | The Suggests: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
suggests :: BinPkgName -> Lens Atoms Relations
suggests b = B.suggests . B.relations . binaryDebDescription b

-- | The Pre-Depends: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
preDepends :: BinPkgName -> Lens Atoms Relations
preDepends b = B.preDepends . B.relations . binaryDebDescription b

-- | The Breaks: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
breaks :: BinPkgName -> Lens Atoms Relations
breaks b = B.breaks . B.relations . binaryDebDescription b

-- | The Conflicts: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
conflicts :: BinPkgName -> Lens Atoms Relations
conflicts b = B.conflicts . B.relations . binaryDebDescription b

-- | The Provides: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
provides :: BinPkgName -> Lens Atoms Relations
provides b = B.provides . B.relations . binaryDebDescription b

-- | The Replaces: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
replaces :: BinPkgName -> Lens Atoms Relations
replaces b = B.replaces . B.relations . binaryDebDescription b

-- | THe Built-Using: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
builtUsing :: BinPkgName -> Lens Atoms Relations
builtUsing b = B.builtUsing . B.relations . binaryDebDescription b

-- | Maintainer field.  Overrides any value found in the cabal file, or
-- in the DEBIANMAINTAINER environment variable.
-- <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Maintainer>
maintainer :: Lens Atoms (Maybe NameAddr)
maintainer = S.maintainer . control

-- | The architectures supported by a binary package
binaryArchitectures :: BinPkgName -> Lens Atoms (Maybe PackageArchitectures)
binaryArchitectures b = B.architecture . binaryDebDescription b

-- | The source package priority - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Priority>
sourcePriority :: Lens Atoms (Maybe PackagePriority)
sourcePriority = S.priority . control

-- | Map of the binary package priorities (FIXME: redundant with BinaryDebDescription)
binaryPriority :: BinPkgName -> Lens Atoms (Maybe PackagePriority)
binaryPriority b = B.binaryPriority . binaryDebDescription b

-- | The source package's section assignment - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Section>
sourceSection :: Lens Atoms (Maybe Section)
sourceSection = S.section . control

-- | Map of the binary deb section assignments (FIXME: redundant with BinaryDebDescription)
binarySection :: BinPkgName -> Lens Atoms (Maybe Section)
binarySection b = B.binarySection . binaryDebDescription b

-- * Debian dependency info

-- | <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Source>
source :: Lens Atoms (Maybe SrcPkgName)
source = S.source . control

-- | <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Changed-By>
changedBy :: Lens Atoms (Maybe NameAddr)
changedBy = S.changedBy . control

-- | <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Uploaders>
uploaders :: Lens Atoms ([NameAddr])
uploaders = S.uploaders . control

-- | Obsolete - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-DM-Upload-Allowed>
dmUploadAllowed :: Lens Atoms (Bool)
dmUploadAllowed = S.dmUploadAllowed . control

-- | The @Standards-Version@ field of the @debian/control@ file - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Standards-Version>
standardsVersion :: Lens Atoms (Maybe StandardsVersion)
standardsVersion = S.standardsVersion . control

-- | <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Homepage>
homepage :: Lens Atoms (Maybe Text)
homepage = S.homepage . control

-- | Version control system field - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-VCS-fields>
vcsFields :: Lens Atoms (Set S.VersionControlSpec)
vcsFields = S.vcsFields . control

-- | User defined fields - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.7>
xFields :: Lens Atoms (Set S.XField)
xFields = S.xFields . control

-- | <http://www.debian.org/doc/debian-policy/ch-relationships.html#s-sourcebinarydeps>
buildDepends :: Lens Atoms Relations
buildDepends = S.buildDepends . control

-- | <http://www.debian.org/doc/debian-policy/ch-relationships.html#s-sourcebinarydeps>
buildDependsIndep :: Lens Atoms Relations
buildDependsIndep = S.buildDependsIndep . control

-- | <http://www.debian.org/doc/debian-policy/ch-relationships.html#s-sourcebinarydeps>
buildConflicts :: Lens Atoms Relations
buildConflicts = S.buildConflicts . control

-- | <http://www.debian.org/doc/debian-policy/ch-relationships.html#s-sourcebinarydeps>
buildConflictsIndep :: Lens Atoms Relations
buildConflictsIndep = S.buildConflictsIndep . control

binaryPackages :: Lens Atoms [B.BinaryDebDescription]
binaryPackages = S.binaryPackages . control
