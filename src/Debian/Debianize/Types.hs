{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Debian.Debianize.Types
    (
    -- * Cabal package info
      packageDescription

    -- * Repository info
    , execMap
    , epochMap
    , missingDependencies
    , extraLibMap
    , debianNameMap

    -- * Source Package Info
    , sourcePackageName
    , overrideDebianNameBase
    , revision
    , debVersion
    , debianMaintainer
    , debianUploaders
    , maintainerOption
    , uploadersOption
    , copyright
    , sourceArchitectures
    , sourcePriority
    , sourceSection
    , compat
    , sourceFormat
    , changelog
    , comments
    , standardsVersion
    , rulesHead
    , rulesSettings
    , rulesIncludes
    , rulesFragments
    , official
    , noProfilingLibrary
    , noDocumentationLibrary
    , utilsPackageNameBase
    , buildDir
    , watch
    , xDescription

    -- * Source Package Build Dependencies
    , omitProfVersionDeps
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
import Data.Lens.Lazy (getL, iso, Lens)
import Data.Set as Set (Set)
import Data.Text (Text)
import Debian.Debianize.Prelude (listElemLens, maybeLens)
import Debian.Debianize.Types.Atoms (apacheSite, backups, buildDir, changelog, comments, compat, control, copyright, debianNameMap, DebInfo, debVersion, epochMap, execMap, executable, extraDevDeps, extraLibMap, file, flags, install, installCabalExec, installCabalExecTo, installData, installDir, installInit, installTo, intermediateFiles, link, logrotateStanza, maintainerOption, missingDependencies, noDocumentationLibrary, noProfilingLibrary, official, omitLTDeps, omitProfVersionDeps, overrideDebianNameBase, packageDescription, packageInfo, postInst, postRm, preInst, preRm, revision, rulesFragments, rulesHead, rulesIncludes, rulesSettings, serverInfo, sourceArchitectures, sourceFormat, sourcePackageName, uploadersOption, utilsPackageNameBase, warning, watch, website, xDescription)
import qualified Debian.Debianize.Types.BinaryDebDescription as B (architecture, BinaryDebDescription, binaryPriority, binarySection, breaks, builtUsing, conflicts, depends, description, essential, newBinaryDebDescription, package, packageType, PackageType, preDepends, provides, recommends, relations, replaces, suggests)
import qualified Debian.Debianize.Types.SourceDebDescription as S (binaryPackages, buildConflicts, buildConflictsIndep, buildDepends, buildDependsIndep, changedBy, dmUploadAllowed, homepage, maintainer, priority, section, source, standardsVersion, uploaders, vcsFields, VersionControlSpec, XField, xFields)
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures, PackagePriority, Section, StandardsVersion)
import Debian.Relation (BinPkgName, Relations, SrcPkgName)
import Prelude hiding ((.), init, init, log, log, unlines)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

-- | Not exported - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Package>
binaryDebDescription :: BinPkgName -> Lens DebInfo B.BinaryDebDescription
binaryDebDescription b = maybeLens (B.newBinaryDebDescription b) (iso id id) . listElemLens ((== b) . getL B.package) . S.binaryPackages . control

-- | Lens onto one of several 'B.PackageType' values of which we have
-- specific knowledge how to package.
packageType :: BinPkgName -> Lens DebInfo (Maybe B.PackageType)
packageType b = B.packageType . binaryDebDescription b

-- | Lens into the description field of a BinaryDebDescription.
debianDescription :: BinPkgName -> Lens DebInfo (Maybe Text)
debianDescription b = B.description . binaryDebDescription b

-- | <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Essential>
essential :: BinPkgName -> Lens DebInfo (Maybe Bool)
essential b = B.essential . binaryDebDescription b

-- | <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
-- relations :: BinPkgName -> Lens DebInfo B.PackageRelations
-- relations b = B.relations . binaryDebDescription b

-- | The Depends: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
depends :: BinPkgName -> Lens DebInfo Relations
depends b = B.depends . B.relations . binaryDebDescription b

-- | The Recommends: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
recommends :: BinPkgName -> Lens DebInfo Relations
recommends b = B.recommends . B.relations . binaryDebDescription b

-- | The Suggests: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
suggests :: BinPkgName -> Lens DebInfo Relations
suggests b = B.suggests . B.relations . binaryDebDescription b

-- | The Pre-Depends: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
preDepends :: BinPkgName -> Lens DebInfo Relations
preDepends b = B.preDepends . B.relations . binaryDebDescription b

-- | The Breaks: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
breaks :: BinPkgName -> Lens DebInfo Relations
breaks b = B.breaks . B.relations . binaryDebDescription b

-- | The Conflicts: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
conflicts :: BinPkgName -> Lens DebInfo Relations
conflicts b = B.conflicts . B.relations . binaryDebDescription b

-- | The Provides: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
provides :: BinPkgName -> Lens DebInfo Relations
provides b = B.provides . B.relations . binaryDebDescription b

-- | The Replaces: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
replaces :: BinPkgName -> Lens DebInfo Relations
replaces b = B.replaces . B.relations . binaryDebDescription b

-- | THe Built-Using: relations for each binary deb - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.6.10>
builtUsing :: BinPkgName -> Lens DebInfo Relations
builtUsing b = B.builtUsing . B.relations . binaryDebDescription b

debianMaintainer :: Lens DebInfo (Maybe NameAddr)
debianMaintainer = S.maintainer . control

debianUploaders :: Lens DebInfo [NameAddr]
debianUploaders = S.uploaders . control

-- | The architectures supported by a binary package
binaryArchitectures :: BinPkgName -> Lens DebInfo (Maybe PackageArchitectures)
binaryArchitectures b = B.architecture . binaryDebDescription b

-- | The source package priority - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Priority>
sourcePriority :: Lens DebInfo (Maybe PackagePriority)
sourcePriority = S.priority . control

-- | The source package's section assignment - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Section>
sourceSection :: Lens DebInfo (Maybe Section)
sourceSection = S.section . control

-- | Map of the binary package priorities (FIXME: redundant with BinaryDebDescription)
binaryPriority :: BinPkgName -> Lens DebInfo (Maybe PackagePriority)
binaryPriority b = B.binaryPriority . binaryDebDescription b

-- | Map of the binary deb section assignments (FIXME: redundant with BinaryDebDescription)
binarySection :: BinPkgName -> Lens DebInfo (Maybe Section)
binarySection b = B.binarySection . binaryDebDescription b

-- * Debian dependency info

-- | <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Source>
source :: Lens DebInfo (Maybe SrcPkgName)
source = S.source . control

-- | <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Changed-By>
changedBy :: Lens DebInfo (Maybe NameAddr)
changedBy = S.changedBy . control

-- | <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Uploaders>
uploaders :: Lens DebInfo ([NameAddr])
uploaders = S.uploaders . control

-- | Obsolete - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-DM-Upload-Allowed>
dmUploadAllowed :: Lens DebInfo (Bool)
dmUploadAllowed = S.dmUploadAllowed . control

-- | The @Standards-Version@ field of the @debian/control@ file - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Standards-Version>
standardsVersion :: Lens DebInfo (Maybe StandardsVersion)
standardsVersion = S.standardsVersion . control

-- | <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Homepage>
homepage :: Lens DebInfo (Maybe Text)
homepage = S.homepage . control

-- | Version control system field - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-VCS-fields>
vcsFields :: Lens DebInfo (Set S.VersionControlSpec)
vcsFields = S.vcsFields . control

-- | User defined fields - <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s5.7>
xFields :: Lens DebInfo (Set S.XField)
xFields = S.xFields . control

-- | <http://www.debian.org/doc/debian-policy/ch-relationships.html#s-sourcebinarydeps>
buildDepends :: Lens DebInfo Relations
buildDepends = S.buildDepends . control

-- | <http://www.debian.org/doc/debian-policy/ch-relationships.html#s-sourcebinarydeps>
buildDependsIndep :: Lens DebInfo Relations
buildDependsIndep = S.buildDependsIndep . control

-- | <http://www.debian.org/doc/debian-policy/ch-relationships.html#s-sourcebinarydeps>
buildConflicts :: Lens DebInfo Relations
buildConflicts = S.buildConflicts . control

-- | <http://www.debian.org/doc/debian-policy/ch-relationships.html#s-sourcebinarydeps>
buildConflictsIndep :: Lens DebInfo Relations
buildConflictsIndep = S.buildConflictsIndep . control

binaryPackages :: Lens DebInfo [B.BinaryDebDescription]
binaryPackages = S.binaryPackages . control
