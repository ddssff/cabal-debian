-- | This module holds a long list of lenses that access the Atoms
-- record, the record that holds the input data from which the
-- debianization is to be constructed.
{-# LANGUAGE CPP, DeriveDataTypeable, Rank2Types, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.DebInfo
    ( -- * Types
#if __HADDOCK__
      -- Is this ifdef working?
      DebInfo(..)
#else
      DebInfo
#endif
    , Atom(File, Install, InstallCabalExec, InstallCabalExecTo, InstallData, InstallDir, InstallTo, Link)
    , Site(Site, domain, server, serverAdmin)
    , Server(Server, headerMessage, hostname, installFile, port, retry, serverFlags)
    , InstallFile(InstallFile, destDir, destName, execName, sourceDir)

      -- * Lenses
    , flags
    , warning
    , sourceFormat
    , watch
    , rulesHead
    , rulesSettings
    , rulesIncludes
    , rulesFragments
    , copyright
    , control
    , intermediateFiles
    , compat
    , changelog
    , installInit
    , logrotateStanza
    , postInst
    , postRm
    , preInst
    , preRm
    , atomSet
    , noDocumentationLibrary
    , noProfilingLibrary
    , omitProfVersionDeps
    , omitLTDeps
    , buildDir
    , sourcePackageName
    , overrideDebianNameBase
    , revision
    , debVersion
    , maintainerOption
    , uploadersOption
    , utilsPackageNameBase
    , xDescriptionText
    , comments
    , missingDependencies
    , extraLibMap
    , execMap
    , apacheSite
    , sourceArchitectures
    , binaryArchitectures
    , sourcePriority
    , binaryPriorities
    , sourceSection
    , binarySections
    , executable
    , serverInfo
    , website
    , backups
    , extraDevDeps
    , official
    , enableTests
    , runTests
    , allowDebianSelfBuildDeps

    , binaryDebDescription

      -- * Atom builders
    , link
    , install
    , installTo
    , installData
    , file
    , installCabalExec
    , installCabalExecTo
    , installDir

      -- * DebInfo Builder
    , makeDebInfo
    ) where

import OldLens (Lens, iso, getL, (%=))

import Control.Category ((.))
import Control.Monad.State (StateT)
--import Data.Default (def)
import Data.Generics (Data, Typeable)
import Control.Lens.TH (makeLenses)
import Data.Map as Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set as Set (insert, Set)
import Data.Text (Text)
import Debian.Changes (ChangeLog)
import Debian.Debianize.BasicInfo (Flags)
import Debian.Debianize.Prelude (listElemLens, maybeLens)
import Debian.Debianize.BinaryDebDescription (BinaryDebDescription, Canonical(canonical), newBinaryDebDescription, package)
import Debian.Debianize.CopyrightDescription (CopyrightDescription)
import qualified Debian.Debianize.SourceDebDescription as S (newSourceDebDescription, SourceDebDescription, binaryPackages)
import Debian.Debianize.VersionSplits (DebBase)
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures, PackagePriority, Section, SourceFormat)
import Debian.Relation (BinPkgName, Relations, SrcPkgName)
import Debian.Version (DebianVersion)
import Prelude hiding ((.), init, init, log, log)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

-- | Information required to represent a non-cabal debianization.
data DebInfo
    = DebInfo
      { _flags :: Flags
      -- ^ Information regarding mode of operation - verbosity, dry-run, usage, etc
      , _warning :: Set Text
      -- ^ A warning to be reported later
      , _sourceFormat :: Maybe SourceFormat
      -- ^ Write debian/source/format
      , _watch :: Maybe Text
      -- ^ the @debian\/watch@ file
      , _rulesHead :: Maybe Text
      -- ^ The rules file header
      , _rulesSettings :: [Text]
      -- ^ The rules file assignments
      , _rulesIncludes :: [Text]
      -- ^ The rules file include directives
      , _rulesFragments :: Set Text
      -- ^ Additional fragments of the rules file
      , _copyright :: Maybe CopyrightDescription
      -- ^ Override the copyright value computed from the cabal package description.
      , _control :: S.SourceDebDescription
      -- ^ The parsed contents of the control file
      , _intermediateFiles :: Set (FilePath, Text)
      -- ^ Put this text into a file with the given name in the debianization.
      , _compat :: Maybe Int
      -- ^ The debhelper compatibility level, from debian/compat.
      , _changelog :: Maybe ChangeLog
      -- ^ The changelog, first entry contains the source package name and version
      , _installInit :: Map BinPkgName Text
      -- ^ Add an init.d file to the binary package
      , _logrotateStanza :: Map BinPkgName (Set Text)
      -- ^ Add a stanza of a logrotate file to the binary package
      , _postInst :: Map BinPkgName Text
      -- ^ Map of @debian/postinst@ scripts - to be run after install,
      -- should contain #DEBHELPER# line before exit 0
      , _postRm :: Map BinPkgName Text
      -- ^ Map of @debian/postrm@ scripts - scripts to run after
      -- remove, should contain #DEBHELPER# line before exit 0
      , _preInst :: Map BinPkgName Text
      -- ^ Map of @debian/preinst@ scripts - to be run before install,
      -- should contain #DEBHELPER# line before exit 0
      , _preRm :: Map BinPkgName Text
      -- ^ Map of @debian/prerm@ scripts - to be run before remove,
      -- should contain #DEBHELPER# line before exit 0
      , _atomSet :: Set Atom
      -- ^ set of items describing file installation requests
      , _noDocumentationLibrary :: Bool
      -- ^ Do not produce a libghc-foo-doc package.
      , _noProfilingLibrary :: Bool
      -- ^ Do not produce a libghc-foo-prof package.
      , _omitProfVersionDeps :: Bool
      -- ^ If present, Do not put the version dependencies on the prof packages that we put on the dev packages.
      , _omitLTDeps :: Bool
      -- ^ If present, don't generate the << dependency when we see a cabal
      -- equals dependency.  (The implementation of this was somehow lost.)
      , _buildDir :: Maybe FilePath
      -- ^ The build directory used by cabal, typically dist/build when
      -- building manually or dist-ghc/build when building using GHC and
      -- haskell-devscripts.  This value is used to locate files
      -- produced by cabal so we can move them into the deb.  Note that
      -- the --builddir option of runhaskell Setup appends the "/build"
      -- to the value it receives, so, yes, try not to get confused.
      -- FIXME: make this FilePath or Maybe FilePath
      , _sourcePackageName :: Maybe SrcPkgName
      -- ^ Name to give to the debian source package.  If not supplied
      -- the name is constructed from the cabal package name.  Note that
      -- DebianNameMap could encode this information if we already knew
      -- the cabal package name, but we can't assume that.
      , _overrideDebianNameBase :: Maybe DebBase
      -- ^ If given, use this name for the base of the debian binary
      -- packages - the string between 'libghc-' and '-dev'.  Normally
      -- this is derived from the hackage package name.
      , _revision :: Maybe String
      -- ^ Specify the revision string to use when converting the
      -- cabal version to debian.
      , _debVersion :: Maybe DebianVersion
      -- ^ Specify the exact debian version of the resulting package,
      -- including epoch.  One use case is to work around the the
      -- "buildN" versions that are often uploaded to the debian and
      -- ubuntu repositories.  Say the latest cabal version of
      -- transformers is 0.3.0.0, but the debian repository contains
      -- version 0.3.0.0-1build3, we need to specify
      -- debVersion="0.3.0.0-1build3" or the version we produce will
      -- look older than the one already available upstream.
      , _maintainerOption :: Maybe NameAddr
      , _uploadersOption :: [NameAddr]
      -- ^ Value for the maintainer field in the control file.  Note that
      -- the cabal maintainer field can have multiple addresses, but debian
      -- only one.  If this is not explicitly set, it is obtained from the
      -- cabal file, and if it is not there then from the environment.  As a
      -- last resort, there is a hard coded string in here somewhere.
      , _utilsPackageNameBase :: Maybe String
      -- ^ Name of a package that will get left-over data files and executables.
      -- If there are more than one, each package will get those files.
      , _xDescriptionText :: Maybe Text
      -- ^ The text for the X-Description field of the Source package stanza.
      , _comments :: Maybe [[Text]]
      -- ^ Each element is a comment to be added to the changelog, where the
      -- element's text elements are the lines of the comment.
      , _missingDependencies :: Set BinPkgName
      -- ^ Lets cabal-debian know that a package it might expect to exist
      -- actually does not, so omit all uses in resulting debianization.
      , _extraLibMap :: Map String Relations
      -- ^ Map a cabal Extra-Library name to a debian binary package name,
      -- e.g. @ExtraLibMapping extraLibMap "cryptopp" "libcrypto-dev"@ adds a
      -- build dependency *and* a regular dependency on @libcrypto-dev@ to
      -- any package that has @cryptopp@ in its cabal Extra-Library list.
      , _execMap :: Map String Relations
      -- ^ Map a cabal Build-Tool name to a debian binary package name,
      -- e.g. @ExecMapping "trhsx" "haskell-hsx-utils"@ adds a build
      -- dependency on @haskell-hsx-utils@ to any package that has @trhsx@ in its
      -- cabal build-tool list.
      , _apacheSite :: Map BinPkgName (String, FilePath, Text)
      -- ^ Have Apache configure a site using PACKAGE, DOMAIN, LOGDIR, and APACHECONFIGFILE
      , _sourceArchitectures :: Maybe PackageArchitectures
      -- ^ Set the Architecture field of the source package
      , _binaryArchitectures :: Map BinPkgName PackageArchitectures
      -- ^ Set the Architecture field of a binary package
      , _sourcePriority :: Maybe PackagePriority
      -- ^ Set the Priority field of the source package
      , _binaryPriorities :: Map BinPkgName PackagePriority
      -- ^ Set the Priority field of a binary package
      , _sourceSection :: Maybe Section
      -- ^ Set the Section field of the source package
      , _binarySections :: Map BinPkgName Section
      -- ^ Set the Section field of a binary package
      , _executable :: Map BinPkgName InstallFile
      -- ^ Create a binary package to hold a cabal executable
      , _serverInfo :: Map BinPkgName Server
      -- ^ Like DHExecutable, but configure the executable as a server process
      , _website :: Map BinPkgName Site
      -- ^ Like DHServer, but configure the server as a web server
      , _backups :: Map BinPkgName String
      -- ^ Configure the executable to do incremental backups
      , _extraDevDeps :: Relations
      -- ^ Limited version of Depends, put a dependency on the dev library package.  The only
      -- reason to use this is because we don't yet know the name of the dev library package.
      , _official :: Bool
      -- ^ Whether this packaging is created by the Debian Haskell Group
      , _enableTests :: Bool
      -- ^ Include the test suites in the debianization if they exists
      , _runTests :: Bool
      -- ^ Prevent the test suite from being run during the package build
      , _allowDebianSelfBuildDeps :: Bool
      -- ^ Normally self dependencies are filtered out of the debian
      -- build dependency list because they usually reflect
      -- interdependencies between the library and the executable in
      -- the Cabal packages.  This flag turns off that filtering.
      } deriving (Show, Data, Typeable)

data Atom
    = Link BinPkgName FilePath FilePath
      -- ^ Create a symbolic link in the binary package
    | Install BinPkgName FilePath FilePath
      -- ^ Install a build file into the binary package
    | InstallTo BinPkgName FilePath FilePath
      -- ^ Install a build file into the binary package at an exact location
    | InstallData BinPkgName FilePath FilePath
      -- ^ DHInstallTo somewhere relative to DataDir (see above)
    | File BinPkgName FilePath Text
      -- ^ Create a file with the given text at the given path
    | InstallCabalExec BinPkgName String FilePath
      -- ^ Install a cabal executable into the binary package
    | InstallCabalExecTo BinPkgName String FilePath
      -- ^ Install a cabal executable into the binary package at an exact location
    | InstallDir BinPkgName FilePath
      -- ^ Create a directory in the binary package
    deriving (Show, Eq, Ord, Data, Typeable)

data InstallFile
    = InstallFile
      { execName :: String -- ^ The name of the executable file
      , sourceDir :: Maybe FilePath -- ^ where to find it, default is dist/build/<execName>/
      , destDir :: Maybe FilePath -- ^ where to put it, default is usr/bin/<execName>
      , destName :: String  -- ^ name to give installed executable
      } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | Information about the web site we are packaging.
data Site
    = Site
      { domain :: String   -- ^ The domain name assigned to the server.
                           -- An apache configuration will be generated to
                           -- redirect requests from this domain to hostname:port
      , serverAdmin :: String   -- ^ Apache ServerAdmin parameter
      , server :: Server   -- ^ The hint to install the server job
      } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | Information about the server we are packaging.
data Server
    = Server
      { hostname :: String      -- ^ Host on which the server will run
      , port :: Int             -- ^ Port on which the server will run.
                                -- Obviously, this must assign each and
                                -- every server package to a different
                                -- port.
      , headerMessage :: String -- ^ A comment that will be inserted to
                                -- explain how the file was generated
      , retry :: String         -- ^ start-stop-daemon --retry argument
      , serverFlags :: [String] -- ^ Extra flags to pass to the server via the init script
      , installFile :: InstallFile -- ^ The hint to install the server executable
      } deriving (Read, Show, Eq, Ord, Data, Typeable)

makeDebInfo :: Flags -> DebInfo
makeDebInfo fs =
    DebInfo
    { _flags = fs
    , _warning = mempty
    , _sourceFormat = Nothing
    , _watch = Nothing
    , _rulesHead = Nothing
    , _rulesSettings = mempty
    , _rulesIncludes = mempty
    , _rulesFragments = mempty
    , _copyright = Nothing
    , _control = S.newSourceDebDescription
    , _intermediateFiles = mempty
    , _compat = Nothing
    , _changelog = Nothing
    , _installInit = mempty
    , _logrotateStanza = mempty
    , _postInst = mempty
    , _postRm = mempty
    , _preInst = mempty
    , _preRm = mempty
    , _atomSet = mempty
    , _noDocumentationLibrary = False
    , _noProfilingLibrary = False
    , _omitProfVersionDeps = False
    , _omitLTDeps = False
    , _buildDir = Nothing
    , _sourcePackageName = Nothing
    , _overrideDebianNameBase = Nothing
    , _revision = Nothing
    , _debVersion = Nothing
    , _maintainerOption = Nothing
    , _uploadersOption = []
    , _utilsPackageNameBase = Nothing
    , _xDescriptionText = Nothing
    , _comments = Nothing
    , _missingDependencies = mempty
    , _extraLibMap = mempty
    , _execMap = mempty
    , _apacheSite = mempty
    , _sourceArchitectures = Nothing
    , _binaryArchitectures = mempty
    , _sourcePriority = Nothing
    , _binaryPriorities = mempty
    , _sourceSection = Nothing
    , _binarySections = mempty
    , _executable = mempty
    , _serverInfo = mempty
    , _website = mempty
    , _backups = mempty
    , _extraDevDeps = mempty
    , _official = False
    , _enableTests = True
    , _runTests = True
    , _allowDebianSelfBuildDeps = False
    }

instance Canonical DebInfo where
    canonical x = x {_control = canonical (_control x)}

$(makeLenses ''DebInfo)

-- We need (%=_)
link :: Monad m => BinPkgName -> FilePath -> FilePath -> StateT DebInfo m ()
link b from dest = atomSet %= (Set.insert $ Link b from dest) >> return ()
install :: Monad m => BinPkgName -> FilePath -> FilePath -> StateT DebInfo m ()
install b from dest = atomSet %= (Set.insert $ Install b from dest) >> return ()
installTo :: Monad m => BinPkgName -> FilePath -> FilePath -> StateT DebInfo m ()
installTo b from dest = atomSet %= (Set.insert $ InstallTo b from dest) >> return ()
installData :: Monad m => BinPkgName -> FilePath -> FilePath -> StateT DebInfo m ()
installData b from dest = atomSet %= (Set.insert $ InstallData b from dest) >> return ()
file :: Monad m => BinPkgName -> FilePath -> Text -> StateT DebInfo m ()
file b dest content = atomSet %= (Set.insert $ File b dest content) >> return ()
installCabalExec :: Monad m => BinPkgName -> String -> FilePath -> StateT DebInfo m ()
installCabalExec b name dest = atomSet %= (Set.insert $ InstallCabalExec b name dest) >> return ()
installCabalExecTo :: Monad m => BinPkgName -> String -> FilePath -> StateT DebInfo m ()
installCabalExecTo b name dest = atomSet %= (Set.insert $ InstallCabalExecTo b name dest) >> return ()
installDir :: Monad m => BinPkgName -> FilePath -> StateT DebInfo m ()
installDir b dir = atomSet %= (Set.insert $ InstallDir b dir) >> return ()

-- | Lens to look up the binary deb description by name and create it if absent.
-- <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Package>
binaryDebDescription :: BinPkgName -> Lens DebInfo BinaryDebDescription
binaryDebDescription b =
    control . S.binaryPackages . listElemLens ((== b) . getL package) . maybeLens (newBinaryDebDescription b) (iso id id)
