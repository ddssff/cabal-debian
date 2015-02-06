-- | This module holds a long list of lenses that access the Atoms
-- record, the record that holds the input data from which the
-- debianization is to be constructed.
{-# LANGUAGE CPP, DeriveDataTypeable, TemplateHaskell #-}
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
    , noTestSuite
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

import Control.Category ((.))
import Control.Monad.State (StateT)
--import Data.Default (def)
import Data.Generics (Data, Typeable)
import Data.Lens.Common (Lens, iso, getL)
import Data.Lens.Lazy ((%=))
import Data.Lens.Template (nameMakeLens)
import Data.List (init)
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
      { flags_ :: Flags
      -- ^ Information regarding mode of operation - verbosity, dry-run, usage, etc
      , warning_ :: Set Text
      -- ^ A warning to be reported later
      , sourceFormat_ :: Maybe SourceFormat
      -- ^ Write debian/source/format
      , watch_ :: Maybe Text
      -- ^ the @debian\/watch@ file
      , rulesHead_ :: Maybe Text
      -- ^ The rules file header
      , rulesSettings_ :: [Text]
      -- ^ The rules file assignments
      , rulesIncludes_ :: [Text]
      -- ^ The rules file include directives
      , rulesFragments_ :: Set Text
      -- ^ Additional fragments of the rules file
      , copyright_ :: Maybe CopyrightDescription
      -- ^ Override the copyright value computed from the cabal package description.
      , control_ :: S.SourceDebDescription
      -- ^ The parsed contents of the control file
      , intermediateFiles_ :: Set (FilePath, Text)
      -- ^ Put this text into a file with the given name in the debianization.
      , compat_ :: Maybe Int
      -- ^ The debhelper compatibility level, from debian/compat.
      , changelog_ :: Maybe ChangeLog
      -- ^ The changelog, first entry contains the source package name and version
      , installInit_ :: Map BinPkgName Text
      -- ^ Add an init.d file to the binary package
      , logrotateStanza_ :: Map BinPkgName (Set Text)
      -- ^ Add a stanza of a logrotate file to the binary package
      , postInst_ :: Map BinPkgName Text
      -- ^ Map of @debian/postinst@ scripts - to be run after install,
      -- should contain #DEBHELPER# line before exit 0
      , postRm_ :: Map BinPkgName Text
      -- ^ Map of @debian/postrm@ scripts - scripts to run after
      -- remove, should contain #DEBHELPER# line before exit 0
      , preInst_ :: Map BinPkgName Text
      -- ^ Map of @debian/preinst@ scripts - to be run before install,
      -- should contain #DEBHELPER# line before exit 0
      , preRm_ :: Map BinPkgName Text
      -- ^ Map of @debian/prerm@ scripts - to be run before remove,
      -- should contain #DEBHELPER# line before exit 0
      , atomSet_ :: Set Atom
      -- ^ set of items describing file installation requests
      , noDocumentationLibrary_ :: Bool
      -- ^ Do not produce a libghc-foo-doc package.
      , noProfilingLibrary_ :: Bool
      -- ^ Do not produce a libghc-foo-prof package.
      , omitProfVersionDeps_ :: Bool
      -- ^ If present, Do not put the version dependencies on the prof packages that we put on the dev packages.
      , omitLTDeps_ :: Bool
      -- ^ If present, don't generate the << dependency when we see a cabal
      -- equals dependency.  (The implementation of this was somehow lost.)
      , buildDir_ :: Maybe FilePath
      -- ^ The build directory used by cabal, typically dist/build when
      -- building manually or dist-ghc/build when building using GHC and
      -- haskell-devscripts.  This value is used to locate files
      -- produced by cabal so we can move them into the deb.  Note that
      -- the --builddir option of runhaskell Setup appends the "/build"
      -- to the value it receives, so, yes, try not to get confused.
      -- FIXME: make this FilePath or Maybe FilePath
      , sourcePackageName_ :: Maybe SrcPkgName
      -- ^ Name to give to the debian source package.  If not supplied
      -- the name is constructed from the cabal package name.  Note that
      -- DebianNameMap could encode this information if we already knew
      -- the cabal package name, but we can't assume that.
      , overrideDebianNameBase_ :: Maybe DebBase
      -- ^ If given, use this name for the base of the debian binary
      -- packages - the string between 'libghc-' and '-dev'.  Normally
      -- this is derived from the hackage package name.
      , revision_ :: Maybe String
      -- ^ Specify the revision string to use when converting the
      -- cabal version to debian.
      , debVersion_ :: Maybe DebianVersion
      -- ^ Specify the exact debian version of the resulting package,
      -- including epoch.  One use case is to work around the the
      -- "buildN" versions that are often uploaded to the debian and
      -- ubuntu repositories.  Say the latest cabal version of
      -- transformers is 0.3.0.0, but the debian repository contains
      -- version 0.3.0.0-1build3, we need to specify
      -- debVersion="0.3.0.0-1build3" or the version we produce will
      -- look older than the one already available upstream.
      , maintainerOption_ :: Maybe NameAddr
      , uploadersOption_ :: [NameAddr]
      -- ^ Value for the maintainer field in the control file.  Note that
      -- the cabal maintainer field can have multiple addresses, but debian
      -- only one.  If this is not explicitly set, it is obtained from the
      -- cabal file, and if it is not there then from the environment.  As a
      -- last resort, there is a hard coded string in here somewhere.
      , utilsPackageNameBase_ :: Maybe String
      -- ^ Name of a package that will get left-over data files and executables.
      -- If there are more than one, each package will get those files.
      , xDescriptionText_ :: Maybe Text
      -- ^ The text for the X-Description field of the Source package stanza.
      , comments_ :: Maybe [[Text]]
      -- ^ Each element is a comment to be added to the changelog, where the
      -- element's text elements are the lines of the comment.
      , missingDependencies_ :: Set BinPkgName
      -- ^ Lets cabal-debian know that a package it might expect to exist
      -- actually does not, so omit all uses in resulting debianization.
      , extraLibMap_ :: Map String Relations
      -- ^ Map a cabal Extra-Library name to a debian binary package name,
      -- e.g. @ExtraLibMapping extraLibMap "cryptopp" "libcrypto-dev"@ adds a
      -- build dependency *and* a regular dependency on @libcrypto-dev@ to
      -- any package that has @cryptopp@ in its cabal Extra-Library list.
      , execMap_ :: Map String Relations
      -- ^ Map a cabal Build-Tool name to a debian binary package name,
      -- e.g. @ExecMapping "trhsx" "haskell-hsx-utils"@ adds a build
      -- dependency on @haskell-hsx-utils@ to any package that has @trhsx@ in its
      -- cabal build-tool list.
      , apacheSite_ :: Map BinPkgName (String, FilePath, Text)
      -- ^ Have Apache configure a site using PACKAGE, DOMAIN, LOGDIR, and APACHECONFIGFILE
      , sourceArchitectures_ :: Maybe PackageArchitectures
      -- ^ Set the Architecture field of the source package
      , binaryArchitectures_ :: Map BinPkgName PackageArchitectures
      -- ^ Set the Architecture field of a binary package
      , sourcePriority_ :: Maybe PackagePriority
      -- ^ Set the Priority field of the source package
      , binaryPriorities_ :: Map BinPkgName PackagePriority
      -- ^ Set the Priority field of a binary package
      , sourceSection_ :: Maybe Section
      -- ^ Set the Section field of the source package
      , binarySections_ :: Map BinPkgName Section
      -- ^ Set the Section field of a binary package
      , executable_ :: Map BinPkgName InstallFile
      -- ^ Create a binary package to hold a cabal executable
      , serverInfo_ :: Map BinPkgName Server
      -- ^ Like DHExecutable, but configure the executable as a server process
      , website_ :: Map BinPkgName Site
      -- ^ Like DHServer, but configure the server as a web server
      , backups_ :: Map BinPkgName String
      -- ^ Configure the executable to do incremental backups
      , extraDevDeps_ :: Relations
      -- ^ Limited version of Depends, put a dependency on the dev library package.  The only
      -- reason to use this is because we don't yet know the name of the dev library package.
      , official_ :: Bool
      -- ^ Whether this packaging is created by the Debian Haskell Group
      , noTestSuite_ :: Bool
      -- ^ Force omission of the test suites from the debianization
      , allowDebianSelfBuildDeps_ :: Bool
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
    { flags_ = fs
    , warning_ = mempty
    , sourceFormat_ = Nothing
    , watch_ = Nothing
    , rulesHead_ = Nothing
    , rulesSettings_ = mempty
    , rulesIncludes_ = mempty
    , rulesFragments_ = mempty
    , copyright_ = Nothing
    , control_ = S.newSourceDebDescription
    , intermediateFiles_ = mempty
    , compat_ = Nothing
    , changelog_ = Nothing
    , installInit_ = mempty
    , logrotateStanza_ = mempty
    , postInst_ = mempty
    , postRm_ = mempty
    , preInst_ = mempty
    , preRm_ = mempty
    , atomSet_ = mempty
    , noDocumentationLibrary_ = False
    , noProfilingLibrary_ = False
    , omitProfVersionDeps_ = False
    , omitLTDeps_ = False
    , buildDir_ = Nothing
    , sourcePackageName_ = Nothing
    , overrideDebianNameBase_ = Nothing
    , revision_ = Nothing
    , debVersion_ = Nothing
    , maintainerOption_ = Nothing
    , uploadersOption_ = []
    , utilsPackageNameBase_ = Nothing
    , xDescriptionText_ = Nothing
    , comments_ = Nothing
    , missingDependencies_ = mempty
    , extraLibMap_ = mempty
    , execMap_ = mempty
    , apacheSite_ = mempty
    , sourceArchitectures_ = Nothing
    , binaryArchitectures_ = mempty
    , sourcePriority_ = Nothing
    , binaryPriorities_ = mempty
    , sourceSection_ = Nothing
    , binarySections_ = mempty
    , executable_ = mempty
    , serverInfo_ = mempty
    , website_ = mempty
    , backups_ = mempty
    , extraDevDeps_ = mempty
    , official_ = False
    , noTestSuite_ = False
    , allowDebianSelfBuildDeps_ = False
    }

instance Canonical DebInfo where
    canonical x = x {control_ = canonical (control_ x)}

$(let f s = case s of
              (_ : _) | last s == '_' -> Just (init s)
              _ -> Nothing in
  nameMakeLens ''DebInfo f)

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
binaryDebDescription b = maybeLens (newBinaryDebDescription b) (iso id id) . listElemLens ((== b) . getL package) . S.binaryPackages . control
