-- | This module holds a long list of lenses that access the Atoms
-- record, the record that holds the input data from which the
-- debianization is to be constructed.
{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.Types.Atoms
{-    ( Atoms
    , showAtoms
    , newAtoms
    , InstallFile(..) -- FIXME - lenses for this
    , Server(..) -- FIXME - lenses for this
    , Site(..) -- FIXME - lenses for this
    , DebAction(..)
    , DebType(..)
    , PackageInfo(..)
    ) -} where

import Control.Applicative ((<$>))
import Control.Monad.State (StateT)
import Data.Generics (Data, Typeable)
import Data.Lens.Lazy (Lens, lens, (%=))
import Data.Map as Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set as Set (Set, insert)
import Data.Text (Text)
import Debian.Changes (ChangeLog)
import Debian.Debianize.InputCabalPackageDescription (inputCabalization, Flags, flagOptions, defaultFlags)
import Debian.Debianize.Types.CopyrightDescription
import Debian.Debianize.Types.BinaryDebDescription (Canonical(canonical))
import qualified Debian.Debianize.Types.SourceDebDescription as S
import Debian.Debianize.VersionSplits (VersionSplits, DebBase)
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures, PackagePriority, Section, SourceFormat)
import Debian.Relation (BinPkgName, Relations, SrcPkgName)
import Debian.Version (DebianVersion)
import Distribution.Package (PackageName)
import Distribution.PackageDescription as Cabal (PackageDescription)
import Prelude hiding (init, init, log, log, (.))
import System.Console.GetOpt (getOpt, ArgOrder(Permute))
import System.Environment (getArgs)
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
      { noDocumentationLibrary_ :: Bool
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
      , flags_ :: Flags
      -- ^ Information regarding mode of operation - verbosity, dry-run, usage, etc
      , debianNameMap_ :: Map PackageName VersionSplits
      -- ^ Mapping from cabal package name and version to debian source
      -- package name.  This allows different ranges of cabal versions to
      -- map to different debian source package names.
      , control_ :: S.SourceDebDescription
      -- ^ The parsed contents of the control file
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
      , sourceFormat_ :: Maybe SourceFormat
      -- ^ Write debian/source/format
      , watch_ :: Maybe Text
      -- ^ Write debian/watch
      , intermediateFiles_ :: Set (FilePath, Text)
      -- ^ Put this text into a file with the given name in the debianization.
      , rulesHead_ :: Maybe Text
      , rulesSettings_ :: [Text]
      , rulesIncludes_ :: [Text]
      -- ^ The header of the debian/rules file.  The remainder is assembled
      -- from DebRulesFragment values in the atom list.
      , rulesFragments_ :: Set Text
      -- ^ A Fragment of debian/rules
      , warning_ :: Set Text
      -- ^ A warning to be reported later
      , utilsPackageNameBase_ :: Maybe String
      -- ^ Name of a package that will get left-over data files and executables.
      -- If there are more than one, each package will get those files.
      , xDescription_ :: Maybe Text
      -- ^ The text for the X-Description field of the Source package stanza.
      , changelog_ :: Maybe ChangeLog
      -- ^ The changelog, first entry contains the source package name and version
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
      , epochMap_ :: Map PackageName Int
      -- ^ Specify epoch numbers for the debian package generated from a
      -- cabal package.  Example: @EpochMapping (PackageName "HTTP") 1@.
      , packageInfo_ :: Map PackageName PackageInfo
      -- ^ Supply some info about a cabal package.
      , compat_ :: Maybe Int
      -- ^ The debhelper compatibility level, from debian/compat.
      -- , copyright_ :: Maybe (Either CopyrightDescription Text)
      , copyright_ :: PackageDescription -> IO CopyrightDescription
      -- ^ Copyright and license information.  This function takes a list of FilePath like
      -- the licenseFiles field of the PackageDescription and returns a CopyrightDescription.
      , apacheSite_ :: Map BinPkgName (String, FilePath, Text)
      -- ^ Have Apache configure a site using PACKAGE, DOMAIN, LOGDIR, and APACHECONFIGFILE
      , logrotateStanza_ :: Map BinPkgName (Set Text)
      -- ^ Add a stanza of a logrotate file to the binary package
      , postInst_ :: Map BinPkgName Text
      -- ^ Script to run after install, should contain #DEBHELPER# line before exit 0
      , postRm_ :: Map BinPkgName Text
      -- ^ Script to run after remove, should contain #DEBHELPER# line before exit 0
      , preInst_ :: Map BinPkgName Text
      -- ^ Script to run before install, should contain #DEBHELPER# line before exit 0
      , preRm_ :: Map BinPkgName Text
      -- ^ Script to run before remove, should contain #DEBHELPER# line before exit 0
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
      , atomSet_ :: Set Atom
      -- ^ set of items describing file installation requests
      , installInit_ :: Map BinPkgName Text
      -- ^ Add an init.d file to the binary package
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
      , packageDescription_ :: PackageDescription
      -- ^ The result of reading a cabal configuration file.
      , official_ :: Bool
      -- ^ Whether this packaging is created by the Debian Haskell Group
      } deriving (Show, Data, Typeable)

instance Canonical Atoms where
    canonical x = x {control_ = canonical (control_ x)}

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

type Atoms' = Set Atom

-- | Look for --buildenvdir in the command line arguments to get the
-- changeroot path, use "/" if not present.
newAtoms :: IO Atoms
newAtoms = do
  (fns, _, _) <- getOpt Permute flagOptions <$> getArgs
  let flags' = foldr ($) defaultFlags fns
  pkgDesc <- inputCabalization flags'
  return $ makeAtoms flags' pkgDesc

makeAtoms :: Flags -> PackageDescription -> Atoms
makeAtoms fs pkgDesc =
    Atoms
      { noDocumentationLibrary_ = False
      , noProfilingLibrary_ = False
      , omitProfVersionDeps_ = False
      , omitLTDeps_ = False
      , buildDir_ = Nothing
      , flags_ = fs
      , debianNameMap_ = mempty
      , control_ = S.newSourceDebDescription
      , sourcePackageName_ = Nothing
      , overrideDebianNameBase_ = Nothing
      , revision_ = Nothing
      , debVersion_ = Nothing
      , maintainerOption_ = Nothing
      , uploadersOption_ = []
      , sourceFormat_ = Nothing
      , watch_ = Nothing
      , intermediateFiles_ = mempty
      , rulesHead_ = Nothing
      , rulesSettings_ = mempty
      , rulesIncludes_ = mempty
      , rulesFragments_ = mempty
      , warning_ = mempty
      , utilsPackageNameBase_ = Nothing
      , changelog_ = Nothing
      , xDescription_ = Nothing
      , comments_ = Nothing
      , missingDependencies_ = mempty
      , extraLibMap_ = mempty
      , execMap_ = mempty
      , epochMap_ = mempty
      , packageInfo_ = mempty
      , compat_ = Nothing
      , copyright_ = defaultCopyrightDescription newCopyrightDescription
      , apacheSite_ = mempty
      , logrotateStanza_ = mempty
      , postInst_ = mempty
      , postRm_ = mempty
      , preInst_ = mempty
      , preRm_ = mempty
      , sourceArchitectures_ = Nothing
      , binaryArchitectures_ = mempty
      , sourcePriority_ = Nothing
      , binaryPriorities_ = mempty
      , sourceSection_ = Nothing
      , binarySections_ = mempty
      , atomSet_ = mempty
      , installInit_ = mempty
      , executable_ = mempty
      , serverInfo_ = mempty
      , website_ = mempty
      , backups_ = mempty
      , extraDevDeps_ = mempty
      , packageDescription_ = pkgDesc
      , official_ = False
      }

data PackageInfo = PackageInfo { cabalName :: PackageName
                               , devDeb :: Maybe (BinPkgName, DebianVersion)
                               , profDeb :: Maybe (BinPkgName, DebianVersion)
                               , docDeb :: Maybe (BinPkgName, DebianVersion) } deriving (Eq, Ord, Show, Data, Typeable)

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

data InstallFile
    = InstallFile
      { execName :: String -- ^ The name of the executable file
      , sourceDir :: Maybe FilePath -- ^ where to find it, default is dist/build/<execName>/
      , destDir :: Maybe FilePath -- ^ where to put it, default is usr/bin/<execName>
      , destName :: String  -- ^ name to give installed executable
      } deriving (Read, Show, Eq, Ord, Data, Typeable)

showAtoms :: Atoms -> IO ()
showAtoms x = putStrLn ("\nTop: " ++ show x ++ "\n")

-- | Obsolete record containing verbosity, dryRun, validate, and debAction.
flags :: Lens Atoms Flags
flags = lens flags_ (\ b a -> a {flags_ = b})

-- | Unused
warning :: Lens Atoms (Set Text)
warning = lens warning_ (\ a b -> b {warning_ = a})

-- | The build directory.  This can be set by an argument to the @Setup@ script.
-- When @Setup@ is run manually it is just @dist@, when it is run by
-- @dpkg-buildpackage@ the compiler name is appended, so it is typically
-- @dist-ghc@.  Cabal-debian needs the correct value of buildDir to find
-- the build results.
buildDir :: Lens Atoms (Maybe FilePath)
buildDir = lens buildDir_ (\ b a -> a {buildDir_ = b})

-- We need to update ghcVersion when this is changed, which means doing IO
-- buildEnv :: Lens Atoms (Maybe EnvSet)
-- buildEnv = lens buildEnv_ (\ b a -> a {buildEnv_ = b})

-- | Map from cabal Extra-Lib names to debian binary package names.
extraLibMap :: Lens Atoms (Map String Relations)
extraLibMap = lens extraLibMap_ (\ a b -> b {extraLibMap_ = a})

-- | Map from cabal Build-Tool names to debian binary package names.
execMap :: Lens Atoms (Map String Relations)
execMap = lens execMap_ (\ a b -> b {execMap_ = a})

maintainerOption :: Lens Atoms (Maybe NameAddr)
maintainerOption = lens maintainerOption_ (\ a b -> b {maintainerOption_ = a})

uploadersOption :: Lens Atoms [NameAddr]
uploadersOption = lens uploadersOption_ (\ a b -> b {uploadersOption_ = a})

-- | The result of loading a .cabal file
packageDescription :: Lens Atoms PackageDescription
packageDescription = lens packageDescription_ (\ a b -> b {packageDescription_ = a})

-- | Map from cabal version number ranges to debian package names.  This is a
-- result of the fact that only one version of a debian package can be
-- installed at a given time, while multiple versions of a cabal package can.
debianNameMap :: Lens Atoms (Map PackageName VersionSplits)
debianNameMap = lens debianNameMap_ (\ a b -> b {debianNameMap_ = a})

-- | Map of Debian epoch numbers assigned to cabal packages.
epochMap :: Lens Atoms (Map PackageName Int)
epochMap = lens epochMap_ (\ a b -> b {epochMap_ = a})

-- | Create a package to hold a cabal executable
executable :: Lens Atoms (Map BinPkgName InstallFile)
executable = lens executable_ (\ a b -> b {executable_ = a})

-- | Create a package for a server
serverInfo :: Lens Atoms (Map BinPkgName Server)
serverInfo = lens serverInfo_ (\ a b -> b {serverInfo_ = a})

-- | Create a package for a website
website :: Lens Atoms (Map BinPkgName Site)
website = lens website_ (\ a b -> b {website_ = a})

-- | Create a package for a timed backup script
backups :: Lens Atoms (Map BinPkgName String)
backups = lens backups_ (\ a b -> b {backups_ = a})

-- | Create an apache configuration file with the given
-- (domain, logdir, filetext).  This is called when expanding
-- the result of the website lens above.
apacheSite :: Lens Atoms (Map BinPkgName (String, FilePath, Text))
apacheSite = lens apacheSite_ (\ a b -> b {apacheSite_ = a})

-- * Lower level hints about the debianization


-- | List if packages that should be omitted from any
-- dependency list - e.g. a profiling package missing due
-- to use of noProfilingPackage lens elsewhere.
missingDependencies :: Lens Atoms (Set BinPkgName)
missingDependencies = lens missingDependencies_ (\ a b -> b {missingDependencies_ = a})

-- | Override the package name used to hold left over data files and executables.
utilsPackageNameBase :: Lens Atoms (Maybe String)
utilsPackageNameBase = lens utilsPackageNameBase_ (\ a b -> b {utilsPackageNameBase_ = a})

-- | Override the debian source package name constructed from the cabal name
sourcePackageName :: Lens Atoms (Maybe SrcPkgName)
sourcePackageName = lens sourcePackageName_ (\ a b -> b {sourcePackageName_ = a})

-- | Override the default base of the debian binary package names.
overrideDebianNameBase :: Lens Atoms (Maybe DebBase)
overrideDebianNameBase = lens overrideDebianNameBase_ (\ a b -> b {overrideDebianNameBase_ = a})

-- | Revision string used in constructing the debian verison number from the cabal version
revision :: Lens Atoms (Maybe String)
revision = lens revision_ (\ a b -> b {revision_ = a})

-- | Exact debian version number, overrides the version generated from the cabal version
debVersion :: Lens Atoms (Maybe DebianVersion)
debVersion = lens debVersion_ (\ b a -> a {debVersion_ = b})

-- | No longer sure what the purpose of this lens is.
packageInfo :: Lens Atoms (Map PackageName PackageInfo)
packageInfo = lens packageInfo_ (\ a b -> b {packageInfo_ = a})

-- | Do not put the version dependencies on the prof packages that we put on the dev packages.
omitProfVersionDeps :: Lens Atoms Bool
omitProfVersionDeps = lens omitProfVersionDeps_ (\ b a -> a {omitProfVersionDeps_ = b})

-- | Set this to filter any less-than dependencies out of the generated debian
-- dependencies.  (Not sure if this is implemented.)
omitLTDeps :: Lens Atoms Bool
omitLTDeps = lens omitLTDeps_ (\ b a -> a {omitLTDeps_ = b})

-- | Set this to omit the prof library deb.
noProfilingLibrary :: Lens Atoms Bool
noProfilingLibrary = lens noProfilingLibrary_ (\ b a -> a {noProfilingLibrary_ = b})

-- | Set this to omit the doc library deb.
noDocumentationLibrary :: Lens Atoms Bool
noDocumentationLibrary = lens noDocumentationLibrary_ (\ b a -> a {noDocumentationLibrary_ = b})

-- | Set this to apply offical Debian settings
official :: Lens Atoms Bool
official = lens official_ (\ b a -> a {official_ = b})

-- | The copyright information from the cabal file
copyright :: Lens Atoms (PackageDescription -> IO CopyrightDescription)
copyright = lens copyright_ (\ a b -> b {copyright_ = a})

{-
-- | The license information from the cabal file
license :: Lens Atoms (Maybe License)
license = lens license_ (\ a b -> b {license_ = a})

-- | The value in the cabal file's license-file field
licenseFile :: Lens Atoms (Maybe Text)
licenseFile = lens licenseFile_ (\ a b -> b {licenseFile_ = a})
-}

-- | The architectures supported by this source package - @Any@,
-- @All@, or some list of specific architectures.
sourceArchitectures :: Lens Atoms (Maybe PackageArchitectures)
sourceArchitectures = lens sourceArchitectures_ (\ a b -> b {sourceArchitectures_ = a})

-- | Extra install dependencies for the devel library.  Redundant
-- with depends, but kept for backwards compatibility.  Also, I
-- think maybe this is or was needed because it can be set before
-- the exact name of the library package is known.
extraDevDeps :: Lens Atoms Relations
extraDevDeps = lens extraDevDeps_ (\ a b -> b {extraDevDeps_ = a})

-- | The rules file header
rulesHead :: Lens Atoms (Maybe Text)
rulesHead = lens rulesHead_ (\ a b -> b {rulesHead_ = a})

-- | The rules file assignments
rulesSettings :: Lens Atoms [Text]
rulesSettings = lens rulesSettings_ (\ a b -> b {rulesSettings_ = a})

-- | The rules file include directives
rulesIncludes :: Lens Atoms [Text]
rulesIncludes = lens rulesIncludes_ (\ a b -> b {rulesIncludes_ = a})

-- | Additional fragments of the rules file
rulesFragments :: Lens Atoms (Set Text)
rulesFragments = lens rulesFragments_ (\ a b -> b {rulesFragments_ = a})

-- | Map of @debian/postinst@ scripts
postInst :: Lens Atoms (Map BinPkgName Text)
postInst = lens postInst_ (\ a b -> b {postInst_ = a})

-- | Map of @debian/postrm@ scripts
postRm :: Lens Atoms (Map BinPkgName Text)
postRm = lens postRm_ (\ a b -> b {postRm_ = a})

-- | Map of @debian/preinst@ scripts
preInst :: Lens Atoms (Map BinPkgName Text)
preInst = lens preInst_ (\ a b -> b {preInst_ = a})

-- | Map of @debian/prerm@ scripts
preRm :: Lens Atoms (Map BinPkgName Text)
preRm = lens preRm_ (\ a b -> b {preRm_ = a})

-- | The @debian/compat@ file, contains the minimum compatible version
-- of the @debhelper@ package.  If not given the version number of the
-- installed debhelper package is used.
compat :: Lens Atoms (Maybe Int)
compat = lens compat_ (\ a b -> b {compat_ = a})

-- | The @debian\/source\/format@ file.
sourceFormat :: Lens Atoms (Maybe SourceFormat)
sourceFormat = lens sourceFormat_ (\ a b -> b {sourceFormat_ = a})

-- | the @debian\/watch@ file
watch :: Lens Atoms (Maybe Text)
watch = lens watch_ (\ a b -> b {watch_ = a})

-- | the @debian\/changelog@ file
changelog :: Lens Atoms (Maybe ChangeLog)
changelog = lens changelog_ (\ a b -> b {changelog_ = a})

xDescription :: Lens Atoms (Maybe Text)
xDescription = lens xDescription_ (\ a b -> b {xDescription_ = a})

-- | Comment entries for the latest changelog entry (DebLogComments [[Text]])
comments :: Lens Atoms (Maybe [[Text]])
comments = lens comments_ (\ a b -> b {comments_ = a})

-- | The @debian\/control@ file.  Many of the following lenses access parts of the @SourceDebDescription@.
control :: Lens Atoms S.SourceDebDescription
control = lens control_ (\ a b -> b {control_ = a})

-- | Add a stanza to the binary package's logrotate script.
logrotateStanza :: Lens Atoms (Map BinPkgName (Set Text))
logrotateStanza = lens logrotateStanza_ (\ a b -> b {logrotateStanza_ = a})

-- | Access the set of new style atoms.
atomSet :: Lens Atoms (Set Atom)
atomSet = lens atomSet_ (\ a b -> b {atomSet_ = a})

-- We need (%=_)
link :: Monad m => BinPkgName -> FilePath -> FilePath -> StateT Atoms m ()
link b from dest = atomSet %= (Set.insert $ Link b from dest) >> return ()
install :: Monad m => BinPkgName -> FilePath -> FilePath -> StateT Atoms m ()
install b from dest = atomSet %= (Set.insert $ Install b from dest) >> return ()
installTo :: Monad m => BinPkgName -> FilePath -> FilePath -> StateT Atoms m ()
installTo b from dest = atomSet %= (Set.insert $ InstallTo b from dest) >> return ()
installData :: Monad m => BinPkgName -> FilePath -> FilePath -> StateT Atoms m ()
installData b from dest = atomSet %= (Set.insert $ InstallData b from dest) >> return ()
file :: Monad m => BinPkgName -> FilePath -> Text -> StateT Atoms m ()
file b dest content = atomSet %= (Set.insert $ File b dest content) >> return ()
installCabalExec :: Monad m => BinPkgName -> String -> FilePath -> StateT Atoms m ()
installCabalExec b name dest = atomSet %= (Set.insert $ InstallCabalExec b name dest) >> return ()
installCabalExecTo :: Monad m => BinPkgName -> String -> FilePath -> StateT Atoms m ()
installCabalExecTo b name dest = atomSet %= (Set.insert $ InstallCabalExecTo b name dest) >> return ()
installDir :: Monad m => BinPkgName -> FilePath -> StateT Atoms m ()
installDir b dir = atomSet %= (Set.insert $ InstallDir b dir) >> return ()

-- | Create an /etc/init.d file in the package
-- FIXME: change signature to BinPkgName -> Lens Atoms Text
installInit :: Lens Atoms (Map BinPkgName Text)
installInit = lens installInit_ (\ a b -> b {installInit_ = a})

-- | Create a file in the debianization.  This is used to implement the file lens above.
-- FIXME: change signature to BinPkgName -> Lens Atoms (Set (FilePath, Text))
intermediateFiles :: Lens Atoms (Set (FilePath, Text))
intermediateFiles = lens intermediateFiles_ (\ a b -> b {intermediateFiles_ = a})

{-
compilerFlavor :: Monad m => StateT Atoms m CompilerFlavor
compilerFlavor = do
#if MIN_VERSION_Cabal(1,20,0)
  CompilerId x _ _ <- access ghcVersion
#else
  CompilerId x _ <- access ghcVersion
#endif
  return x
-}
