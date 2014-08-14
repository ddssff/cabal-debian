-- | This module holds a long list of lenses that access the Atoms
-- record, the record that holds the input data from which the
-- debianization is to be constructed.
{-# LANGUAGE CPP, DeriveDataTypeable #-}
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
import Control.Category ((.))
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Generics (Data, Typeable)
import Data.Lens.Lazy (Lens, lens)
import Data.Map as Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set as Set (Set, singleton)
import Data.Text (Text)
import Debian.Changes (ChangeLog)
import qualified Debian.Debianize.Types.SourceDebDescription as S
import Debian.Debianize.VersionSplits (VersionSplits)
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures, PackagePriority, Section, SourceFormat)
import Debian.Relation (BinPkgName, Relations, SrcPkgName)
import Debian.Version (DebianVersion)
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.License (License)
import Distribution.Package (PackageName)
import Distribution.PackageDescription as Cabal (FlagName, PackageDescription)
import Prelude hiding (init, init, log, log, unlines, (.))
import System.Console.GetOpt (getOpt, ArgOrder(Permute), OptDescr(Option), ArgDescr(ReqArg))
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

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
      , noHoogle_ :: Bool
      -- ^ Don't link the documentation for hoogle.
      , omitLTDeps_ :: Bool
      -- ^ If present, don't generate the << dependency when we see a cabal
      -- equals dependency.  (The implementation of this was somehow lost.)
      , buildDir_ :: Set FilePath
      -- ^ The build directory used by cabal, typically dist/build when
      -- building manually or dist-ghc/build when building using GHC and
      -- haskell-devscripts.  This value is used to locate files
      -- produced by cabal so we can move them into the deb.  Note that
      -- the --builddir option of runhaskell Setup appends the "/build"
      -- to the value it receives, so, yes, try not to get confused.
      -- FIXME: make this FilePath or Maybe FilePath
      , buildEnv_ :: EnvSet
      -- ^ Directory containing the build environment for which the
      -- debianization will be generated.  This determines which
      -- compiler will be available, which in turn determines which
      -- basic libraries can be provided by the compiler.  By default
      -- all the paths in EnvSet are "/".
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
      , maintainerOld_ :: Maybe NameAddr
      -- ^ Value for the maintainer field in the control file.  Note that
      -- the cabal maintainer field can have multiple addresses, but debian
      -- only one.  If this is not explicitly set, it is obtained from the
      -- cabal file, and if it is not there then from the environment.  As a
      -- last resort, there is a hard coded string in here somewhere.
      , cabalFlagAssignments_ :: Set (FlagName, Bool)
      -- ^ Flags to pass to Cabal function finalizePackageDescription,
      -- this can be used to control the flags in the cabal file.  It
      -- can be supplied to the cabal-debian binary using the --flags
      -- option.
      , sourceFormat_ :: Maybe SourceFormat
      -- ^ Write debian/source/format
      , watch_ :: Maybe Text
      -- ^ Write debian/watch
      , intermediateFiles_ :: Set (FilePath, Text)
      -- ^ Put this text into a file with the given name in the debianization.
      , rulesHead_ :: Maybe Text
      -- ^ The header of the debian/rules file.  The remainder is assembled
      -- from DebRulesFragment values in the atom list.
      , rulesFragments_ :: Set Text
      -- ^ A Fragment of debian/rules
      , warning_ :: Set Text
      -- ^ A warning to be reported later
      , utilsPackageNameBase_ :: Maybe String
      -- ^ Name of a package that will get left-over data files and executables.
      -- If there are more than one, each package will get those files.
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
      , copyright_ :: Maybe Text
      -- ^ Copyright information
      , license_ :: Maybe License
      -- ^ License information Cabal License value
      , licenseFile_ :: Maybe Text
      -- ^ The contents of the file specified in the cabal license-file: field
      , apacheSite_ :: Map BinPkgName (String, FilePath, Text)
      -- ^ Have Apache configure a site using PACKAGE, DOMAIN, LOGDIR, and APACHECONFIGFILE
      , logrotateStanza_ :: Map BinPkgName (Set Text)
      -- ^ Add a stanza of a logrotate file to the binary package
      , link_ :: Map BinPkgName (Set (FilePath, FilePath))
      -- ^ Create a symbolic link in the binary package
      , postInst_ :: Map BinPkgName Text
      -- ^ Script to run after install, should contain #DEBHELPER# line before exit 0
      , postRm_ :: Map BinPkgName Text
      -- ^ Script to run after remove, should contain #DEBHELPER# line before exit 0
      , preInst_ :: Map BinPkgName Text
      -- ^ Script to run before install, should contain #DEBHELPER# line before exit 0
      , preRm_ :: Map BinPkgName Text
      -- ^ Script to run before remove, should contain #DEBHELPER# line before exit 0
      , sourceArchitecture_ :: Maybe PackageArchitectures
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
      , install_ :: Map BinPkgName (Set (FilePath, FilePath))
      -- ^ Install a build file into the binary package
      , installTo_ :: Map BinPkgName (Set (FilePath, FilePath))
      -- ^ Install a build file into the binary package at an exact location
      , installData_ :: Map BinPkgName (Set (FilePath, FilePath))
      -- ^ DHInstallTo somewhere relative to DataDir (see above)
      , file_ :: Map BinPkgName (Set (FilePath, Text))
      -- ^ Create a file with the given text at the given path
      , installCabalExec_ :: Map BinPkgName (Set (String, FilePath))
      -- ^ Install a cabal executable into the binary package
      , installCabalExecTo_ :: Map BinPkgName (Set (String, FilePath))
      -- ^ Install a cabal executable into the binary package at an exact location
      , installDir_ :: Map BinPkgName (Set FilePath)
      -- ^ Create a directory in the binary package
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
      , packageDescription_ :: Maybe PackageDescription
      -- ^ The result of reading a cabal configuration file.
      , compilerFlavors_ :: Set CompilerFlavor
      -- ^ Which compilers should we generate library packages for?
      } deriving (Eq, Show, Data, Typeable)

data EnvSet = EnvSet
    { cleanOS :: FilePath  -- ^ The output of the debootstrap command
    , dependOS :: FilePath -- ^ An environment with build dependencies installed
    , buildOS :: FilePath  -- ^ An environment where we have built a package
    } deriving (Eq, Show, Data, Typeable)

-- | Look for --buildenvdir in the command line arguments to get the
-- changeroot path, use "/" if not present.
newAtoms :: MonadIO m => m Atoms
newAtoms = liftIO $ do
  (roots, _, _) <- getOpt Permute [Option "buildenvdir" [] (ReqArg id "PATH")
                                          "Directory containing the build environment"] <$> getArgs
  let envset = case roots of
                 (x : _) -> EnvSet {cleanOS = x </> "clean", dependOS = x </> "depend", buildOS = x </> "build"}
                 _ -> EnvSet {cleanOS = "/", dependOS = "/", buildOS = "/"}
  return $ makeAtoms envset

makeAtoms :: EnvSet -> Atoms
makeAtoms envset =
    Atoms
      { noDocumentationLibrary_ = False
      , noProfilingLibrary_ = False
      , noHoogle_ = False
      , omitLTDeps_ = False
      , buildDir_ = mempty
      , buildEnv_ = envset
      , flags_ = defaultFlags
      , debianNameMap_ = mempty
      , control_ = S.newSourceDebDescription
      , sourcePackageName_ = Nothing
      , revision_ = Nothing
      , debVersion_ = Nothing
      , maintainerOld_ = Nothing
      , cabalFlagAssignments_ = mempty
      , sourceFormat_ = Nothing
      , watch_ = Nothing
      , intermediateFiles_ = mempty
      , rulesHead_ = Nothing
      , rulesFragments_ = mempty
      , warning_ = mempty
      , utilsPackageNameBase_ = Nothing
      , changelog_ = Nothing
      , comments_ = Nothing
      , missingDependencies_ = mempty
      , extraLibMap_ = mempty
      , execMap_ = mempty
      , epochMap_ = mempty
      , packageInfo_ = mempty
      , compat_ = Nothing
      , copyright_ = Nothing
      , license_ = Nothing
      , licenseFile_ = mempty
      , apacheSite_ = mempty
      , logrotateStanza_ = mempty
      , link_ = mempty
      , postInst_ = mempty
      , postRm_ = mempty
      , preInst_ = mempty
      , preRm_ = mempty
      , sourceArchitecture_ = Nothing
      , binaryArchitectures_ = mempty
      , sourcePriority_ = Nothing
      , binaryPriorities_ = mempty
      , sourceSection_ = Nothing
      , binarySections_ = mempty
      , install_ = mempty
      , installTo_ = mempty
      , installData_ = mempty
      , file_ = mempty
      , installCabalExec_ = mempty
      , installCabalExecTo_ = mempty
      , installDir_ = mempty
      , installInit_ = mempty
      , executable_ = mempty
      , serverInfo_ = mempty
      , website_ = mempty
      , backups_ = mempty
      , extraDevDeps_ = mempty
      , packageDescription_ = Nothing
      , compilerFlavors_ = singleton GHC
      }

-- | This record supplies information about the task we want done -
-- debianization, validataion, help message, etc.
data Flags = Flags
    {
    -------------------------
    -- Modes of Operation ---
    -------------------------
      verbosity_ :: Int
    -- ^ Run with progress messages at the given level of verboseness.
    , dryRun_ :: Bool
    -- ^ Don't write any files or create any directories, just explain
    -- what would have been done.
    , validate_ :: Bool
    -- ^ Fail if the debianization already present doesn't match the
    -- one we are going to generate closely enough that it is safe to
    -- debianize during the run of dpkg-buildpackage, when Setup
    -- configure is run.  Specifically, the version number in the top
    -- changelog entry must match, and the sets of package names in
    -- the control file must match.
    , debAction_ :: DebAction
    -- ^ What to do - Usage, Debianize or Substvar
    } deriving (Eq, Ord, Show, Data, Typeable)

data DebAction = Usage | Debianize | SubstVar DebType deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | A redundant data type, too lazy to expunge.
data DebType = Dev | Prof | Doc deriving (Eq, Ord, Read, Show, Data, Typeable)

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

defaultFlags :: Flags
defaultFlags =
    Flags {
      verbosity_ = 1
    , debAction_ = Debianize
    , dryRun_ = False
    , validate_ = False
    }

showAtoms :: Atoms -> IO ()
showAtoms x = putStrLn ("\nTop: " ++ show x ++ "\n")

-- | Set how much progress messages get generated.
verbosity :: Lens Atoms Int
verbosity = lens verbosity_ (\ b a -> a {verbosity_ = b}) . flags

-- | Don't write anything, just output a description of what would have happened
dryRun :: Lens Atoms Bool
dryRun = lens dryRun_ (\ b a -> a {dryRun_ = b}) . flags

-- | Make sure the version number and package names of the supplied
-- and generated debianizations match.
validate :: Lens Atoms Bool
validate = lens validate_ (\ b a -> a {validate_ = b}) . flags

-- | Debianize, SubstVars, or Usage.  I'm no longer sure what SubstVars does, but someone
-- may still be using it.
debAction :: Lens Atoms DebAction
debAction = lens debAction_ (\ b a -> a {debAction_ = b}) . flags

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
buildDir :: Lens Atoms (Set FilePath)
buildDir = lens buildDir_ (\ b a -> a {buildDir_ = b})

-- We need to update ghcVersion when this is changed, which means doing IO
-- buildEnv :: Lens Atoms (Maybe EnvSet)
-- buildEnv = lens buildEnv_ (\ b a -> a {buildEnv_ = b})

buildEnv :: Lens Atoms EnvSet
buildEnv = lens buildEnv_ (\ b a -> a {buildEnv_ = b})

setBuildEnv :: MonadIO m => EnvSet -> Atoms -> m Atoms
setBuildEnv envset atoms = return $ atoms {buildEnv_ = envset}

-- | Map from cabal Extra-Lib names to debian binary package names.
extraLibMap :: Lens Atoms (Map String Relations)
extraLibMap = lens extraLibMap_ (\ a b -> b {extraLibMap_ = a})

-- | Map from cabal Build-Tool names to debian binary package names.
execMap :: Lens Atoms (Map String Relations)
execMap = lens execMap_ (\ a b -> b {execMap_ = a})

-- | Cabal flag assignments to use when loading the cabal file.
cabalFlagAssignments :: Lens Atoms (Set (FlagName, Bool))
cabalFlagAssignments = lens cabalFlagAssignments_ (\ a b -> b {cabalFlagAssignments_ = a})

-- | The result of loading a .cabal file
packageDescription :: Lens Atoms (Maybe PackageDescription)
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

-- | Revision string used in constructing the debian verison number from the cabal version
revision :: Lens Atoms (Maybe String)
revision = lens revision_ (\ a b -> b {revision_ = a})

-- | Exact debian version number, overrides the version generated from the cabal version
debVersion :: Lens Atoms (Maybe DebianVersion)
debVersion = lens debVersion_ (\ b a -> a {debVersion_ = b})

-- | No longer sure what the purpose of this lens is.
packageInfo :: Lens Atoms (Map PackageName PackageInfo)
packageInfo = lens packageInfo_ (\ a b -> b {packageInfo_ = a})

-- | Set this to filter any less-than dependencies out of the generated debian
-- dependencies.  (Not sure if this is implemented.)
omitLTDeps :: Lens Atoms Bool
omitLTDeps = lens omitLTDeps_ (\ b a -> a {omitLTDeps_ = b})

-- | Set this to omit the prof library deb.
noProfilingLibrary :: Lens Atoms Bool
noProfilingLibrary = lens noProfilingLibrary_ (\ b a -> a {noProfilingLibrary_ = b})

-- | Set this to omit the hoogle documentation link
noHoogle :: Lens Atoms Bool
noHoogle = lens noHoogle_ (\ b a -> a {noHoogle_ = b})

-- | Set this to omit the doc library deb.
noDocumentationLibrary :: Lens Atoms Bool
noDocumentationLibrary = lens noDocumentationLibrary_ (\ b a -> a {noDocumentationLibrary_ = b})

-- | The copyright information from the cabal file
copyright :: Lens Atoms (Maybe Text)
copyright = lens copyright_ (\ a b -> b {copyright_ = a})

-- | The license information from the cabal file
license :: Lens Atoms (Maybe License)
license = lens license_ (\ a b -> b {license_ = a})

-- | The value in the cabal file's license-file field
licenseFile :: Lens Atoms (Maybe Text)
licenseFile = lens licenseFile_ (\ a b -> b {licenseFile_ = a})

-- | The architectures supported by this source package - @Any@,
-- @All@, or some list of specific architectures.
sourceArchitectures :: Lens Atoms (Maybe PackageArchitectures)
sourceArchitectures = lens sourceArchitecture_ (\ a b -> b {sourceArchitecture_ = a})

-- | Extra install dependencies for the devel library.  Redundant
-- with depends, but kept for backwards compatibility.  Also, I
-- think maybe this is or was needed because it can be set before
-- the exact name of the library package is known.
extraDevDeps :: Lens Atoms Relations
extraDevDeps = lens extraDevDeps_ (\ a b -> b {extraDevDeps_ = a})

-- | The beginning of the rules file
rulesHead :: Lens Atoms (Maybe Text)
rulesHead = lens rulesHead_ (\ a b -> b {rulesHead_ = a})

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

-- | Comment entries for the latest changelog entry (DebLogComments [[Text]])
comments :: Lens Atoms (Maybe [[Text]])
comments = lens comments_ (\ a b -> b {comments_ = a})

-- | The @debian\/control@ file.  Many of the following lenses access parts of the @SourceDebDescription@.
control :: Lens Atoms S.SourceDebDescription
control = lens control_ (\ a b -> b {control_ = a})

-- | Add a stanza to the binary package's logrotate script.
logrotateStanza :: Lens Atoms (Map BinPkgName (Set Text))
logrotateStanza = lens logrotateStanza_ (\ a b -> b {logrotateStanza_ = a})

-- | Add entries to a binary deb's debian/foo.links file.
link :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
link = lens link_ (\ a b -> b {link_ = a})

-- | Install files into directories by adding entries to the binary
-- deb's debian/foo.install file.
install :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
install = lens install_ (\ a b -> b {install_ = a})

-- | Rename and install files.  This is done by adding rules to debian/rules.
installTo :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
installTo = lens installTo_ (\ a b -> b {installTo_ = a})

-- | Install files into the a binary deb's data directory,
-- /usr/share/packagename-version.  This expands to either an install
-- or an installTo.
installData :: Lens Atoms (Map BinPkgName (Set (FilePath, FilePath)))
installData = lens installData_ (\ a b -> b {installData_ = a})

-- | Create a file in the binary deb with the given text.  This is done by
-- writing the file into the cabalInstall directory and adding an entry
-- to the binary deb's .install file.
file :: Lens Atoms (Map BinPkgName (Set (FilePath, Text)))
file = lens file_ (\ a b -> b {file_ = a})

-- | Install a cabal executable into a binary deb.
-- FIXME: change signature to BinPkgName -> Lens Atoms (Set (String, FilePath))
installCabalExec :: Lens Atoms (Map BinPkgName (Set (String, FilePath)))
installCabalExec = lens installCabalExec_ (\ a b -> b {installCabalExec_ = a})

-- | Rename and install a cabal executable
-- FIXME: change signature to BinPkgName -> Lens Atoms (Set (String, FilePath))
installCabalExecTo :: Lens Atoms (Map BinPkgName (Set (String, FilePath)))
installCabalExecTo = lens installCabalExecTo_ (\ a b -> b {installCabalExecTo_ = a})

-- | Create directories in the package
-- FIXME: change signature to BinPkgName -> Lens Atoms (Set FilePath)
installDir :: Lens Atoms (Map BinPkgName (Set FilePath))
installDir = lens installDir_ (\ a b -> b {installDir_ = a})

-- | Create an /etc/init.d file in the package
-- FIXME: change signature to BinPkgName -> Lens Atoms Text
installInit :: Lens Atoms (Map BinPkgName Text)
installInit = lens installInit_ (\ a b -> b {installInit_ = a})

-- | Create a file in the debianization.  This is used to implement the file lens above.
-- FIXME: change signature to BinPkgName -> Lens Atoms (Set (FilePath, Text))
intermediateFiles :: Lens Atoms (Set (FilePath, Text))
intermediateFiles = lens intermediateFiles_ (\ a b -> b {intermediateFiles_ = a})

compilerFlavors :: Lens Atoms (Set CompilerFlavor)
compilerFlavors = lens compilerFlavors_ (\ a b -> b {compilerFlavors_ = a})

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
