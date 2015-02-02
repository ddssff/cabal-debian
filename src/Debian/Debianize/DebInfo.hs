-- | This module holds a long list of lenses that access the Atoms
-- record, the record that holds the input data from which the
-- debianization is to be constructed.
{-# LANGUAGE CPP, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.DebInfo

    ( DebInfo(DebInfo, atomSet_, changelog_, compat_, control_,
        copyright_, flags_, installInit_, intermediateFiles_,
        logrotateStanza_, postInst_, postRm_, preInst_, preRm_,
        rulesFragments_, rulesHead_, rulesIncludes_, rulesSettings_,
        sourceFormat_, warning_, watch_)
    , Atom(File, Install, InstallCabalExec, InstallCabalExecTo,
     InstallData, InstallDir, InstallTo, Link)
    , makeDebInfo
    , flags
    , warning
    , copyright
    , rulesHead
    , rulesSettings
    , rulesIncludes
    , rulesFragments
    , postInst
    , postRm
    , preInst
    , preRm
    , sourceFormat
    , watch
    , changelog
    , control
    , logrotateStanza
    , atomSet
    , installInit
    , intermediateFiles
    , compat
    , link
    , install
    , installTo
    , installData
    , file
    , installCabalExec
    , installCabalExecTo
    , installDir
    ) where
import Control.Monad.State (StateT)
import Data.Generics (Data, Typeable)
import Data.Lens.Lazy ((%=), lens, Lens)
import Data.Map as Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set as Set (insert, Set)
import Data.Text (Text)
import Debian.Changes (ChangeLog)
import Debian.Debianize.InputCabalPackageDescription (Flags)
import Debian.Debianize.Types.BinaryDebDescription (Canonical(canonical))
import Debian.Debianize.Types.CopyrightDescription (CopyrightDescription, defaultCopyrightDescription, newCopyrightDescription)
import qualified Debian.Debianize.Types.SourceDebDescription as S (newSourceDebDescription, SourceDebDescription)
import Debian.Orphans ()
import Debian.Policy (SourceFormat)
import Debian.Relation (BinPkgName)
import Distribution.PackageDescription as Cabal (PackageDescription)
import Prelude hiding ((.), init, init, log, log)

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
      -- ^ Write debian/watch
      , rulesHead_ :: Maybe Text
      , rulesSettings_ :: [Text]
      , rulesIncludes_ :: [Text]
      -- ^ The header of the debian/rules file.  The remainder is assembled
      -- from DebRulesFragment values in the atom list.
      , rulesFragments_ :: Set Text
      -- ^ A Fragment of debian/rules
      , copyright_ :: PackageDescription -> IO CopyrightDescription
      -- ^ Copyright and license information.  This function takes a list of FilePath like
      -- the licenseFiles field of the PackageDescription and returns a CopyrightDescription.
      , control_ :: S.SourceDebDescription
      -- ^ The parsed contents of the control file
      , intermediateFiles_ :: Set (FilePath, Text)
      -- ^ Put this text into a file with the given name in the debianization.
      , compat_ :: Maybe Int
      -- ^ The debhelper compatibility level, from debian/compat.
      -- , copyright_ :: Maybe (Either CopyrightDescription Text)
      , changelog_ :: Maybe ChangeLog
      -- ^ The changelog, first entry contains the source package name and version
      , installInit_ :: Map BinPkgName Text
      -- ^ Add an init.d file to the binary package
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
      , atomSet_ :: Set Atom
      -- ^ set of items describing file installation requests
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
    , copyright_ = defaultCopyrightDescription newCopyrightDescription
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
    }

-- | Obsolete record containing verbosity, dryRun, validate, and debAction.
flags :: Lens DebInfo Flags
flags = lens flags_ (\ b a -> a {flags_ = b})

warning :: Lens DebInfo (Set Text)
warning = lens warning_ (\ a b -> b {warning_ = a})

-- | The copyright information from the cabal file
copyright :: Lens DebInfo (PackageDescription -> IO CopyrightDescription)
copyright = lens copyright_ (\ a b -> b {copyright_ = a})

-- | The rules file header
rulesHead :: Lens DebInfo (Maybe Text)
rulesHead = lens rulesHead_ (\ a b -> b {rulesHead_ = a})

-- | The rules file assignments
rulesSettings :: Lens DebInfo [Text]
rulesSettings = lens rulesSettings_ (\ a b -> b {rulesSettings_ = a})

-- | The rules file include directives
rulesIncludes :: Lens DebInfo [Text]
rulesIncludes = lens rulesIncludes_ (\ a b -> b {rulesIncludes_ = a})

-- | Additional fragments of the rules file
rulesFragments :: Lens DebInfo (Set Text)
rulesFragments = lens rulesFragments_ (\ a b -> b {rulesFragments_ = a})

-- | Map of @debian/postinst@ scripts
postInst :: Lens DebInfo (Map BinPkgName Text)
postInst = lens postInst_ (\ a b -> b {postInst_ = a})

-- | Map of @debian/postrm@ scripts
postRm :: Lens DebInfo (Map BinPkgName Text)
postRm = lens postRm_ (\ a b -> b {postRm_ = a})

-- | Map of @debian/preinst@ scripts
preInst :: Lens DebInfo (Map BinPkgName Text)
preInst = lens preInst_ (\ a b -> b {preInst_ = a})

-- | Map of @debian/prerm@ scripts
preRm :: Lens DebInfo (Map BinPkgName Text)
preRm = lens preRm_ (\ a b -> b {preRm_ = a})

-- | The @debian\/source\/format@ file.
sourceFormat :: Lens DebInfo (Maybe SourceFormat)
sourceFormat = lens sourceFormat_ (\ a b -> b {sourceFormat_ = a})

-- | the @debian\/watch@ file
watch :: Lens DebInfo (Maybe Text)
watch = lens watch_ (\ a b -> b {watch_ = a})

-- | the @debian\/changelog@ file
changelog :: Lens DebInfo (Maybe ChangeLog)
changelog = lens changelog_ (\ a b -> b {changelog_ = a})

-- | The @debian\/control@ file.  Many of the following lenses access parts of the @SourceDebDescription@.
control :: Lens DebInfo S.SourceDebDescription
control = lens control_ (\ a b -> b {control_ = a})

instance Canonical DebInfo where
    canonical x = x {control_ = canonical (control_ x)}

-- | Add a stanza to the binary package's logrotate script.
logrotateStanza :: Lens DebInfo (Map BinPkgName (Set Text))
logrotateStanza = lens logrotateStanza_ (\ a b -> b {logrotateStanza_ = a})

-- | Access the set of new style atoms.
atomSet :: Lens DebInfo (Set Atom)
atomSet = lens atomSet_ (\ a b -> b {atomSet_ = a})

-- | Create an /etc/init.d file in the package
-- FIXME: change signature to BinPkgName -> Lens Atoms Text
installInit :: Lens DebInfo (Map BinPkgName Text)
installInit = lens installInit_ (\ a b -> b {installInit_ = a})

-- | Create a file in the debianization.  This is used to implement the file lens above.
-- FIXME: change signature to BinPkgName -> Lens Atoms (Set (FilePath, Text))
intermediateFiles :: Lens DebInfo (Set (FilePath, Text))
intermediateFiles = lens intermediateFiles_ (\ a b -> b {intermediateFiles_ = a})

-- | The @debian/compat@ file, contains the minimum compatible version
-- of the @debhelper@ package.  If not given the version number of the
-- installed debhelper package is used.
compat :: Lens DebInfo (Maybe Int)
compat = lens compat_ (\ a b -> b {compat_ = a})

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
