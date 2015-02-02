-- | This module holds a long list of lenses that access the Atoms
-- record, the record that holds the input data from which the
-- debianization is to be constructed.
{-# LANGUAGE CPP, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.DebInfo

    ( DebInfo
    , Atom(File, Install, InstallCabalExec, InstallCabalExecTo, InstallData, InstallDir, InstallTo, Link)
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
import Data.Lens.Lazy ((%=))
import Data.Lens.Template (nameMakeLens)
import Data.List (init)
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
      -- ^ the @debian\/watch@ file
      , rulesHead_ :: Maybe Text
      -- ^ The rules file header
      , rulesSettings_ :: [Text]
      -- ^ The rules file assignments
      , rulesIncludes_ :: [Text]
      -- ^ The rules file include directives
      , rulesFragments_ :: Set Text
      -- ^ Additional fragments of the rules file
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
