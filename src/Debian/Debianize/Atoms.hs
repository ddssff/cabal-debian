-- | This module holds a long list of lenses that access the Atoms
-- record, the record that holds the input data from which the
-- debianization is to be constructed.
{-# LANGUAGE CPP, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.Atoms
    ( Atoms
    , newAtoms
    -- , makeAtoms
    , PackageInfo(PackageInfo, cabalName, devDeb, docDeb, profDeb)
    , Site(Site, domain, server, serverAdmin)
    , Server(Server, headerMessage, hostname, installFile, port, retry,
       serverFlags)
    , InstallFile(InstallFile, destDir, destName, execName, sourceDir)
    , showAtoms
    , debInfo
    , buildDir
    , extraLibMap
    , execMap
    , maintainerOption
    , uploadersOption
    , packageDescription
    , debianNameMap
    , epochMap
    , executable
    , serverInfo
    , website
    , backups
    , apacheSite
    , missingDependencies
    , utilsPackageNameBase
    , sourcePackageName
    , overrideDebianNameBase
    , revision
    , debVersion
    , packageInfo
    , omitProfVersionDeps
    , omitLTDeps
    , noProfilingLibrary
    , noDocumentationLibrary
    , official
    , sourceArchitectures
    , extraDevDeps
    , xDescription
    , comments
    , binaryArchitectures
    , sourcePriority
    , sourceSection
    , binaryPriorities
    , binarySections
    ) where

import Data.Generics (Data, Typeable)
import Data.Lens.Template (nameMakeLens)
import Data.List (init)
import Data.Map as Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set as Set (Set)
import Data.Text (Text)
import Debian.Debianize.DebInfo (DebInfo, makeDebInfo)
import Debian.Debianize.InputCabalPackageDescription (Flags, inputCabalization)
import Debian.Debianize.BinaryDebDescription (Canonical(canonical))
import Debian.Debianize.VersionSplits (DebBase, VersionSplits)
import Debian.Orphans ()
import Debian.Policy (PackageArchitectures, PackagePriority, Section)
import Debian.Relation (BinPkgName, Relations, SrcPkgName)
import Debian.Version (DebianVersion)
import Distribution.Package (PackageName)
import Distribution.PackageDescription as Cabal (PackageDescription)
import Prelude hiding ((.), init, init, log, log)
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
      , debianNameMap_ :: Map PackageName VersionSplits
      -- ^ Mapping from cabal package name and version to debian source
      -- package name.  This allows different ranges of cabal versions to
      -- map to different debian source package names.
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
      , xDescription_ :: Maybe Text
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
      , epochMap_ :: Map PackageName Int
      -- ^ Specify epoch numbers for the debian package generated from a
      -- cabal package.  Example: @EpochMapping (PackageName "HTTP") 1@.
      , packageInfo_ :: Map PackageName PackageInfo
      -- ^ Supply some info about a cabal package.
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
      , packageDescription_ :: PackageDescription
      -- ^ The result of reading a cabal configuration file.
      , official_ :: Bool
      -- ^ Whether this packaging is created by the Debian Haskell Group
      , debInfo_ :: DebInfo
      -- ^ Information required to represent a non-cabal debianization.
      } deriving (Show, Data, Typeable)

instance Canonical Atoms where
    canonical x = x {debInfo_ = canonical (debInfo_ x)}

newAtoms :: Flags -> IO Atoms
newAtoms flags' = do
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
      , debianNameMap_ = mempty
      , sourcePackageName_ = Nothing
      , overrideDebianNameBase_ = Nothing
      , revision_ = Nothing
      , debVersion_ = Nothing
      , maintainerOption_ = Nothing
      , uploadersOption_ = []
      , debInfo_ = makeDebInfo fs
      , utilsPackageNameBase_ = Nothing
      , xDescription_ = Nothing
      , comments_ = Nothing
      , missingDependencies_ = mempty
      , extraLibMap_ = mempty
      , execMap_ = mempty
      , epochMap_ = mempty
      , packageInfo_ = mempty
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

$(let f s = case s of
              (_ : _) | last s == '_' -> Just (init s)
              _ -> Nothing in
  nameMakeLens ''Atoms f)
