{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Debian.Debianize.Optparse (
  CommandLineOptions(..),
  BehaviorAdjustment,
  Flags(..),
  parseProgramArguments,
  parseProgramArguments',
  handleBehaviorAdjustment) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), pure)
#endif
import Control.Applicative (many, (<|>))
import Control.Lens
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans
import Control.Newtype.Generics
import Data.Bifunctor (first)
import Data.Char(toUpper)
import Data.Foldable (forM_)
import Data.Maybe.Extended (fromMaybe)
import Data.Maybe.Extended (nothingIf)
import Data.Monoid ((<>))
import Debian.Debianize.BasicInfo (EnvSet(EnvSet), cleanOS, dependOS, buildOS, Flags(..))
import Debian.Debianize.DebInfo (TestsStatus(..))
import Debian.Debianize.Monad
import Debian.Debianize.Prelude (maybeRead)
import Debian.Debianize.VersionSplits
import Debian.GHC ()
import Debian.Policy
import Debian.Relation
import Debian.Version (DebianVersion, parseDebianVersion')
import Distribution.Compiler (CompilerFlavor(..))
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Package (PackageName, mkPackageName, unPackageName)
import Distribution.PackageDescription (FlagName, mkFlagName)
#else
import Distribution.Package (PackageName(..))
import Distribution.PackageDescription (FlagName(FlagName))
#endif
import GHC.Generics
import System.Environment (getArgs)
import System.FilePath(splitFileName, (</>))
import System.Process (showCommandForUser)
#if MIN_VERSION_hsemail(2,0,0)
import Text.Parsec.Rfc2822 (NameAddr(..))
#else
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))
#endif
import Text.PrettyPrint.ANSI.Leijen (linebreak, (<+>), string, indent)
import qualified  Debian.Debianize.DebInfo as D
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Debian.Debianize.BinaryDebDescription as B
import qualified Debian.Debianize.CabalInfo as A
import qualified Debian.Debianize.SourceDebDescription as S
import qualified Options.Applicative as O

data HaddockStatus = HaddockEnabled | HaddockDisabled deriving Eq
data ProfilingStatus = ProfilingEnabled | ProfilingDisabled deriving Eq
data OfficialStatus = Official| NonOfficial deriving Eq
newtype BuildDep = BuildDep Relations deriving Generic
instance Newtype BuildDep
newtype BuildDepIndep = BuildDepIndep Relations deriving Generic
instance Newtype BuildDepIndep
newtype DevDep = DevDep Relations deriving Generic
instance Newtype DevDep
newtype ExtraDepends = ExtraDepends (BinPkgName, Relations) deriving Generic
instance Newtype ExtraDepends
newtype ExtraConflicts = ExtraConflicts (BinPkgName, Relations) deriving Generic
instance Newtype ExtraConflicts
newtype ExtraProvides = ExtraProvides (BinPkgName, Relations) deriving Generic
instance Newtype ExtraProvides
newtype ExtraReplaces = ExtraReplaces (BinPkgName, Relations) deriving Generic
instance Newtype ExtraReplaces
newtype ExtraRecommends = ExtraRecommends (BinPkgName, Relations) deriving Generic
instance Newtype ExtraRecommends
newtype ExtraSuggests = ExtraSuggests (BinPkgName, Relations) deriving Generic
instance Newtype ExtraSuggests
newtype CabalDebMapping = CabalDebMapping (PackageName, Relations) deriving Generic
instance Newtype CabalDebMapping
newtype ExecDebMapping = ExecDebMapping (String, Relations) deriving Generic
instance Newtype ExecDebMapping
newtype Revision = Revision String deriving Generic
instance Newtype Revision
newtype CabalEpochMapping = CabalEpochMapping (PackageName, Int) deriving Generic
instance Newtype CabalEpochMapping
newtype CabalFlagMapping = CabalFlagMapping (FlagName, Bool) deriving Generic
instance Newtype CabalFlagMapping

-- | This data type is an abomination. It represent information,
-- provided on command line. Part of such information provides
-- means to create initial 'CabalT' state and is stored in
-- '_flags' field. See 'newCabalInfo'.
--
-- Other, much greater part represent changes to already created
-- state. They are stored in '_adjustment' field.
--
-- All this can be understood from (simplified) types:
--
-- > type CabalT m a = StateT CabalInfo m a
-- > newCabalInfo :: Flags -> IO CabalInfo
-- > handleBehaviorAdjustment :: BehaviorAdjustment -> CabalT IO ()

data CommandLineOptions = CommandLineOptions {
  _flags :: Flags,
  _adjustment :: BehaviorAdjustment
}
-- | This data type represents changes to 'CabalT' state,
-- requested at command line.
data BehaviorAdjustment = BehaviorAdjustment {
  _maintainer        :: NameAddr,
  _uploaders         :: [NameAddr],
  _executable        :: [(BinPkgName, D.InstallFile)],
  _defaultPackage    :: Maybe String,
  _missingDependency :: [BinPkgName],
  _debianNameBase    :: Maybe DebBase,
  _debianVersion     :: Maybe DebianVersion,
  _revision          :: Maybe Revision,
  _sourcePackageName :: Maybe SrcPkgName,
  _sourceSection     :: Section,
  _standardsVersion  :: StandardsVersion,
  _buildDep          :: [BuildDep],
  _buildDepIndep     :: [BuildDepIndep],
  _devDep            :: [DevDep],
  _extraDepends      :: [ExtraDepends],
  _extraConflicts    :: [ExtraConflicts],
  _extraProvides     :: [ExtraProvides],
  _extraReplaces     :: [ExtraReplaces],
  _extraRecommends   :: [ExtraRecommends],
  _extraSuggests     :: [ExtraSuggests],
  _cabalDebMapping   :: [CabalDebMapping],
  _cabalEpochMapping :: [CabalEpochMapping],
  _execDebMapping    :: [ExecDebMapping],
  _profiling         :: ProfilingStatus,
  _haddock           :: [HaddockStatus],
  _official          :: OfficialStatus,
  _sourceFormat      :: SourceFormat,
  _tests             :: TestsStatus
}

-- Brief instruction to save you, dear developer from scrutinizing
-- `optparse-applicative` documentation.
--
-- There is two main types in command line parsing.
--
-- 'ReadM' is description how make object from string.
-- For every object of type 'a' with some parsing logic
-- we define auxiliary function with 'R' suffix and
-- type 'ReadM a'.
--
-- 'Parser' is type, containing information about
-- which string in command line should be converted
-- to object. Every field in 'BehaviorAdjustment'
-- and 'Flags' type of type 'b' have corresponding function
-- of type 'Parser' with suffix 'P'.


-- Here are all 'ReadM' values.

executableR :: O.ReadM (BinPkgName, D.InstallFile)
executableR = parsePair . span (/= ':') <$> O.str where
  parsePair :: (String, String) -> (BinPkgName, D.InstallFile)
  parsePair (sp, md) = let (sd, name) = splitFileName sp in
    (BinPkgName name, D.InstallFile { D.execName  = name,
                                      D.destName  = name,
                                      D.sourceDir = nothingIf ( == "./") sd,
                                      D.destDir   = case md of
                                                      (':' : dd) -> Just dd
                                                      _          -> Nothing })

binPkgNameR :: O.ReadM BinPkgName
binPkgNameR = BinPkgName <$> O.str

nameAddrR :: O.ReadM NameAddr
nameAddrR = either fail return =<< parseMaintainer <$> O.str

relationsR :: O.ReadM Relations
relationsR = either (fail . show) return =<< parseRelations <$> (O.str :: O.ReadM String)

mappingR :: O.ReadM (String, Relations)
mappingR = span (/= ':') <$> O.str >>= \case
  (str, "") -> fail $ "Does not contains colon: `" ++ str ++ "'"
  (pkgstr, _ : relstr) -> do
    rels <- either (fail . show) return $ parseRelations relstr
    return (pkgstr, rels)

epochMappingR :: O.ReadM (String, Int)
epochMappingR = span (/= '=') <$> O.str >>= \case
  (pkgstr, '=' : numstr) -> do
    let epoch = fromMaybe (error ("Invalid epoch: " ++ numstr)) (maybeRead numstr :: Maybe Int)
    return (pkgstr, epoch)
  (str, _) -> fail $ "Does not contains equals: `" ++ str ++ "'"

extraRelationsR :: O.ReadM (BinPkgName, Relations)
extraRelationsR = first BinPkgName <$> mappingR

cabalDebMappingR :: O.ReadM CabalDebMapping
#if MIN_VERSION_Cabal(2,0,0)
cabalDebMappingR = CabalDebMapping . first mkPackageName <$> mappingR
#else
cabalDebMappingR = CabalDebMapping . first PackageName <$> mappingR
#endif

cabalEpochMappingR :: O.ReadM CabalEpochMapping
#if MIN_VERSION_Cabal(2,0,0)
cabalEpochMappingR = CabalEpochMapping . first mkPackageName <$> epochMappingR
#else
cabalEpochMappingR = CabalEpochMapping . first PackageName <$> epochMappingR
#endif

cabalFlagMappingR :: O.ReadM CabalFlagMapping
cabalFlagMappingR = O.str >>= \case
#if MIN_VERSION_Cabal(2,0,0)
  ('-' : str) -> return $ CabalFlagMapping (mkFlagName str, False)
  str -> return $ CabalFlagMapping (mkFlagName str, True)
#else
  ('-' : str) -> return $ CabalFlagMapping (FlagName str, False)
  str -> return $ CabalFlagMapping (FlagName str, True)
#endif

-- Here are parser for BehaviorAdjustment and next are parsers for
-- every field of this data.  Please, keep parsers declarations in
-- same order, as are fields.

behaviorAdjustmentP :: O.Parser BehaviorAdjustment
behaviorAdjustmentP = BehaviorAdjustment <$> maintainerP
                                         <*> uploadersP
                                         <*> executableP
                                         <*> defaultPackageP
                                         <*> missingDependencyP
                                         <*> debianNameBaseP
                                         <*> debianVersionP
                                         <*> debianRevisionP
                                         <*> sourcePackageNameP
                                         <*> sourceSectionP
                                         <*> standardsVersionP
                                         <*> buildDepP
                                         <*> buildDepIndepP
                                         <*> devDepP
                                         <*> extraDependsP
                                         <*> extraConflictsP
                                         <*> extraProvidesP
                                         <*> extraReplacesP
                                         <*> extraRecommendsP
                                         <*> extraSuggestsP
                                         <*> cabalDebMappingP
                                         <*> cabalEpochMappingP
                                         <*> execDebMappingP
                                         <*> profilingP
                                         <*> haddockP
                                         <*> officialP
                                         <*> sourceFormatP
                                         <*> testsP

maintainerP :: O.Parser NameAddr
maintainerP = O.option nameAddrR m where
  m = O.help helpMsg
      <> O.long "maintainer"
      <> O.short 'm'
      <> O.value (NameAddr (Just "Debian Haskell Group")
                           "pkg-haskell-maintainers@lists.alioth.debian.org")
      <> O.metavar "'NAME <EMAIL>'"
  helpMsg = "Set the `Maintainer' field in debian/control file."

uploadersP :: O.Parser [NameAddr]
uploadersP = many $ O.option nameAddrR m where
  m = O.help helpMsg
      <> O.long "uploader"
      <> O.short 'u'
      <> O.metavar "'NAME <EMAIL>'"
  helpMsg = "Add entry to `Uploaders' field in debian/control file."

executableP :: O.Parser [(BinPkgName, D.InstallFile)]
executableP = many $ O.option executableR m where
  m = O.help helpMsg
      <> O.long "executable"
      <> O.short 'e'
      <> O.metavar "SOURCEPATH[:DESTDIR]"
  helpMsg = unlines [
   "Create an individual binary package to hold this executable.",
   "Other executables and data files are gathered into a single package",
   "named `haskell-PACKAGENAME-utils'"
   ]

defaultPackageP :: O.Parser (Maybe String)
defaultPackageP = O.option (Just <$> O.str) m where
  m = O.help helpMsg
      <> O.long "default-package"
      <> O.short 'd'
      <> O.value Nothing
      <> O.metavar "PKGNAME"
  helpMsg = unlines [
    "Set the name of the catch-all package that receives",
    "all the files not included in a library package or some",
    "other executable package. By default this is `haskell-PACKAGENAME-utils'"
    ]

missingDependencyP :: O.Parser [BinPkgName]
missingDependencyP = many $ O.option binPkgNameR m where
  m = O.help helpMsg
      <> O.long "missing-dependency"
      <> O.metavar "DEB"
  helpMsg = unlines [
    "This is the counterpart to --disable-haddock.  It prevents a package",
    "from being added to the build dependencies.  This is necessary,",
    "for example, when a dependency package was built with the",
    "--disable-haddock option, because normally cabal-debian assumes",
    "that the -doc package exists and adds it as a build dependency."
    ]

debianNameBaseP :: O.Parser (Maybe DebBase)
debianNameBaseP = O.option (Just . DebBase <$> O.str) m where
  m = O.help helpMsg
      <> O.long "debian-name-base"
      <> O.short 'b'
      <> O.value Nothing
      <> O.metavar "NAME"
  helpMsg = unlines [
    "Use this name for the base of the debian binary packages - the string between",
    "'libghc-' and '-dev'. Normally this is derived from the hackage package name."
    ]

debianVersionP :: O.Parser (Maybe DebianVersion)
debianVersionP = O.option (Just . parseDebianVersion' <$> (O.str :: O.ReadM String)) m where
  m = O.help helpMsg
      <> O.long "deb-version"
      <> O.metavar "DEBIANVERSION"
      <> O.value Nothing
  helpMsg = unlines [
    "Specify the version number for the debian package.",
    "This will pin the version and should be considered dangerous."
    ]

debianRevisionP :: O.Parser (Maybe Revision)
debianRevisionP = O.option (Just . Revision <$> O.str) m where
  m = O.help helpMsg
      <> O.long "revision"
      <> O.value Nothing
      <> O.metavar "DEBIANREVISION"
  helpMsg = unlines [
    "Add this string to the cabal version to get the debian version number.",
    "Debian policy says this must either be empty (--revision '')",
    "or begin with a dash."
    ]

sourcePackageNameP :: O.Parser (Maybe SrcPkgName)
sourcePackageNameP = O.option (Just . SrcPkgName <$> O.str) m where
  m = O.help helpMsg
      <> O.long "source-package-name"
      <> O.short 's'
      <> O.value Nothing
      <> O.metavar "DEBIANNAME"
  helpMsg = unlines [
    "Use this name for the debian source package, the name in the Source field",
    "at the top of the debian/control file, and also at the very beginning",
    "of the debian/changelog file.  By default it is haskell-<cabalname>,",
    "where the cabal package name is downcased."
    ]

sourceSectionP :: O.Parser Section
sourceSectionP = O.option (MainSection <$> O.str) m where
  m = O.help helpMsg
      <> O.long "source-section"
      <> O.short 'S'
      <> O.value (MainSection "haskell")
      <> O.metavar "SECTION"
  helpMsg = "Set the `Section' field in debian/control file."

standardsVersionP :: O.Parser StandardsVersion
standardsVersionP = O.option (parseStandardsVersion <$> O.str) m where
  m = O.help helpMsg
      <> O.long "standards-version"
      <> O.value (parseStandardsVersion "4.4.1")
      <> O.metavar "CABALVERSION"
  helpMsg = unlines [
    "Claim compatibility to this version of the Debian policy",
    "(i.e. the value of the Standards-Version field)"
    ]

buildDepP :: O.Parser [BuildDep]
buildDepP = many $ O.option (BuildDep <$> relationsR) m where
  m = O.help helpMsg
      <> O.long "build-dep"
      <> O.metavar "DEBIANRELATIONS"
  helpMsg = unlines [
    "Add a dependency relation to the `Build-Depends'",
    "field for this source package."
    ]

buildDepIndepP :: O.Parser [BuildDepIndep]
buildDepIndepP = many $ O.option (BuildDepIndep <$> relationsR) m where
  m = O.help helpMsg
      <> O.long "build-dep-indep"
      <> O.metavar "DEBIANRELATIONS"
  helpMsg = unlines [
    "Add a dependency relation to the `Build-Depends-Indep'",
    "field for this source package."
    ]

devDepP :: O.Parser [DevDep]
devDepP = many $ O.option (DevDep <$> relationsR) m where
  m = O.help helpMsg
      <> O.long "dev-dep"
      <> O.metavar "RELATION"
  helpMsg = "Add an entry to the `Depends' field of the -dev package"


-- Since `depends', `conflicts' and so on options are totally same,
-- we can avoid code via this function, which, given long option name
-- makes correct O.Parser. Newtype around (BinPkgName, Relations)
-- is inferred, but there is still some duplication.
--
-- Long option name can also be inferred from Typeable instance of
-- mentioned newtype, but this would introduce some amount of
-- low-level string manipulations.
--
-- Nice to know, but now, to me, it would introduce more complexity,
-- than eliminate.
mkExtraP :: (Newtype n, O n ~ (BinPkgName, Relations))
            => String -> O.Parser [n]
mkExtraP long@(c:cr) = many $ O.option (pack <$> extraRelationsR) m where
    fieldName = toUpper c : cr
    m = O.help helpMsg
        <> O.long long
        <> O.metavar "DEB:RELATION"
    helpMsg = "Add extry to '" ++ fieldName ++ " 'field of DEB binary package"
mkExtraP "" = error "mkExtraP: empty long option"

extraDependsP :: O.Parser [ExtraDepends]
extraDependsP = mkExtraP "depends"

extraConflictsP :: O.Parser [ExtraConflicts]
extraConflictsP = mkExtraP "conflicts"

extraProvidesP :: O.Parser [ExtraProvides]
extraProvidesP = mkExtraP "provides"

extraReplacesP :: O.Parser [ExtraReplaces]
extraReplacesP = mkExtraP "replaces"

extraRecommendsP :: O.Parser [ExtraRecommends]
extraRecommendsP = mkExtraP "recommends"

extraSuggestsP :: O.Parser [ExtraSuggests]
extraSuggestsP = mkExtraP "suggests"

cabalDebMappingP :: O.Parser [CabalDebMapping]
cabalDebMappingP = many $ O.option cabalDebMappingR m where
  m = O.help helpMsg
      <> O.long "dep-map"
      <> O.metavar "CABAL:DEBIANBINARYPACKAGE"
  helpMsg = unlines [
    "Specify what debian package name corresponds with a name that appears",
    "in the Extra-Library field of a cabal file,",
    "e.g. --map-dep cryptopp:libcrypto-dev."
    ]

execDebMappingP :: O.Parser [ExecDebMapping]
execDebMappingP = many $ O.option (ExecDebMapping <$> mappingR) m where
  m = O.help helpMsg
      <> O.long "exec-map"
      <> O.metavar "CABAL:DEBIANBINARYPACKAGE"
  helpMsg = unlines [
    "Specify a mapping from the name appearing in the Build-Tool",
    "field of the cabal file to a debian binary package name,",
    "e.g. --exec-map trhsx:haskell-hsx-utils"
    ]

cabalEpochMappingP :: O.Parser [CabalEpochMapping]
cabalEpochMappingP = many $ O.option (cabalEpochMappingR) m where
  m = O.help helpMsg
      <> O.long "epoch-map"
      <> O.metavar "CABALPACKAGE=DIGIT"
  helpMsg = unlines [
    "Specify a mapping from the cabal package name to a digit to use",
    "as the debian package epoch number, e.g. --epoch-map HTTP=1"
    ]

cabalFlagsP :: O.Parser [CabalFlagMapping]
cabalFlagsP = many $ O.option (cabalFlagMappingR) m where
  m = O.help helpMsg
      <> O.long "cabal-flags"
      <> O.long "cabal-flag"
      <> O.metavar "CABALFLAG or -CABALFLAG"
  helpMsg = "Flags to pass to cabal configure with the --flags= option"


profilingP :: O.Parser ProfilingStatus
profilingP = O.flag ProfilingEnabled ProfilingDisabled m where
  m = O.help helpMsg
      <> O.long "disable-profiling"
  helpMsg = unlines [
    "Do not generate profiling (-prof) library package, do not",
    "add -prof packages to the build dependency list."
     ]

haddockP :: O.Parser [HaddockStatus]
haddockP = (: []) <$> (O.flag HaddockEnabled HaddockDisabled m) where
  m = O.help helpMsg
      <> O.long "disable-haddock"
  helpMsg = "Do not create a -doc package"

officialP :: O.Parser OfficialStatus
officialP = O.flag NonOfficial Official m where
  m = O.help helpMsg
      <> O.long "official"
  helpMsg = "Follow guidelines of Debian Haskell Group"

sourceFormatP :: O.Parser SourceFormat
sourceFormatP = O.flag Quilt3 Native3 m where
  m = O.help helpMsg
      <> O.long "native"
  helpMsg = unlines [
    "Package has an no upstream tarball,",
    "write '3.0 (native)' into source/format."
    ]

testsP :: O.Parser TestsStatus
testsP = buildOnlyTestsP <|> disableTestsP

disableTestsP :: O.Parser TestsStatus
disableTestsP = O.flag TestsRun TestsDisable m where
  m = O.help "disable test suite"
      <> O.long "disable-tests"
      <> O.long "no-tests"

buildOnlyTestsP :: O.Parser TestsStatus
buildOnlyTestsP = O.flag TestsRun TestsBuild m where
  m = O.help "build, but do not run test suite"
      <> O.long "no-run-tests"
      <> O.long "disable-running-tests"

-- Here is 'Flags' parser and parsers for every it's field.

flagsP :: O.Parser Flags
flagsP = Flags <$> verbosityP
               <*> dryRunP
               <*> upgradeP
               <*> roundtripP
               <*> pure False     -- validate
               <*> hcFlavorP         -- CompilerFlavor
               <*> (flagSet <$> cabalFlagsP)    -- cabalFlagAssignments
               <*> buildEnvDirP
    where
      flagSet cfms = Set.fromList (map (\ (CabalFlagMapping (name, bool)) -> (name, bool)) cfms)

verbosityP :: O.Parser Int
verbosityP = length <$> many (O.flag' () m) where
  m = O.help helpMsg
      <> O.short 'v'
      <> O.long "verbose"
  helpMsg = unlines [
    "Every instance of this flag increases amount",
    "of progress messages generated"
    ]

dryRunP :: O.Parser Bool
dryRunP = O.switch m where
  m = O.help helpMsg
      <> O.short 'n'
      <> O.long "dry-run"
  helpMsg = unlines [
    "Just compare the existing debianization",
    "to the one we would generate."
    ]

upgradeP :: O.Parser Bool
upgradeP = O.switch m where
  m = O.help helpMsg
      <> O.long "upgrade"
  helpMsg = unlines [
    "Upgrade an existing debianization carefully",
    "preserving fields that are commonly hand-edited."
    ]

roundtripP :: O.Parser Bool
roundtripP = O.switch m where
  m = O.help helpMsg
      <> O.long "roundtrip"
  helpMsg = unlines [
    "Roundtrip a debianization to normalize it."
    ]

-- versionR :: O.ReadM Version
-- versionR = (maybe (error "Invalid compiler version") id . parseVersion') <$> O.str

hcFlavorP :: O.Parser CompilerFlavor
hcFlavorP = O.flag GHC
#if MIN_VERSION_Cabal(1,22,0)
                    GHCJS
#else
                    GHC
#endif
                          m where
  m = O.help helpMsg
      <> O.long "ghcjs"
  helpMsg = "Set compiler flavor to GHCJS."

buildEnvDirP :: O.Parser EnvSet
buildEnvDirP = O.option ((\s -> EnvSet {cleanOS = s </> "clean", dependOS = s </> "depend", buildOS = s </> "build"}) <$> O.str) m where
  m = O.help "Directory containing the three build environments, clean, depend, and build."
      <> O.long "buildenvdir"
      <> O.value (EnvSet {cleanOS = "/", dependOS = "/", buildOS = "/"})
      <> O.metavar "DIR"

commandLineOptionsP :: O.Parser CommandLineOptions
commandLineOptionsP = CommandLineOptions <$> flagsP <*> behaviorAdjustmentP

commandLineOptionsParserInfo :: [String] -> O.ParserInfo CommandLineOptions
commandLineOptionsParserInfo args = O.info (O.helper <*> commandLineOptionsP) im where
  im = O.header "cabal-debian -- create debianization of cabal package"
       <> O.fullDesc
       <> O.progDescDoc (Just descDoc)
  descDoc =
    "Typical usage is run in unpacked source root directory"
    <+> linebreak <+> linebreak
    <+> indent 2 "% cabal-debian  --maintainer 'Maintainer Name <maintainer@email>'"
    <+> linebreak <+> linebreak
    <+> (string . unlines $ [
     "This will read the package's cabal file and any existing debian/changelog file and",
     "deduce what it can about the debianization, then it will create or modify files in",
     "the debian subdirectory.  Note that it will not remove any files in debian, and",
     "these could affect the operation of the debianization in unknown ways.  For this",
     "reason it is recommended either using a pristine unpacked directory each time, or else",
     "using a revision control system to revert the package to a known state before running.",
     "",
     "Arguments: " ++ showCommandForUser "cabal-debian" args
     ])

-- FIXME: Separation of parsing of `BehaviorAdjustment' and performing
-- of corresponding actions is all great, but now it is pretty easy
-- to not handle particular field in `BehaviorAdjustment' field and
-- ghc will not complain.
handleBehaviorAdjustment :: (MonadIO m) => BehaviorAdjustment -> CabalT m ()
handleBehaviorAdjustment (BehaviorAdjustment {..}) = do
 forM_ _cabalEpochMapping $ \(CabalEpochMapping (pkg, num)) -> A.epochMap %= Map.insert pkg num
 zoom A.debInfo $ do
  forM_ _executable $ (D.executable %=) . uncurry Map.insert
  forM_ _execDebMapping $ (D.execMap %=) . uncurry Map.insert . unpack
  forM_ _missingDependency $ (D.missingDependencies %=) . Set.insert
  D.utilsPackageNameBase .= _defaultPackage
  D.noDocumentationLibrary .= (HaddockDisabled `elem` _haddock)
  D.noProfilingLibrary .= (_profiling == ProfilingDisabled)
  D.overrideDebianNameBase .= _debianNameBase
  D.sourcePackageName .= _sourcePackageName
  D.maintainerOption .= Just _maintainer
  D.sourceFormat .= _sourceFormat
  D.revision .= unpack `fmap` _revision
  D.debVersion .= _debianVersion
  D.uploadersOption %= (++ _uploaders)
  D.extraDevDeps %= (++ concatMap unpack _devDep)
#if MIN_VERSION_Cabal(2,0,0)
  forM_ _cabalDebMapping $ \(CabalDebMapping (pkg, rels)) -> do
    D.extraLibMap %= Map.insert (unPackageName pkg) rels
#else
  forM_ _cabalDebMapping $ \(CabalDebMapping (PackageName pkg, rels)) -> do
    D.extraLibMap %= Map.insert pkg rels
#endif
  addExtra _extraDepends B.depends
  addExtra _extraConflicts B.conflicts
  addExtra _extraProvides B.provides
  addExtra _extraReplaces B.replaces
  addExtra _extraRecommends B.recommends
  addExtra _extraSuggests B.suggests
  D.testsStatus .= _tests
  D.official .= (_official == Official)
  zoom D.control $ do
    S.section .= Just _sourceSection
    S.standardsVersion .= Just _standardsVersion
    S.buildDepends %= (++ concatMap unpack _buildDep)
    S.buildDepends %= (++ concatMap unpack _devDep)
    S.buildDependsIndep %= (++ concatMap unpack _buildDepIndep)

addExtra :: (MonadState D.DebInfo m, Newtype n, O n ~ (BinPkgName, Relations)) =>
            [n] -> Lens' B.PackageRelations Relations -> m ()
addExtra extra lens' = forM_ extra $ \arg -> do
  let (pkg, rel) = unpack arg
  D.binaryDebDescription pkg . B.relations . lens' %= (++ rel)

parseProgramArguments' :: [String] -> IO CommandLineOptions
parseProgramArguments' args =  O.handleParseResult result where
  prefs = O.prefs O.idm
  result = O.execParserPure prefs (commandLineOptionsParserInfo args) args

parseProgramArguments :: IO CommandLineOptions
parseProgramArguments = getArgs >>= parseProgramArguments' . leaveOne "--disable-haddock"
    where
      leaveOne :: String -> [String] -> [String]
      leaveOne s xs = go False xs
          where
            go _ [] = []
            go False (x : xs') | x == s = x : go True xs'
            go True (x : xs') | x == s = go True xs'
            go flag (x : xs') = x : go flag xs'
