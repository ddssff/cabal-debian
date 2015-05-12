{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Debian.Debianize.Optparse (
  CommandLineOptions(..),
  BehaviorAdjustment,
  Flags(..),
  parseProgramArguments,
  handleBehaviorAdjustment) where
import qualified  Debian.Debianize.DebInfo as D
import qualified Debian.Debianize.SourceDebDescription as S
import qualified Debian.Debianize.CabalInfo as A
import Distribution.Compiler (CompilerFlavor(..))
import Data.Maybe.Extended (fromMaybe)
import Data.Foldable (forM_)
import Control.Monad.Trans
import Debian.Debianize.Monad
import System.FilePath(splitFileName)
import Debian.Debianize.VersionSplits
import Debian.Policy
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))
import Debian.Relation
import Debian.Debianize.Prelude (maybeRead)
import qualified Options.Applicative as O
import Control.Applicative ((<$>), (<*>), many, pure)
import Data.Maybe.Extended (nothingIf)
import Data.Monoid ((<>), mempty)
import Text.PrettyPrint.ANSI.Leijen (linebreak, (<+>), string, indent)
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debian.Debianize.BasicInfo
import System.Posix.Env (getEnv)
import System.Environment (getArgs)

data HaddockStatus = HaddockEnabled | HaddockDisabled deriving (Show, Eq)
data ProfilingStatus = ProfilingEnabled | ProfilingDisabled deriving (Show, Eq)
data OfficialStatus = Official| NonOfficial deriving (Show, Eq)
newtype BuildDep = BuildDep { unBuildDep :: Relations } deriving Show
newtype BuildDepIndep = BuildDepIndep { unBuildDepIndep :: Relations } deriving Show

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
  _executable        :: [(BinPkgName, D.InstallFile)],
  _defaultPackage    :: Maybe String,
  _missingDependency :: [BinPkgName],
  _debianNameBase    :: Maybe DebBase,
  _sourcePackageName :: Maybe SrcPkgName,
  _sourceSection     :: Section,
  _standardsVersion  :: StandardsVersion,
  _buildDep          :: [BuildDep],
  _buildDepIndep     :: [BuildDepIndep],
  _profiling         :: ProfilingStatus,
  _haddock           :: HaddockStatus,
  _official          :: OfficialStatus
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

maintainerR :: O.ReadM NameAddr
maintainerR = either fail return =<< parseMaintainer <$> O.str

relationsR :: O.ReadM Relations
relationsR = either (fail . show) return =<< parseRelations <$> O.str


-- Here are parser for BehaviorAdjustment and next are parsers for
-- every field of this data.  Please, keep parsers declarations in
-- same order, as are fields.

behaviorAdjustmentP :: O.Parser BehaviorAdjustment
behaviorAdjustmentP = BehaviorAdjustment <$> maintainerP
                                         <*> executableP
                                         <*> defaultPackageP
                                         <*> missingDependencyP
                                         <*> debianNameBaseP
                                         <*> sourcePackageNameP
                                         <*> sourceSectionP
                                         <*> standardsVersionP
                                         <*> buildDepP
                                         <*> buildDepIndepP
                                         <*> profilingP
                                         <*> haddockP
                                         <*> officialP

maintainerP :: O.Parser NameAddr
maintainerP = O.option maintainerR m where
  m = O.help helpMsg
      <> O.long "maintainer"
      <> O.short 'm'
      <> O.value (NameAddr (Just "Debian Haskell Group")
                           "<pkg-haskell-maintainers@lists.alioth.debian.org>")
      <> O.metavar "'NAME <EMAIL>'"
  helpMsg = "Set the `Maintainer' field in debian/control file."

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

sourcePackageNameP :: O.Parser (Maybe SrcPkgName)
sourcePackageNameP = O.option (Just . SrcPkgName <$> O.str) m where
  m = O.help helpMsg
      <> O.long "source-package-name"
      <> O.short 's'
      <> O.value Nothing
      <> O.metavar "NAME"
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
      <> O.value (parseStandardsVersion "3.9.6")
      <> O.metavar "VERSION"
  helpMsg = unlines [
    "Claim compatibility to this version of the Debian policy",
    "(i.e. the value of the Standards-Version field)"
    ]

buildDepP :: O.Parser [BuildDep]
buildDepP = many $ O.option (BuildDep <$> relationsR) m where
  m = O.help helpMsg
      <> O.long "build-dep"
      <> O.metavar "RELATION"
  helpMsg = unlines [
    "Add a dependency relation to the `Build-Depends'",
    "field for this source package."
    ]

buildDepIndepP :: O.Parser [BuildDepIndep]
buildDepIndepP = many $ O.option (BuildDepIndep <$> relationsR) m where
  m = O.help helpMsg
      <> O.long "build-dep-indep"
      <> O.metavar "RELATION"
  helpMsg = unlines [
    "Add a dependency relation to the `Build-Depends-Indep'",
    "field for this source package."
    ]

profilingP :: O.Parser ProfilingStatus
profilingP = O.flag ProfilingEnabled ProfilingDisabled m where
  m = O.help helpMsg
      <> O.long "disable-profiling"
  helpMsg = "Do not generate profiling (-prof) library package."

haddockP :: O.Parser HaddockStatus
haddockP = O.flag HaddockEnabled HaddockDisabled m where
  m = O.help helpMsg
      <> O.long "disable-haddock"
  helpMsg = "Do not build haddoc documentation"

officialP :: O.Parser OfficialStatus
officialP = O.flag NonOfficial Official m where
  m = O.help helpMsg
      <> O.long "official"
  helpMsg = "Follow guidelines of Debian Haskell Group"

-- Here is 'Flags' parser and parsers for every it's field.

flagsP :: O.Parser Flags
flagsP = Flags <$> verbosityP
               <*> dryRunP
               <*> pure False     -- validate
               <*> pure GHC       -- CompilerFlavor
               <*> pure mempty    -- cabalFlagAssignments
               <*> pure (EnvSet {cleanOS = "/", dependOS = "/", buildOS = "/"})

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

commandLineOptionsP :: O.Parser CommandLineOptions
commandLineOptionsP = CommandLineOptions <$> flagsP <*> behaviorAdjustmentP

commandLineOptionsParserInfo :: O.ParserInfo CommandLineOptions
commandLineOptionsParserInfo = O.info (O.helper <*> commandLineOptionsP) im where
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
     "using a revision control system to revert the package to a known state before running."
     ])

handleBehaviorAdjustment :: (MonadIO m) => BehaviorAdjustment -> CabalT m ()
handleBehaviorAdjustment (BehaviorAdjustment {..}) = zoom A.debInfo $ do
  forM_ _executable $ (D.executable %=) . uncurry Map.insert
  forM_ _missingDependency $ (D.missingDependencies %=) . Set.insert
  D.utilsPackageNameBase .= _defaultPackage
  D.noDocumentationLibrary .= (_haddock == HaddockDisabled)
  D.noProfilingLibrary .= (_profiling == ProfilingDisabled)
  D.overrideDebianNameBase .= _debianNameBase
  D.sourcePackageName .= _sourcePackageName
  D.maintainerOption .= Just _maintainer
  D.official .= (_official == Official)
  zoom D.control $ do
    S.section .= Just _sourceSection
    S.standardsVersion .= Just _standardsVersion
    S.buildDepends %= (++ concatMap unBuildDep _buildDep)
    S.buildDependsIndep %= (++ concatMap unBuildDepIndep _buildDepIndep)


parseProgramArguments' :: [String] -> IO CommandLineOptions
parseProgramArguments' args =  O.handleParseResult result where
  prefs = O.prefs O.idm
  result = O.execParserPure prefs commandLineOptionsParserInfo args

parseProgramArguments :: IO CommandLineOptions
parseProgramArguments = defArgs >>= parseProgramArguments'

defArgs :: IO [String]
defArgs = (++) <$> getArgs <*> (fromMaybe [] . (maybeRead =<<) <$> getEnv "CABALDEBIAN")
