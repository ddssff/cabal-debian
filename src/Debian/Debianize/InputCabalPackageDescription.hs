-- | Input the Cabal package description.
{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Debian.Debianize.InputCabalPackageDescription
    ( Flags(..)
    , EnvSet(..)
    , DebType(..)
    , DebAction(..)
    , newFlags
    , verbosity, dryRun, validate, debAction, cabalFlagAssignments, compilerFlavor, buildEnv, setBuildEnv
    , inputCabalization
    , inputCabalization'
    , flagOptions
    ) where

import Control.Applicative ((<$>))
import Control.Category ((.))
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.State (StateT, execStateT)
import Control.Monad.Trans (MonadIO)
import Data.Char (toUpper, toLower)
import Data.Generics (Data, Typeable)
import Data.Lens.Common (Lens, lens)
import Data.Monoid (Monoid(..))
import Data.Set as Set (Set, toList, fromList, union)
import Debian.Debianize.Prelude (intToVerbosity', read', (~=), (%=))
import Debian.GHC (newestAvailableCompilerId)
import Debian.Orphans ()
import Distribution.Compiler (CompilerId, CompilerFlavor(..))
#if MIN_VERSION_Cabal(1,22,0)
import Distribution.Compiler (unknownCompilerInfo, AbiTag(NoAbiTag))
#endif
import Distribution.Package (Package(packageId), Dependency)
import qualified Distribution.PackageDescription as Cabal (PackageDescription)
import Distribution.PackageDescription as Cabal (PackageDescription, FlagName(FlagName))
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Utils (defaultPackageDesc, die, setupMessage)
import Distribution.System as Cabal (Platform(..), buildArch)
import qualified Distribution.System as Cabal (buildOS)
import Distribution.Verbosity (Verbosity)
import Prelude hiding (readFile, lines, break, null, log, sum, (.))
-- import qualified Prelude (lines)
import System.Console.GetOpt (OptDescr(Option), ArgDescr(ReqArg, NoArg), ArgOrder(Permute), getOpt)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Posix.Files (setFileCreationMask)
import System.Process (system)
-- import System.Unix.Chroot (useEnv)
-- import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)
import Text.Read (readMaybe)

data EnvSet = EnvSet
    { cleanOS :: FilePath  -- ^ The output of the debootstrap command
    , dependOS :: FilePath -- ^ An environment with build dependencies installed
    , buildOS :: FilePath  -- ^ An environment where we have built a package
    } deriving (Eq, Ord, Show, Data, Typeable)

-- | This record supplies enough information to locate and load the
-- cabal file from the IO monad.
data Flags = Flags
    {
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
    , compilerFlavor_ :: CompilerFlavor
    -- ^ Which compiler should we generate library packages for?  In theory a single
    -- deb could handle multiple compiler flavors, but the support tools are not ready
    -- for this as of right now (28 Jan 2015.)
    , cabalFlagAssignments_ :: Set (FlagName, Bool)
    -- ^ Flags to pass to Cabal function finalizePackageDescription,
    -- this can be used to control the flags in the cabal file.  It
    -- can be supplied to the cabal-debian binary using the --flags
    -- option.
    , buildEnv_ :: EnvSet
    -- ^ Directory containing the build environment for which the
    -- debianization will be generated.  This determines which
    -- compiler will be available, which in turn determines which
    -- basic libraries can be provided by the compiler.  By default
    -- all the paths in EnvSet are "/".
    } deriving (Eq, Ord, Show, Data, Typeable)

data DebAction = Usage | Debianize | SubstVar DebType deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | A redundant data type, too lazy to expunge.
data DebType = Dev | Prof | Doc deriving (Eq, Ord, Read, Show, Data, Typeable)

defaultFlags :: Flags
defaultFlags =
    Flags
    { verbosity_ = 1
    , debAction_ = Debianize
    , dryRun_ = False
    , validate_ = False
    , compilerFlavor_ = GHC
    , cabalFlagAssignments_ = mempty
    , buildEnv_ = EnvSet {cleanOS = "/", dependOS = "/", buildOS = "/"}
    }

newFlags :: IO Flags
newFlags = do
  (fns, _, _) <- getOpt Permute flagOptions <$> getArgs
  execStateT (sequence fns) defaultFlags

flagOptions :: MonadIO m => [OptDescr (StateT Flags m ())]
flagOptions =
    [ Option "v" ["verbose"] (ReqArg (\ s -> verbosity ~= (read' (\ s' -> error $ "verbose: " ++ show s') s :: Int)) "number")
             "Change the amount of progress messages generated",
      Option "n" ["dry-run", "compare"] (NoArg (dryRun ~= True))
             "Just compare the existing debianization to the one we would generate.",
      Option "h?" ["help"] (NoArg (debAction ~= Usage))
             "Show this help text",
      Option "" ["ghc"] (NoArg (compilerFlavor ~= GHC)) "Generate packages for GHC - same as --with-compiler GHC",
#if MIN_VERSION_Cabal(1,21,0)
      Option "" ["ghcjs"] (NoArg (compilerFlavor ~= GHCJS)) "Generate packages for GHCJS - same as --with-compiler GHCJS",
#endif
      Option "" ["hugs"] (NoArg (compilerFlavor ~= Hugs)) "Generate packages for Hugs - same as --with-compiler GHC",
      Option "" ["with-compiler"] (ReqArg (\ s -> maybe (error $ "Invalid compiler id: " ++ show s)
                                                        (\ hc -> compilerFlavor ~= hc)
                                                        (readMaybe (map toUpper s) :: Maybe CompilerFlavor)) "COMPILER")
             (unlines [ "Generate packages for this CompilerFlavor" ]),
      Option "f" ["flags"] (ReqArg (\ fs -> cabalFlagAssignments %= (Set.union (Set.fromList (flagList fs)))) "FLAGS")
      -- Option "f" ["flags"] (ReqArg (\ fs p -> foldl (\ p' x -> p' {cabalFlagAssignments_ = Set.insert x (cabalFlagAssignments_ p')}) p (flagList fs)) "FLAGS")
             (unlines [ "Flags to pass to the finalizePackageDescription function in"
                      , "Distribution.PackageDescription.Configuration when loading the cabal file."]),
      Option "" ["debianize"] (NoArg (debAction ~= Debianize))
             "Deprecated - formerly used to get what is now the normal benavior.",
      Option "" ["substvar"] (ReqArg (\ name -> debAction ~= (SubstVar (read' (\ s -> error $ "substvar: " ++ show s) name))) "Doc, Prof, or Dev")
             (unlines [ "With this option no debianization is generated.  Instead, the list"
                      , "of dependencies required for the dev, prof or doc package (depending"
                      , "on the argument) is printed to standard output.  These can be added"
                      , "to the appropriate substvars file.  (This is an option whose use case"
                      , "is lost in the mists of time.)"]),
      Option "buildenvdir" [] (ReqArg (\ s -> buildEnv ~= EnvSet {cleanOS = s </> "clean", dependOS = s </> "depend", buildOS = s </> "build"}) "PATH")
             "Directory containing the three build environments, clean, depend, and build."
      ]

-- | Set how much progress messages get generated.
verbosity :: Lens Flags Int
verbosity = lens verbosity_ (\ b a -> a {verbosity_ = b})

-- | Don't write anything, just output a description of what would have happened
dryRun :: Lens Flags Bool
dryRun = lens dryRun_ (\ b a -> a {dryRun_ = b})

-- | Make sure the version number and package names of the supplied
-- and generated debianizations match.
validate :: Lens Flags Bool
validate = lens validate_ (\ b a -> a {validate_ = b})

-- | Debianize, SubstVars, or Usage.  I'm no longer sure what SubstVars does, but someone
-- may still be using it.
debAction :: Lens Flags DebAction
debAction = lens debAction_ (\ b a -> a {debAction_ = b})

-- | Cabal flag assignments to use when loading the cabal file.
cabalFlagAssignments :: Lens Flags (Set (FlagName, Bool))
cabalFlagAssignments = lens cabalFlagAssignments_ (\ a b -> b {cabalFlagAssignments_ = a})

compilerFlavor :: Lens Flags CompilerFlavor
compilerFlavor = lens compilerFlavor_ (\ a b -> b {compilerFlavor_ = a})

buildEnv :: Lens Flags EnvSet
buildEnv = lens buildEnv_ (\ b a -> a {buildEnv_ = b})

setBuildEnv :: MonadIO m => EnvSet -> Flags -> m Flags
setBuildEnv envset atoms = return $ atoms {buildEnv_ = envset}

inputCabalization :: Flags -> IO PackageDescription
inputCabalization flags =
    do let root = dependOS (buildEnv_ flags)
       let cid = newestAvailableCompilerId root (compilerFlavor_ flags)
       ePkgDesc <- inputCabalization' (intToVerbosity' $ verbosity_ flags) (cabalFlagAssignments_ flags) cid
       either (\ deps -> getCurrentDirectory >>= \ here ->
                         error $ "Missing dependencies in cabal package at " ++ here ++ ": " ++ show deps)
              return
              ePkgDesc

-- | Load a GenericPackageDescription from the current directory and
-- from that create a finalized PackageDescription for the given
-- CompilerId.
inputCabalization' :: Verbosity -> Set (FlagName, Bool) -> CompilerId -> IO (Either [Dependency] PackageDescription)
inputCabalization' vb flags cid = do
  genPkgDesc <- defaultPackageDesc vb >>= readPackageDescription vb
#if MIN_VERSION_Cabal(1,22,0)
  let cid' = unknownCompilerInfo cid NoAbiTag
#else
  let cid' = cid
#endif
  let finalized = finalizePackageDescription (toList flags) (const True) (Platform buildArch Cabal.buildOS) cid' [] genPkgDesc
  either (return . Left)
         (\ (pkgDesc, _) -> do bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> autoreconf vb pkgDesc
                               return (Right pkgDesc))
         finalized

-- | Run the package's configuration script.
autoreconf :: Verbosity -> Cabal.PackageDescription -> IO ()
autoreconf verbose pkgDesc = do
    ac <- doesFileExist "configure.ac"
    when ac $ do
        c <- doesFileExist "configure"
        when (not c) $ do
            setupMessage verbose "Running autoreconf" (packageId pkgDesc)
            ret <- system "autoreconf"
            case ret of
              ExitSuccess -> return ()
              ExitFailure n -> die ("autoreconf failed with status " ++ show n)

-- Lifted from Distribution.Simple.Setup, since it's not exported.
flagList :: String -> [(FlagName, Bool)]
flagList = map tagWithValue . words
  where tagWithValue ('-':name) = (FlagName (map toLower name), False)
        tagWithValue name       = (FlagName (map toLower name), True)
