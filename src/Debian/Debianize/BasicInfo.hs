-- | The basic information required to load a Cabal or Debian package description.
{-# LANGUAGE CPP, DeriveDataTypeable, TemplateHaskell #-}
module Debian.Debianize.BasicInfo
    ( -- * Types
      Flags(..)
    , EnvSet(..)
    , DebType(..)
      -- * Lenses
    , verbosity
    , dryRun
    , upgrade
    , roundtrip
    , validate
    , compilerChoice
    , cabalFlagAssignments
    , buildEnv
      -- * State Monad
    , flagOptions
    ) where

import Control.Lens
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadIO)
import Data.Char (toLower, toUpper)
import Data.Generics (Data, Typeable)
import Data.Set as Set (fromList, Set, union)
import Debian.Debianize.Prelude (read')
import Debian.GHC (CompilerChoice, hcFlavor)
import Debian.Orphans ()
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.PackageDescription as Cabal (FlagName(FlagName))
import Prelude hiding (break, lines, log, null, readFile, sum)
import System.Console.GetOpt (ArgDescr(ReqArg, NoArg), OptDescr(Option))
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- | This record supplies enough information to locate and load a debianization
-- or a cabal file from the IO monad.
data Flags = Flags
    {
      _verbosity :: Int
    -- ^ Run with progress messages at the given level of verboseness.
    , _dryRun :: Bool
    -- ^ Don't write any files or create any directories, just explain
    -- what would have been done.
    , _upgrade :: Bool
    -- ^ Carefully upgrade the packaging
    , _roundtrip :: Bool
    -- ^ Normalize a debianization (field order, whitespace) by round-tripping it.
    , _validate :: Bool
    -- ^ Fail if the debianization already present doesn't match the
    -- one we are going to generate closely enough that it is safe to
    -- debianize during the run of dpkg-buildpackage, when Setup
    -- configure is run.  Specifically, the version number in the top
    -- changelog entry must match, and the sets of package names in
    -- the control file must match.
    , _compilerChoice :: CompilerChoice
    -- ^ Which compiler should we generate library packages for?  In theory a single
    -- deb could handle multiple compiler flavors, but the support tools are not ready
    -- for this as of right now (28 Jan 2015.)
    , _cabalFlagAssignments :: Set (FlagName, Bool)
    -- ^ Flags to pass to Cabal function finalizePackageDescription,
    -- this can be used to control the flags in the cabal file.  It
    -- can be supplied to the cabal-debian binary using the --flags
    -- option.
    , _buildEnv :: EnvSet
    -- ^ Directory containing the build environment for which the
    -- debianization will be generated.  This determines which
    -- compiler will be available, which in turn determines which
    -- basic libraries can be provided by the compiler.  By default
    -- all the paths in EnvSet are "/".
    } deriving (Eq, Ord, Show, Data, Typeable)

data EnvSet = EnvSet
    { cleanOS :: FilePath  -- ^ The output of the debootstrap command
    , dependOS :: FilePath -- ^ An environment with build dependencies installed
    , buildOS :: FilePath  -- ^ An environment where we have built a package
    } deriving (Eq, Ord, Show, Data, Typeable)

-- | A redundant data type, too lazy to expunge.
data DebType = Dev | Prof | Doc deriving (Eq, Ord, Read, Show, Data, Typeable)



-- Build the lenses
$(makeLenses ''Flags)

-- | Command line options which build a function that modifies a
-- state monad value of type 'Flags'
flagOptions :: MonadIO m => [OptDescr (StateT Flags m ())]
flagOptions =
    [ Option "v" ["verbose"] (ReqArg (\ s -> verbosity .= (read' (\ s' -> error $ "verbose: " ++ show s') s :: Int)) "number")
             "Change the amount of progress messages generated",
      Option "n" ["dry-run", "compare"] (NoArg (dryRun .= True))
             "Just compare the existing debianization to the one we would generate.",
      Option "" ["upgrade"] (NoArg (upgrade .= True))
             "Carefully upgrade an existing debianization",
      Option "" ["roundtrip"] (NoArg (roundtrip .= True))
             "Rountrip a debianization to normalize it",
      Option "" ["ghc"] (NoArg ((compilerChoice . hcFlavor) .= GHC)) "Generate packages for GHC - same as --with-compiler GHC",
#if MIN_VERSION_Cabal(1,22,0)
      Option "" ["ghcjs"] (NoArg ((compilerChoice . hcFlavor) .= GHCJS)) "Generate packages for GHCJS - same as --with-compiler GHCJS",
#endif
      Option "" ["hugs"] (NoArg ((compilerChoice . hcFlavor) .= Hugs)) "Generate packages for Hugs - same as --with-compiler GHC",
      Option "" ["with-compiler"] (ReqArg (\ s -> maybe (error $ "Invalid compiler id: " ++ show s)
                                                        (\ hc -> (compilerChoice . hcFlavor) .= hc)
                                                        (readMaybe (map toUpper s) :: Maybe CompilerFlavor)) "COMPILER")
             (unlines [ "Generate packages for this CompilerFlavor" ]),
      Option "f" ["flags"] (ReqArg (\ fs -> cabalFlagAssignments %= (Set.union (Set.fromList (flagList fs)))) "FLAGS")
      -- Option "f" ["flags"] (ReqArg (\ fs p -> foldl (\ p' x -> p' {cabalFlagAssignments_ = Set.insert x (cabalFlagAssignments_ p')}) p (flagList fs)) "FLAGS")
             (unlines [ "Flags to pass to the finalizePackageDescription function in"
                      , "Distribution.PackageDescription.Configuration when loading the cabal file."]),
      Option "" ["buildenvdir"] (ReqArg (\ s -> buildEnv .= EnvSet {cleanOS = s </> "clean", dependOS = s </> "depend", buildOS = s </> "build"}) "PATH")
             "Directory containing the three build environments, clean, depend, and build.",
      Option "f" ["cabal-flags"] (ReqArg (\ s -> cabalFlagAssignments %= (Set.union (fromList (flagList s)))) "FLAG FLAG ...")
             "Flags to pass to cabal configure with the --flags= option "
      ]

-- Lifted from Distribution.Simple.Setup, since it's not exported.
flagList :: String -> [(FlagName, Bool)]
flagList = map tagWithValue . words
  where tagWithValue ('-':name) = (FlagName (map toLower name), False)
        tagWithValue name       = (FlagName (map toLower name), True)
