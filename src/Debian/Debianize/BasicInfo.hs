-- | Input the Cabal package description.
{-# LANGUAGE CPP, DeriveDataTypeable, TemplateHaskell #-}
module Debian.Debianize.BasicInfo where

import Control.Applicative ((<$>))
import Control.Category ((.))
import Control.Monad.State (StateT, execStateT)
import Control.Monad.Trans (MonadIO)
import Data.Char (toLower, toUpper)
import Data.Default (Default(def))
import Data.Generics (Data, Typeable)
import Data.Lens.Template (nameMakeLens)
import Data.Monoid (Monoid(..))
import Data.Set as Set (fromList, Set, union)
import Debian.Debianize.Prelude ((%=), read', (~=))
import Debian.Orphans ()
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.PackageDescription as Cabal (FlagName(FlagName))
import Prelude hiding ((.), break, lines, log, null, readFile, sum)
import System.Console.GetOpt (ArgDescr(ReqArg, NoArg), ArgOrder(Permute), getOpt, OptDescr(Option))
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- | This record supplies enough information to locate and load a debianization
-- or a cabal file from the IO monad.
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

data EnvSet = EnvSet
    { cleanOS :: FilePath  -- ^ The output of the debootstrap command
    , dependOS :: FilePath -- ^ An environment with build dependencies installed
    , buildOS :: FilePath  -- ^ An environment where we have built a package
    } deriving (Eq, Ord, Show, Data, Typeable)

data DebAction = Usage | Debianize | SubstVar DebType deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | A redundant data type, too lazy to expunge.
data DebType = Dev | Prof | Doc deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Default Flags where
    def = Flags
          { verbosity_ = 1
          , debAction_ = Debianize
          , dryRun_ = False
          , validate_ = False
          , compilerFlavor_ = GHC
          , cabalFlagAssignments_ = mempty
          , buildEnv_ = EnvSet {cleanOS = "/", dependOS = "/", buildOS = "/"}
          }

-- Build the lenses
$(let f s = case s of
              (_ : _) | last s == '_' -> Just (init s)
              _ -> Nothing in
  nameMakeLens ''Flags f)

flagOptions :: MonadIO m => [OptDescr (StateT Flags m ())]
flagOptions =
    [ Option "v" ["verbose"] (ReqArg (\ s -> verbosity ~= (read' (\ s' -> error $ "verbose: " ++ show s') s :: Int)) "number")
             "Change the amount of progress messages generated",
      Option "n" ["dry-run", "compare"] (NoArg (dryRun ~= True))
             "Just compare the existing debianization to the one we would generate.",
      Option "h?" ["help"] (NoArg (debAction ~= Usage))
             "Show this help text",
      Option "" ["ghc"] (NoArg (compilerFlavor ~= GHC)) "Generate packages for GHC - same as --with-compiler GHC",
      Option "" ["ghcjs"] (NoArg (compilerFlavor ~= GHCJS)) "Generate packages for GHCJS - same as --with-compiler GHCJS",
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
      Option "" ["buildenvdir"] (ReqArg (\ s -> buildEnv ~= EnvSet {cleanOS = s </> "clean", dependOS = s </> "depend", buildOS = s </> "build"}) "PATH")
             "Directory containing the three build environments, clean, depend, and build.",
      Option "f" ["cabal-flags"] (ReqArg (\ s -> cabalFlagAssignments %= (Set.union (fromList (flagList s)))) "FLAG FLAG ...")
             "Flags to pass to cabal configure with the --flags= option "
      ]

-- Lifted from Distribution.Simple.Setup, since it's not exported.
flagList :: String -> [(FlagName, Bool)]
flagList = map tagWithValue . words
  where tagWithValue ('-':name) = (FlagName (map toLower name), False)
        tagWithValue name       = (FlagName (map toLower name), True)

newFlags :: IO Flags
newFlags = do
  (fns, _, _) <- getOpt Permute flagOptions <$> getArgs
  execStateT (sequence fns) def
