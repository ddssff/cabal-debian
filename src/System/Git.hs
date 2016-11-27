-- | Git related functions that belong in some other package.

{-# LANGUAGE ScopedTypeVariables #-}

module System.Git
    ( gitResetHard
    , gitResetSubdir
    , gitUnclean
    , gitIsClean
    , withCleanRepo
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (<$>))
#endif 
import Control.Exception (catch, SomeException, throw)
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode, readProcess)

-- | Do a hard reset of all the files of the repository containing the
-- working directory.
gitResetHard :: IO ()
gitResetHard = do
  (code, _out, _err) <- readProcessWithExitCode "git" ["reset", "--hard"] ""
  case code of
    ExitSuccess -> pure ()
    ExitFailure _n -> error "gitResetHard"

-- | Do a hard reset of all the files of a subdirectory within a git
-- repository.  (Does this every throw an exception?)
gitResetSubdir :: FilePath -> IO ()
gitResetSubdir dir = do
  (readProcess "git" ["checkout", "--", dir] "" >>
   readProcess "git" ["clean", "-f", dir] "" >> pure ())
    `catch` \(e :: SomeException) -> hPutStrLn stderr ("gitResetSubdir " ++ show dir ++ " failed: " ++ show e) >> throw e

-- | Determine whether the repository containing the working directory
-- is in a modified state, if so return the messages.
gitUnclean :: IO (Maybe String)
gitUnclean = do
  here <- getCurrentDirectory
  hPutStrLn stderr ("here: " ++ show here)
  (code, out, _err) <- readProcessWithExitCode "git" ["status", "--porcelain"] ""
  case code of
    ExitFailure _ -> error "gitCheckClean failure"
    ExitSuccess | all unmodified (lines out) -> pure Nothing
    ExitSuccess -> pure $ Just out
    where
      unmodified (a : b : _) = elem a "?! " && elem b "?! "
      unmodified _ = False

gitIsClean :: IO Bool
gitIsClean = maybe True (const False) <$> gitUnclean

withCleanRepo :: IO a -> IO a
withCleanRepo action = do
  gitUnclean >>= maybe action (\s -> error $ "withCleanRepo: please commit or revert changes:\n" ++ s)
