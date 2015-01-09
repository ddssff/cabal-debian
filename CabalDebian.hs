{-# LANGUAGE ScopedTypeVariables #-}
-- | This is the main function of the cabal-debian executable.  This
-- is generally run by the autobuilder to debianize packages that
-- don't have any custom debianization code in Setup.hs.  This is a
-- less flexible and powerful method than calling the debianize
-- function directly, many sophisticated configuration options cannot
-- be accessed using the command line interface.

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Lens.Lazy (access)
import Data.List as List (unlines)
import Debian.Debianize.Details (debianDefaultAtoms)
import Debian.Debianize.Finalize (debianize)
import Debian.Debianize.Monad (DebT, evalDebT)
import Debian.Debianize.Options (options)
import Debian.Debianize.Output (doDebianizeAction)
import Debian.Debianize.SubstVars (substvars)
import Debian.Debianize.Types.Atoms (DebAction(Debianize, SubstVar, Usage), EnvSet(EnvSet), debAction, newAtoms)
import Prelude hiding (unlines, writeFile, init)
import System.Console.GetOpt (OptDescr, usageInfo)
import System.Environment (getProgName)

main :: IO ()
main = cabalDebianMain debianDefaultAtoms

-- | The main function for the cabal-debian executable.
cabalDebianMain :: (MonadIO m, Functor m) => DebT m () -> m ()
cabalDebianMain init =
    -- This picks up the options required to decide what action we are
    -- taking.  Much of this will be repeated in the call to debianize.
    do atoms <- newAtoms
       evalDebT (do init
                    action <- access debAction
                    finish action) atoms
    where
      envset = EnvSet "/" "/" "/"
      finish :: forall m. (MonadIO m, Functor m) => DebAction -> DebT m ()
      finish (SubstVar debType) = substvars debType
      finish Debianize = debianize (return ()) >> doDebianizeAction envset
      finish Usage = do
          progName <- liftIO getProgName
          let info = unlines [ "Typical usage is to cd to the top directory of the package's unpacked source and run: "
                             , ""
                             , "    " ++ progName ++ " --maintainer 'Maintainer Name <maintainer@email>'."
                             , ""
                             , "This will read the package's cabal file and any existing debian/changelog file and"
                             , "deduce what it can about the debianization, then it will create or modify files in"
                             , "the debian subdirectory.  Note that it will not remove any files in debian, and"
                             , "these could affect the operation of the debianization in unknown ways.  For this"
                             , "reason I recommend either using a pristine unpacked directory each time, or else"
                             , "using a revision control system to revert the package to a known state before running."
                             , "The following additional options are available:" ]
          liftIO $ putStrLn (usageInfo info (options :: [OptDescr (DebT m ())]))
