{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.Monad
    ( Atoms

    , DebT
    , runDebT
    , evalDebT
    , execDebT
    , DebM
    , runDebM
    , evalDebM
    , execDebM

    -- * modify cabal to debian package version map
    -- , mapCabal
    -- , splitCabal
    ) where

import Control.Monad.State (evalState, evalStateT, execState, execStateT, runState, State, StateT(runStateT))
import Debian.Debianize.Types.Atoms (Atoms)
import Debian.Orphans ()
import Prelude hiding (init, log, unlines)

type DebT m = StateT Atoms m
type DebM = State Atoms

execDebT :: Monad m => DebT m a -> Atoms -> m Atoms
execDebT action atoms = execStateT action atoms

evalDebT :: Monad m => DebT m a -> Atoms -> m a
evalDebT action atoms = evalStateT action atoms

runDebT :: Monad m => DebT m a -> Atoms -> m (a, Atoms)
runDebT action atoms = runStateT action atoms

execDebM :: DebM a -> Atoms -> Atoms
execDebM action atoms = execState action atoms

evalDebM :: DebM a -> Atoms -> a
evalDebM action atoms = evalState action atoms

runDebM :: DebM a -> Atoms -> (a, Atoms)
runDebM action atoms = runState action atoms
