{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.Monad
    ( Atoms

    , CabalT
    , runCabalT
    , evalCabalT
    , execCabalT
    , CabalM
    , runCabalM
    , evalCabalM
    , execCabalM

    -- * modify cabal to debian package version map
    -- , mapCabal
    -- , splitCabal

    , DebianT
    , evalDebianT
    , execDebianT
    , liftCabal
    ) where

import Control.Monad.State (evalState, evalStateT, execState, execStateT, runState, State, StateT(runStateT))
import Data.Lens.Lazy (focus)
import Debian.Debianize.Types.Atoms (Atoms, debInfo, DebInfo)
import Debian.Orphans ()
import Prelude hiding (init, log, unlines)

type CabalT m = StateT Atoms m -- Better name - CabalT?
type CabalM = State Atoms

execCabalT :: Monad m => CabalT m a -> Atoms -> m Atoms
execCabalT action atoms = execStateT action atoms

evalCabalT :: Monad m => CabalT m a -> Atoms -> m a
evalCabalT action atoms = evalStateT action atoms

runCabalT :: Monad m => CabalT m a -> Atoms -> m (a, Atoms)
runCabalT action atoms = runStateT action atoms

execCabalM :: CabalM a -> Atoms -> Atoms
execCabalM action atoms = execState action atoms

evalCabalM :: CabalM a -> Atoms -> a
evalCabalM action atoms = evalState action atoms

runCabalM :: CabalM a -> Atoms -> (a, Atoms)
runCabalM action atoms = runState action atoms

type DebianT m = StateT DebInfo m

evalDebianT :: Monad m => DebianT m a -> DebInfo -> m a
evalDebianT = evalStateT

execDebianT :: Monad m => DebianT m () -> DebInfo -> m DebInfo
execDebianT = execStateT

liftCabal :: Monad m => StateT DebInfo m a -> StateT Atoms m a
liftCabal = focus debInfo
