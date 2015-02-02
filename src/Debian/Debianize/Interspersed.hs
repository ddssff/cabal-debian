-- | A class used while converting Cabal dependencies into Debian
-- dependencies.

{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Debian.Debianize.Interspersed
    ( Interspersed(..)
    ) where

import Debug.Trace (trace)

-- | A class of Bs insterspersed with Cs.  It is used when converting
-- the cabal dependencies to debian, where the "around" type is the
-- binary package name and the "between" type is the version number.
-- 
-- Minimum implementation is a method to return the leftmost B, and
-- another to return the following (C,B) pairs.  Its unfortunate to
-- require lists in the implementation, a fold function would be
-- better (though I find implementing such folds to be a pain in the
-- you-know-what.)
-- 
-- The class provides implementations of three folds, each of which
-- exposes slightly different views of the data.
class Interspersed t around between | t -> around, t -> between where
    leftmost :: t -> around
    pairs :: t -> [(between, around)]

    foldTriples :: (around -> between -> around -> r -> r) -> r -> t -> r
    foldTriples f r0 x = snd $ foldl (\ (b1, r) (c, b2) -> (b1, f b1 c b2 r)) (leftmost x, r0) (pairs x)

    -- Treat the b's as the centers and the c's as the things to their
    -- left and right.  Use Maybe to make up for the missing c's at the
    -- ends.
    foldInverted :: (Maybe between -> around -> Maybe between -> r -> r) -> r -> t -> r
    foldInverted f r0 x =
        (\ (bn, an, r) -> f bn an Nothing r) $
           foldl g (Nothing, leftmost x, r0) (pairs x)
        where
          g (b1, a1, r) (b2, a2) = (Just b2, a2, f b1 a1 (Just b2) r)

    foldArounds :: (around -> around -> r -> r) -> r -> t -> r
    foldArounds f r0 x = snd $ foldl (\ (a1, r) (_, a2) -> (a2, f a1 a2 r)) (leftmost x, r0) (pairs x)

    foldBetweens :: (between -> r -> r) -> r -> t -> r
    foldBetweens f r0 x = foldl (\ r (b, _) -> (f b r)) r0 (pairs x)

-- | An example
data Splits = Splits Double [(String, Double)] deriving Show

instance Interspersed Splits Double String where
    leftmost (Splits x _) = x
    pairs (Splits _ x) = x

_splits :: Splits
_splits = Splits 1.0 [("between 1 and 2", 2.0), ("between 2 and 3", 3.0)]

_test1 :: ()
_test1 = foldTriples (\ l s r () -> trace ("l=" ++ show l ++ " s=" ++ show s ++ " r=" ++ show r) ()) () _splits

_test2 :: ()
_test2 = foldInverted (\ sl f sr () -> trace ("sl=" ++ show sl ++ " f=" ++ show f ++ " sr=" ++ show sr) ()) () _splits
