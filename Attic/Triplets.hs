{-# LANGUAGE RankNTypes, ScopedTypeVariables, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS -Wwarn -fno-warn-name-shadowing -fno-warn-orphans #-}
module Triplets
    ( gzipWithM3
    , gzipWithT3
    , gzipWithA3
    , gzip3
    , gzipQ3
    , gzipBut3
    , gzipButA3
    -- , gzipBut3'
    , extQ2
    , extQ3
    , extT3
    , mkQ3
    , mkQ2
    -- , mkP3
    -- , gzip3F
    , mergeBy
    --, mergeByM
    --, GenericA
    , GB, GM, GA, PB, PM, PA
    ) where

import Prelude hiding (GT)
import Control.Applicative (Applicative(..))
import Control.Applicative.Error (Failing(..))
import Control.Monad (MonadPlus(mzero, mplus))
import Data.Generics (Data, Typeable, Typeable1, toConstr, cast, gcast, gmapAccumQ, gshow, gfoldlAccum,
                      unGT, GenericT, GenericT'(GT), gmapAccumT,
                      unGM, GenericM, GenericM'(GM), gmapAccumM,
                      unGQ, GenericQ, GenericQ'(GQ), gmapQ)
import Data.Maybe (fromMaybe)

instance Monad Failing where
  return = Success
  m >>= f =
      case m of
        (Failure errs) -> (Failure errs)
        (Success a) -> f a
  fail errMsg = Failure [errMsg]

instance MonadPlus Failing where
    mzero = Failure []
    mplus (Failure xs) (Failure ys) = Failure (xs ++ ys)
    mplus success@(Success _) _ = success
    mplus _ success@(Success _) = success
  
deriving instance Typeable1 Failing
deriving instance Data a => Data (Failing a)
deriving instance Read a => Read (Failing a)
deriving instance Eq a => Eq (Failing a)
deriving instance Ord a => Ord (Failing a)

cast' :: (Monad m, Typeable a, Typeable b) => a -> m b
cast' = maybe (fail "cast") return . cast

--orElse' = mplus

-- As originally defined: Twin map for transformation

{-
gzipWithT2 :: GenericQ (GenericT) -> GenericQ (GenericT)
gzipWithT2 f x y = case gmapAccumT perkid funs y of
                   ([], c) -> c
                   _       -> error "gzipWithT2"
 where
 perkid a d = (tail a, unGT (head a) d)
 funs = gmapQ (\k -> GT (f k)) x
-}


-- As originally defined: Twin map for transformation

{-
gzipWithM2 :: Monad m => GenericQ (GenericM m) -> GenericQ (GenericM m)
gzipWithM2 f x y = case gmapAccumM perkid funs y of
                   ([], c) -> c
                   _       -> error "gzipWithM"
 where
 perkid a d = (tail a, unGM (head a) d)
 funs = gmapQ (\k -> GM (f k)) x
-}


-- As originally defined: generic zip

{-
gzip2 ::
   (forall x. Data x => x -> x -> Maybe x)
 -> (forall x. Data x => x -> x -> Maybe x)

gzip2 f = gzip2' f'
 where
 f' :: GenericQ (GenericM Maybe)
 f' x y = cast x >>= \x' -> f x' y
 gzip2' :: GenericQ (GenericM Maybe) -> GenericQ (GenericM Maybe)
 gzip2' f x y =
   f x y
   `orElse`
   if toConstr x == toConstr y
     then gzipWithM2 (gzip2' f) x y
     else Nothing
-}

-- For three args now

--type GenericT = forall a. Data a => a -> a
--type GenericQ r = forall a. Data a => a -> r
--type GenericM m = forall a. Data a => a -> m a

gzipWithT3 ::
   GenericQ (GenericQ (GenericT))
 -> GenericQ (GenericQ (GenericT))
gzipWithT3 f x y z =
   case gmapAccumT perkid funs z of
     ([], c) -> c
     _       -> error "gzipWithT3"
 where
 perkid a d = (tail a, unGT (head a) d)
 funs = case gmapAccumQ perkid' funs' y of
          ([], q) -> q
          _       -> error "gzipWithT3"
  where
   perkid' a d = (tail a, unGQ (head a) d)
   funs' = gmapQ (\k -> (GQ (\k' -> GT (f k k')))) x

gzipWithM3 :: Monad m
 => GenericQ (GenericQ (GenericM m))
 -> GenericQ (GenericQ (GenericM m))
gzipWithM3 f x y z =
    case gmapAccumM perkid funs z of
      ([], c) -> c
      _       -> error "gzipWithM3"
    where
      perkid a d = (tail a, unGM (head a) d)
      funs = case gmapAccumQ perkid' funs' y of
               ([], q) -> q
               _       -> error "gzipWithM3"
          where
            perkid' a d = (tail a, unGQ (head a) d)
            funs' = gmapQ (\k -> (GQ (\k' -> GM (f k k')))) x

gzipWithA3 :: forall f. Applicative f => GA f -> GA f
gzipWithA3 f x y z =
    case gmapAccumA perkid funs z of
      ([], c) -> c
      _       -> error "gzipWithA3"
    where
      perkid a d = (tail a, unGM (head a) d)
      funs = case gmapAccumQ perkid' funs' y of
               ([], q) -> q
               _       -> error "gzipWithA3"
          where
            perkid' a d = (tail a, unGQ (head a) d)
            funs' = gmapQ (\k -> (GQ (\k' -> GM (f k k')))) x

type GB = GenericQ (GenericQ (GenericQ Bool))
-- ^ Generic Bool Query, (Data a, Data b, Data c) => a -> b -> c -> Bool
type GM = MonadPlus m => GenericQ (GenericQ (GenericM m))
-- ^ Generic Maybe Query, (Data a, Data b, Data c) => a -> b -> c -> Maybe c
type PB = forall x. Data x => x -> x -> x -> Bool
-- ^ Polymorphic Bool Query
type PM = forall m x. (MonadPlus m, Data x) => x -> x -> x -> m x
-- ^ Polymorphic Failing Query, forall x. Data x => x -> x -> x -> Failing x
type GA f = GenericQ (GenericQ (GenericM f))
-- ^ Generic Applicative Query, forall a. Data a => a -> (forall b. Data b => b -> (forall c. Data c => c -> f c))
type PA f = forall x. Data x => x -> x -> x -> f x
-- ^ Polymorphic Applicative Query

-- | gmapA with accumulation (untested)
-- (Move to Data.Generics.Twins)
gmapAccumA :: forall a d f. (Data d, Applicative f)
           => (forall e. Data e => a -> e -> (a, f e))
           -> a -> d -> (a, f d)
gmapAccumA f a0 d0 = gfoldlAccum k z a0 d0
    where
      k :: forall d e. (Data d) =>
           a -> f (d -> e) -> d -> (a, f e)
      k a c d = let (a',d') = f a d
                    c' = c <*> d'
                in (a', c')
      z :: forall t a f. (Applicative f) =>
           t -> a -> (t, f a)
      z a x = (a, pure x)

-- |The purpose of gzip3 is to map a polymorphic (generic) function
-- over the "elements" of three instances of a type.  The function
-- returns a Maybe value of the same type as the elements passed.  If
-- it returns a Just the subtree is not traversed, the returned value
-- is used.  If it returns Nothing the subtree is traversed.  This
-- traversal may succeed where the top level test failed, resulting in
-- a successful zip.  For example, the merge function wouldn't merge
-- these three values:
--     (1, 1)  (1, 2) (2, 1) -> (?, ?)
-- but it could merge the two unzipped triples:
--     (1, 1, 2) -> 2
--     (1, 2, 1) -> 2
--       -> (2, 2)
gzip3 :: PM -> PM
gzip3 f = gzipBut3 f gzipQ3

-- |This is the minimal condition for recursing into a value - the
-- constructors must all match.
gzipQ3 :: GM
gzipQ3 x y z = 
    if and [toConstr x == toConstr y, toConstr y == toConstr z]
    then return undefined
    else fail ("Conflict: x=" ++ gshow x ++ " y=" ++ gshow y ++ " z=" ++ gshow z)

-- |This function adds a test to limit the recursion of gzip3.  For
-- example, with the merge function mentioned above you might want to
-- avoid merging strings character by character:
-- 
--     gzip3 merge "dim" "kim" "dip" -> Just "kip" (no!)
-- 
-- so you would pass a limiting function to prevent recursing into strings:
-- 
--     let continue =
--          (\ x y z -> extQ3 gzipQ3 x y z) x y z
--          where
--            stringFail :: String -> String -> String -> Bool
--            stringFail _ _ _ = False
--     gzipBut3 merge continue "dim" "kim" "dip" -> Nothing
-- 
-- this can also save a lot of time examining all the heads and tails
-- of every string.
gzipBut3 :: PM -> GM -> PM
gzipBut3 merge continue x y z =
    gzip3' merge' x y z
    where
      -- If the three elements aren't all the type of f's arguments,
      -- this expression will return Nothing.  Also, the f function
      -- might return Nothing.  In those cases we call gzipWithM3 to
      -- traverse the sub-elements.
      merge' :: GM
      merge' x y z = cast' x >>= \x' -> cast' y >>= \y' -> merge x' y' z
      gzip3' :: GM -> GM
      gzip3' merge x y z =
          merge x y z
         `mplus`
          (continue x y z >> gzipWithM3 (gzip3' merge) x y z)

-- | gzipWithA3 plus a continue function to prevent recursion into
-- particular types.  (UNTESTED)
gzipButA3 :: forall f. (Applicative f) => PM -> GB -> PA f -> PA f
gzipButA3  merge continue conflict x y z =
    gzip3' x y z
    where
      gzip3' :: GA f
      gzip3' x y z =
          case merge' x y z of
            Just x' -> pure x'
            Nothing ->
                if continue x y z
                then gzipWithA3 gzip3' x y z
                else conflict' x y z
      merge' :: GM
      merge' x y z =
          case (cast x, cast y) of
            (Just x', Just y') -> merge x' y' z
            _ -> fail "type conflict"
      conflict' :: GA f
      conflict' x y z =
          case (cast x, cast y) of
            (Just x', Just y') -> conflict x' y' z
            _ -> error "type conflict"

-- | Not to be confused with ext2Q, this extends queries of two
-- arguments (rather than queries involving constructors with two type
-- parameters.)
extQ2 :: (Typeable a, Typeable b, Typeable d, Typeable e)
      => (a -> b -> r) -> (d -> e -> r) -> a -> b -> r
extQ2 d q x y = fromMaybe (d x y) $ cast x >>= \x' -> cast y >>= \y' -> Just (q x' y')

extQ3 :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f)
      => (a -> b -> c -> r) -> (d -> e -> f -> r) -> a -> b -> c -> r
extQ3 d q x y z = fromMaybe (d x y z) $ cast x >>= \x' -> cast y >>= \y' -> cast z >>= \z' -> Just (q x' y' z')

mkQ3 :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f)
     => r -> (a -> b -> c -> r) -> d -> e -> f -> r
mkQ3 d q x y z = extQ3 (\ _ _ _ -> d) q x y z

extT3 :: (Typeable a, Typeable b)
      => (a -> a -> a -> Maybe a) -> (b -> b -> b -> Maybe b) -> a -> a -> a -> Maybe a
extT3 d q x y z = fromMaybe (d x y z) $ cast x >>= \x' -> cast y >>= \y' -> cast z >>= \z' -> gcast (q x' y' z')

mkQ2 :: (Data a, Data b, Data c) => (a -> b -> r) -> (c -> c -> r) -> a -> b -> r
mkQ2 d q x y = fromMaybe (d x y) $ cast x >>= \x' -> cast y >>= \y' -> Just (q x' y')

-- |This function implements the f function used to do three way
-- merging.  A triplet (original, new1, new2) conflicts if the two
-- new values each differ from the original, and from each other.
-- Otherwise, the new value that differs from the original is kept, or
-- either of the new values if they match.  However, even if the
-- values conflict, it still might be possible to do the merge by
-- examining the components of the value.  So conflict is typically
-- (\ _ _ _ -> Nothing), while eq could be geq, but it could also
-- just return false for more complex datatypes that we don't want
-- to repeatedly traverse.
mergeBy :: forall a m. MonadPlus m => (a -> a -> a -> m a) -> (a -> a -> Bool) -> a -> a -> a -> m a
mergeBy conflict eq original left right =
    if eq original left then return right
    else if eq original right then return left
         else if eq left right then return left
              else conflict original left right
