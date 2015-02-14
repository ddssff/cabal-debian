{-# LANGUAGE Rank2Types, GADTs #-}
module OldLens
    ( Lens
    , lens
    , OldLens.iso
    , getL
    , setL
    , modL
    , access
    , focus
    , (OldLens.%=)
    , (OldLens.~=)
    ) where

import Control.Monad.State (StateT, get, put)
import Control.Lens hiding (Lens, lens)
import Control.Lens.Internal.Zoom (Zoomed)
import qualified Control.Lens (lens)

-- Need to reverse order of compositions
-- Need to add {-# LANGUAGE Rank2Types #-} to some modules
-- If you use fancy name functions with nameMakeLens you need
--   to convert your types to simple "starts with _" format, and you need
--   to call makeLenses for each type name individually.

type Lens a b = Lens' a b

lens :: (a -> b) -> (b -> a -> a) -> Lens a b
lens getter setter = Control.Lens.lens getter (flip setter)

iso :: (a -> b) -> (b -> a) -> Lens a b
iso = Control.Lens.iso

getL :: Lens a b -> a -> b
getL lns x = x ^. lns -- view lns x

setL :: Lens a b -> b -> a -> a
setL lns y x = set lns y x

modL :: Lens a b -> (b -> b) -> a -> a
modL lns f x = set lns (f (view lns x)) x

access :: Monad m => Lens a b -> StateT a m b
access lns = get >>= return Prelude.. view lns

(~=) :: Monad m => Lens a b -> b -> StateT a m ()
lns ~= y = get >>= \x -> put (set lns y x)

(%=) :: Monad m => Lens a b -> (b -> b) -> StateT a m ()
lns %= f = get >>= \x -> put (set lns (f (getL lns x)) x)

-- focus :: Monad m => Lens a b -> StateT b m c -> StateT a m c
--focus (Lens f) (StateT g) = StateT $ \a -> case f a of
--  StoreT (Identity h) b -> liftM (second h) (g b)
focus :: forall m n s t c. (Zoom m n s t, Zoomed n ~ Zoomed m) => LensLike' (Zoomed m c) t s -> m c -> n c
focus lns st = zoom lns st

{-
Couldn't match type
 ‘(b -> Zoomed (StateT b m) c b) -> a -> Zoomed (StateT b m) c a’
                  with
 ‘forall (f :: * -> *). Functor f => (b -> f b) -> a -> f a’
    Expected type: Lens a b -> StateT b m c -> StateT a m c
      Actual type: LensLike' (Zoomed (StateT b m) c) a b -> StateT b m c -> StateT a m c
-}
