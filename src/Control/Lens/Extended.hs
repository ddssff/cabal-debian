{-# LANGUAGE Rank2Types #-}
module Control.Lens.Extended (
  module Control.Lens,
  (~=),
  (-<=),
  (++=),
  (+++=),
  (~?=),
  getL,
  setL,
  modL,
  access
  ) where
import Control.Monad.State
import Control.Lens
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)

-- | Set a lens value.  (This is a version of Data.Lens.Lazy.~= that returns () instead of b.)
(~=) :: Monad m => Lens' a b -> b -> StateT a m ()
l ~= x = modify (l .~ x)

getL :: Lens' a b -> a -> b
getL lns x = x ^. lns -- view lns x

setL :: Lens' a b -> b -> a -> a
setL lns y x = lns .~ y $ x -- set lens y x

modL :: Lens' a b -> (b -> b) -> a -> a
modL lns f x = lns %~ f $ x -- over lns f x

access :: (Monad m) => Lens' a b -> StateT a m b
access lns = get >>= return . getL lns

-- | Insert an element into a @(Set b)@
(-<=) :: (Monad m, Ord b) => Lens' a (Set b) -> b -> StateT a m ()
l -<= x = l %= Set.insert x

-- | Insert an element into a @(Map b c)@
(++=) :: (Monad m, Ord b) => Lens' a (Map b c) -> (b, c) -> StateT a m ()
l ++= (k, a) = l %= Map.insert k a

-- | Insert an element into a @(Map b (Set c))@
(+++=) :: (Monad m, Ord b, Monoid c) => Lens' a (Map b c) -> (b, c) -> StateT a m ()
l +++= (k, a) = l %= Map.insertWith mappend k a

-- | Set @b@ if it currently isNothing and the argument isJust, that is
--  1. Nothing happens if the argument isNothing
--  2. Nothing happens if the current value isJust
(~?=) :: Monad m => Lens' a (Maybe b) -> Maybe b -> StateT a m ()
l ~?= (Just x) = l %= maybe (Just x) Just >> return ()
_ ~?= _ = return ()

