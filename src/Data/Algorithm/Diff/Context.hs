module Data.Algorithm.Diff.Context
    ( contextDiff
    , groups
    ) where

import Data.Algorithm.Diff (Diff(..), getGroupedDiff)

-- | Do a grouped diff and then turn it into a list of hunks, where
-- each hunk is a grouped diff with at most N elements of common
-- context around each one.
contextDiff :: Eq a => Int -> [a] -> [a] -> [[Diff [a]]]
contextDiff context a b =
    group $ swap $ trimTail $ trimHead $ concatMap split $ getGroupedDiff a b
    where
      -- Split common runs longer than 2N elements, keeping first and
      -- last N lines.
      split (Both xs ys) =
          case length xs of
            n | n > (2 * context) -> [Both (take context xs) (take context ys), Both (drop (n - context) xs) (drop (n - context) ys)]
            _ -> [Both xs ys]
      split x = [x]
      -- If split created a a pair of Both's at the beginning or end
      -- of the diff, remove the outermost.
      trimHead [] = []
      trimHead [Both _ _] = []
      trimHead [Both _ _, Both _ _] = []
      trimHead (Both _ _ : x@(Both _ _) : more) = x : more
      trimHead xs = trimTail xs
      trimTail [x@(Both _ _), Both _ _] = [x]
      trimTail (x : more) = x : trimTail more
      trimTail [] = []
      -- If we see Second before First swap them so that the deletions
      -- appear before the additions.
      swap (x@(Second _) : y@(First _) : xs) = y : x : swap xs
      swap (x : xs) = x : swap xs
      swap [] = []
      -- Split the list wherever we see adjacent Both constructors
      group xs =
          groups (\ x y -> not (isBoth x && isBoth y)) xs
          where
            isBoth (Both _ _) = True
            isBoth _ = False

-- | Group the elements whose adjacent pairs satisfy the predicate.
-- Differs from groupBy because the predicate does not have to define
-- a total ordering.
groups :: Eq a => (a -> a -> Bool) -> [a] -> [[a]]
groups f xs =
    filter (/= []) $ reverse (groups' [[]] xs)
    where
      -- Predicate satisfied, add x to the current group r and recurse with y at head
      groups' (r : rs) (x : y : xs') | f x y = groups' ((x : r) : rs) (y : xs')
      -- Predicate not satisfied, add x to current group and start a new group containing y
      groups' (r : rs) (x : y : xs') = groups' ([y] : reverse (x : r) : rs) xs'
      -- Last element, add it to the current group
      groups' (r : rs) [y] = reverse (y : r) : rs
      -- Nothing left, return result
      groups' rs [] = rs
      -- This won't happen, groups' is always called with a non-empty list in the first argument
      groups' [] (_ : _) = error "groups"
