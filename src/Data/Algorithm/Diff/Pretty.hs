{-# LANGUAGE OverloadedStrings #-}
module Data.Algorithm.Diff.Pretty
    ( prettyDiff
    ) where

import Data.Algorithm.Diff (Diff(..))
import Data.Monoid (mconcat, (<>))
import Debian.Pretty (Pretty(pretty), Doc, text, empty)

-- | Format the output of 'contextDiff' to look approximately like the
-- output of the unix diff command.
prettyDiff :: (Pretty a, Pretty b, Pretty c) => a -> b -> [[Diff [c]]] -> Doc
prettyDiff _ _ [] = empty
prettyDiff old new hunks =
    text "--- " <> pretty old <> text "\n+++ " <> pretty new <> text "\n" <> mconcat (map (\ hunk -> text "@@\n" <> p hunk) hunks)
    where
      p (Both ts _ : more) = mconcat (map (\ l -> text " " <> pretty l <> text "\n") ts) <> p more
      p (First ts : more)  = mconcat (map (\ l -> text "-" <> pretty l <> text "\n") ts) <> p more
      p (Second ts : more) = mconcat (map (\ l -> text "+" <> pretty l <> text "\n") ts) <> p more
      p [] = empty
