{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Data.Algorithm.Diff.Pretty
    ( prettyDiff
    , prettyHunk
    , prettyChange
    ) where

import Data.Algorithm.Diff (Diff(..))
import Data.Monoid ((<>))
import Text.PrettyPrint (Doc, text, empty, hcat)

-- | Pretty print a list of hunks.
prettyDiff :: forall c. Doc -> Doc -> (c -> Doc) -> [[Diff [c]]] -> Doc
prettyDiff _ _ _ [] = empty
prettyDiff old new prettyElem hunks =
    hcat . map (<> text "\n") $ (text "--- " <> old :
                                 text "+++ " <> new :
                                 concatMap (prettyHunk prettyElem) hunks)

-- | Pretty print a hunk of adjacent changes
prettyHunk :: (c -> Doc) -> [Diff [c]] -> [Doc]
prettyHunk prettyElem hunk =
    text "@@" : concatMap (prettyChange prettyElem) hunk

-- | Pretty print a single change (e.g. a line of a text file)
prettyChange :: (c -> Doc) -> Diff [c] -> [Doc]
prettyChange prettyElem (Both ts _) = map (\ l -> text " " <> prettyElem l) ts
prettyChange prettyElem (First ts)  = map (\ l -> text "-" <> prettyElem l) ts
prettyChange prettyElem (Second ts) = map (\ l -> text "+" <> prettyElem l) ts
