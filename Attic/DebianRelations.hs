-- |This could be made into an instance of PropositionalFormula, but
-- it isn't yet.
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PackageImports, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Package.Debian.DebianRelations
    ( toFormula
    , ofFormula
    ) where

import Debian.Relation
import Data.Logic.Logic
import Data.Logic.Propositional.Formula
import Data.Logic.Propositional.Instances.Native

-- |There is no such thing as the True or False DebianRelation, so we
-- return something meaningless here - the relation is invalid because
-- capitalized package names "True" and "False" are invalid.  Having
-- to implement this is evidence of a weakness in the class structure.
instance Boolean Relation where
    fromBool flag = Rel (show flag) Nothing Nothing

toFormula :: Relations -> Formula Relation
toFormula xs =
    toFormula' xs
    where
      toFormula' :: [[Relation]] -> Formula Relation
      toFormula' [] = fromBool True
      toFormula' [x] = toFormula'' x
      toFormula' (x : xs') = toFormula'' x .&. toFormula' xs'

      toFormula'' :: [Relation] -> Formula Relation
      toFormula'' [] = fromBool False
      toFormula'' [x] = atomic x
      toFormula'' (x : xs') = atomic x .|. toFormula'' xs'

ofFormula :: Formula Relation -> [[Relation]]
ofFormula form =
    foldF0 c a form
    where
      -- Turn all the ands into a list
      c :: Combine (Formula Relation) -> [[Relation]]
      c (BinOp p (:&:) q) = foldF0 c a p ++ foldF0 c a q
      c (BinOp p (:|:) q) = [foldF0 c' a' p ++ foldF0 c' a' q]
      c ((:~:) p) = [[foldF0 c'' a'' p]]
      c _ = error $ "Unexpected c"
      a :: Relation -> [[Relation]]
      a x = [[x]]

      c' :: Combine (Formula Relation) -> [Relation]
      c' (BinOp p (:|:) q) = foldF0 c' a' p ++ foldF0 c' a' q
      c' ((:~:) p) = [foldF0 c'' a'' p]
      c' _ = error $ "Unexpected c'"
      a' :: Relation -> [Relation]
      a' x = [x]

      c'' :: Combine (Formula Relation) -> Relation
      c'' ((:~:) p) = foldF0 nc na p
      c'' _ = error $ "Unexpected c''"
      a'' :: Relation -> Relation
      a'' x = x

      nc :: Combine (Formula Relation) -> Relation
      nc _ = error $ "Unexpected nc"
      na :: Relation -> Relation
      na (Rel name (Just (SLT v)) arch) = Rel name (Just (GRE v)) arch
      na (Rel name (Just (SGR v)) arch) = Rel name (Just (LTE v)) arch
      na (Rel name (Just (GRE v)) arch) = Rel name (Just (SLT v)) arch
      na (Rel name (Just (LTE v)) arch) = Rel name (Just (SGR v)) arch
      na _ = error $ "Unexepected na"

{-
instance Negatable Relations where
    (.~.) x = ofFormula ((.~.) (toFormula x :: Formula Relation))
    negated _ = False

instance PropositionalFormula Relations Relation where
    atomic x = [[x]]
    foldF0 c a form = foldF0 c a (toFormula form)
    asBool x = Nothing

instance Boolean Relations where
    fromBool x = error "fromBool Relations"

instance Logic Relations where
    x .<=>. y = Combine (BinOp  x (:<=>:) y)
    x .=>.  y = Combine (BinOp  x (:=>:)  y)
    x .|.   y = [[x, y]]
    x .&.   y = [[x], [y]]

prettyPropForm :: (a -> Doc) -> PropForm a -> Doc
prettyPropForm p (A a) = cat [text "(A ", p a, text ")"]
prettyPropForm _ F = text "F"
prettyPropForm _ T = text "T"
prettyPropForm p (N x') = cat [text "(N ", prettyPropForm p x', text ")"]
prettyPropForm p (CJ xs) = cat ([text "(CJ "] ++ map (prettyPropForm p) xs ++ [text ")"])
prettyPropForm p (DJ xs) = cat ([text "(DJ "] ++ map (prettyPropForm p) xs ++ [text ")"])
prettyPropForm p (SJ xs) = cat ([text "(SJ "] ++ map (prettyPropForm p) xs ++ [text ")"])
prettyPropForm p (EJ xs) = cat ([text "(EJ "] ++ map (prettyPropForm p) xs ++ [text ")"])

pretty :: PropForm Relation -> Doc
pretty = prettyPropForm prettyRelation
-}
