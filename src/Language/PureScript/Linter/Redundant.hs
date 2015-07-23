-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Linter.Redundant
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- | Module for redundancy checking over pattern matching definitions
-- | The algorithm analyses the clauses of a definition one by one from top
-- | to bottom, where in each step it has the cases already defined (redundant),
-- | and it generates the new set of redundant cases.
-- 
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Linter.Redundant where 

import Data.Maybe (fromMaybe)
import Data.List (foldl', sortBy, nub)
import Data.Function (on)

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Declarations
import Language.PureScript.Environment
import Language.PureScript.Names as P

import Language.PureScript.Linter.Utils
import Language.PureScript.Linter.Bindable

-- |
-- Given two binders (the first one is an uncovered binder),
-- tells whether or not the second binder covers the first one.
--
coverCasesSingle :: Binder -> Binder -> [Binder]
coverCasesSingle _ NullBinder = [NullBinder]
coverCasesSingle _ (VarBinder _) = [NullBinder]
coverCasesSingle NullBinder b = [b]
coverCasesSingle (VarBinder _) b = [b]
coverCasesSingle br (NamedBinder _ bl) = coverCasesSingle br bl
coverCasesSingle (ConstructorBinder con bs) (ConstructorBinder con' bs')
  | con == con' = map (ConstructorBinder con) $ coverCasesMultiple bs bs'
  | otherwise = [] -- con' covers nothing for con, it is maybe an overlapping case
coverCasesSingle _ _ = []

coverCasesMultiple :: [Binder] -> [Binder] -> [[Binder]]
coverCasesMultiple = go
  where
  go [] [] = [[]]
  go bs@(_:_) bs'@(_:_) = zipWith coverCasesSingle bs bs'
  go _ _ = error "Argument lengths did not match in coverCasesMultiple."

coverAlternative :: CaseAlternative -> [Binder] -> [[Binder]]
coverAlternative ca uncovered = coverCasesMultiple uncovered (caseAlternativeBinders ca) 

-- |
-- Given two binders (the first one is an uncovered binder),
-- tells whether or not the second binder overlaps the first one.
--

instance Bindable [Binder] where
  bind NullBinder _ = []
  bind (VarBinder _) _ = []
  bind _ NullBinder = []
  bind _ (VarBinder _) = []
  bind br (NamedBinder _ bl) = bind br bl
  bind (ConstructorBinder con bs) b@(ConstructorBinder con' bs')
    | con == con' = map (ConstructorBinder con) $ multipleBind bs bs'
    | otherwise = [b]
  bind (ObjectBinder bs) bin@(ObjectBinder bs') =
    if any (/=([] :: [Binder])) (uncurry (zipWith bind) (unzip binders)) then [bin] else []
    where
    sortNames = sortBy (compare `on` fst)

    (sbs, sbs') = (sortNames bs, sortNames bs')

    compB :: a -> Maybe a -> Maybe a -> (a, a)
    compB e b b' = (fm b, fm b')
      where
      fm = fromMaybe e

    compBS :: Eq a => b -> a -> Maybe b -> Maybe b -> (a, (b, b))
    compBS e s b b' = (s, compB e b b')

    (_, binders) = unzip $ genericMerge (compBS NullBinder) sbs sbs'
  bind (BooleanBinder bl) b@(BooleanBinder br)
    | bl == br = []
    | otherwise = [b]
  bind (NumberBinder _) b@(NumberBinder _) = [b]
  bind b (PositionedBinder _ _ cb) = bind b cb
  bind _ _ = []

  multipleBind = go
    where
    go [] [] = []
    go bs@(_:_) bs'@(_:_) = if all (==([]::[Binder])) (zipWith bind bs bs') then [] else [bs']
    go _ _ = error "Argument lengths did not match in overlapCasesMultiple."

overlapAlternative :: CaseAlternative -> [Binder] -> [[Binder]]
overlapAlternative ca uncovered = multipleBind uncovered (caseAlternativeBinders ca) 
