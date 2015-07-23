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
overlapCasesSingle :: Binder -> Binder -> [Binder]
overlapCasesSingle NullBinder _ = []
overlapCasesSingle (VarBinder _) _ = []
overlapCasesSingle _ NullBinder = []
overlapCasesSingle _ (VarBinder _) = []
overlapCasesSingle br (NamedBinder _ bl) = overlapCasesSingle br bl
overlapCasesSingle (ConstructorBinder con bs) b@(ConstructorBinder con' bs')
  | con == con' = map (ConstructorBinder con) $ overlapCasesMultiple bs bs'
  | otherwise = [b]
overlapCasesSingle (ObjectBinder bs) b@(ObjectBinder bs') =
  if any (/=[]) (uncurry (zipWith overlapCasesSingle) (unzip binders)) then [b] else []
  where
  sortNames = sortBy (compare `on` fst)

  (sbs, sbs') = (sortNames bs, sortNames bs')

  compB :: a -> Maybe a -> Maybe a -> (a, a)
  compB e b b' = (fm b, fm b')
    where
    fm = fromMaybe e

  compBS :: Eq a => b -> a -> Maybe b -> Maybe b -> (a, (b, b))
  compBS e s b b' = (s, compB e b b')

  (sortedNames, binders) = unzip $ genericMerge (compBS NullBinder) sbs sbs'
overlapCasesSingle b (PositionedBinder _ _ cb) = overlapCasesSingle b cb
overlapCasesSingle _ _ = []

overlapCasesMultiple :: [Binder] -> [Binder] -> [[Binder]]
overlapCasesMultiple = go
  where
  go [] [] = []
  go bs@(_:_) bs'@(_:_) = if all (==[]) (zipWith overlapCasesSingle bs bs') then [] else [bs']
  go _ _ = error "Argument lengths did not match in overlapCasesMultiple."

overlapAlternative :: CaseAlternative -> [Binder] -> [[Binder]]
overlapAlternative ca uncovered = overlapCasesMultiple uncovered (caseAlternativeBinders ca) 
