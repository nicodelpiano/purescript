-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Exhaustive
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

import Data.List (foldl', sortBy, nub)

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Declarations
import Language.PureScript.Environment
import Language.PureScript.Names as P

-- |
-- Finds the redundant set between two binders:
-- the first binder is a missing case and the second one is the matching binder
--
coverCasesSingle :: Environment -> ModuleName -> Binder -> Binder -> [Binder]
coverCasesSingle _ _ _ NullBinder = [NullBinder]
coverCasesSingle _ _ _ (VarBinder _) = [NullBinder]
coverCasesSingle _ _ NullBinder b = [b]
coverCasesSingle _ _ (VarBinder _) b = [b]
coverCasesSingle env mn br (NamedBinder _ bl) = coverCasesSingle env mn br bl
coverCasesSingle env mn (ConstructorBinder con bs) (ConstructorBinder con' bs')
  | con == con' = map (ConstructorBinder con) $ coverCasesMultiple env mn bs bs'
  | otherwise = [] -- con' covers nothing for con, it is maybe an overlapping case
coverCasesSingle _ _ _ _ = []

overlapCasesSingle :: Environment -> ModuleName -> Binder -> Binder -> [Binder]
overlapCasesSingle _ _ NullBinder _ = []
overlapCasesSingle _ _ (VarBinder _) _ = []
overlapCasesSingle _ _ _ NullBinder = []
overlapCasesSingle _ _ _ (VarBinder _) = []
overlapCasesSingle env mn br (NamedBinder _ bl) = overlapCasesSingle env mn br bl
overlapCasesSingle env mn (ConstructorBinder con bs) b@(ConstructorBinder con' bs')
  | con == con' = map (ConstructorBinder con) $ overlapCasesMultiple env mn bs bs'
  | otherwise = [b]
overlapCasesSingle env mn b (PositionedBinder _ _ cb) = overlapCasesSingle env mn b cb
overlapCasesSingle _ _ _ _ = []

coverCasesMultiple :: Environment -> ModuleName -> [Binder] -> [Binder] -> [[Binder]]
coverCasesMultiple env mn = go
  where
  go [] [] = [[]]
  go bs@(_:_) bs'@(_:_) = zipWith (coverCasesSingle env mn) bs bs'
  go _ _ = error "Argument lengths did not match in coverCasesMultiple."

overlapCasesMultiple :: Environment -> ModuleName -> [Binder] -> [Binder] -> [[Binder]]
overlapCasesMultiple env mn = go
  where
  go [] [] = []
  go bs@(_:_) bs'@(_:_) = if all (==[]) (zipWith (overlapCasesSingle env mn) bs bs') then [] else [bs']
  go _ _ = error "Argument lengths did not match in overlapCasesMultiple."

overlapAlternative :: Environment -> ModuleName -> CaseAlternative -> [Binder] -> [[Binder]]
overlapAlternative env mn ca uncovered = overlapCasesMultiple env mn uncovered (caseAlternativeBinders ca) 

coverAlternative :: Environment -> ModuleName -> CaseAlternative -> [Binder] -> [[Binder]]
coverAlternative env mn ca uncovered = coverCasesMultiple env mn uncovered (caseAlternativeBinders ca) 
