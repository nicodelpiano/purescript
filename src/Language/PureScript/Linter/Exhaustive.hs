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
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Linter.Exhaustive 
  ( checkExhaustive
  , checkExhaustiveModule
  ) where

import qualified Data.Map as M

import Control.Applicative
import Control.Monad.Writer.Class

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Declarations
import Language.PureScript.Environment
import Language.PureScript.Names as P
import Language.PureScript.Kinds
import Language.PureScript.Types as P
import Language.PureScript.Errors

import Language.PureScript.AST.Traversals (everywhereOnValuesTopDownM)

-- |
-- | Constructors handling
-- |

-- |
-- Given an environment and a datatype or newtype name,
-- this function returns the associated data constructors if it's the case of a datatype
-- where: - ProperName is the name of the constructor (for example, "Nothing" in Maybe)
--        - [Type] is the list of arguments, if it has (for example, "Just" has [TypeVar "a"])
--
getConstructors :: Environment -> (Qualified ProperName) -> [(ProperName, [Type])]
getConstructors env n = go lnte
  where
  lnte :: Maybe (Kind, TypeKind) 
  lnte = M.lookup n (types env)
  go :: Maybe (Kind, TypeKind) -> [(ProperName, [Type])]
  go (Just (_, DataType _ pt)) = pt
  go _ = []

-- |
-- Find the uncovered set between two binders:
-- the first binder is the case we are trying to cover
-- the second one is the matching binder
--
missingCasesSingle :: Environment -> Binder -> Binder -> [Binder]
missingCasesSingle _ _ NullBinder = []
missingCasesSingle _ _ (VarBinder _) = []
missingCasesSingle _ b _ = [b]
{-
missingCasesSingle env (ConstructorBinder con bs) b =
  case b of
    NullBinder -> undefined
    VarBinder _ -> undefined
    _ -> undefined
-- cons-cons
-- ConstructorBinder (Qualified ProperName) [Binder]
missingCasesSingle env cb@(ConstructorBinder con bs) (ConstructorBinder con' bs')
  | con == con' = undefined 
  | otherwise = [cb]
-}

-- |
-- Returns the uncovered set of binders
-- the first argument is the list of uncovered binders
-- the second argument is a clause of a pattern matching def
--
missingCasesMultiple :: Environment -> [Binder] -> [Binder] -> [[Binder]]
missingCasesMultiple env = go
  where
  go [] _ = []
  go (x:xs) (y:ys) 
    | null miss = map (x :) (go xs ys)
    | otherwise = map (: xs) miss ++ map (x :) (go xs ys)
    where
    miss = missingCasesSingle env x y
  go _ _ = error "Argument lengths did not match in missingCasesMultiple."

-- |
-- Returns the uncovered set of case alternatives
-- TODO: handle guards
missingCases :: Environment -> [Binder] -> CaseAlternative -> [[Binder]]
missingCases env uncovered ca = missingCasesMultiple env uncovered (caseAlternativeBinders ca)

-- |
-- Main exhaustivity checker function
-- Returns the uncovered set of case alternatives.
-- 
checkExhaustive :: forall m. (MonadWriter MultipleErrors m) => Environment -> [CaseAlternative] -> m ()
checkExhaustive env cas = makeResult $ foldl step [initial] cas
  where
  step :: [[Binder]] -> CaseAlternative -> [[Binder]]
  step uncovered ca = concatMap (\u -> missingCases env u ca) uncovered

  initial :: [Binder]
  initial = replicate numArgs NullBinder
    where
    numArgs = length . caseAlternativeBinders . head $ cas 

  makeResult :: [[Binder]] -> m ()
  makeResult bss | null bss = return ()
                 | otherwise = tell (error "TODO: add new warning type")

checkExhaustiveModule :: forall m. (Applicative m, MonadWriter MultipleErrors m) => Environment -> Module -> m ()
checkExhaustiveModule env (Module _ _ ds _) = 
  let (f, _, _) = everywhereOnValuesTopDownM return checkExpr return
  in mapM_ f ds
  where
  checkExpr :: Expr -> m Expr
  checkExpr c@(Case _ cas) = checkExhaustive env cas >> return c
  checkExpr other = return other








