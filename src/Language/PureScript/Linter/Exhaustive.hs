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
import Data.Maybe (fromMaybe)
import Data.List (delete)

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
-- Qualifies a propername from a given qualified propername
--
qualifyProperName :: ProperName -> Qualified ProperName -> Qualified ProperName
qualifyProperName pn qpn = Qualified (Just mn) pn
  where
  (mn, _) = qualify defmn qpn
  defmn = moduleNameFromString "Main"

-- |
-- Given an environment and a datatype or newtype name,
-- this function returns the associated data constructors if it's the case of a datatype
-- where: - ProperName is the name of the constructor (for example, "Nothing" in Maybe)
--        - [Type] is the list of arguments, if it has (for example, "Just" has [TypeVar "a"])
--
getConstructors :: Environment -> (Qualified ProperName) -> [(ProperName, [Type])]
getConstructors env n = go lnte
  where
  qpn :: Qualified ProperName
  qpn = getConsDataName n

  getConsDataName :: (Qualified ProperName) -> (Qualified ProperName)
  getConsDataName con = qualifyProperName nm con
    where
    nm = case getConsInfo con of
           Nothing -> error $ "ProperName " ++ show con ++ " not in the scope of the current environment in getConsDataName."
           Just (_, pm, _, _) -> pm

  getConsInfo :: (Qualified ProperName) -> Maybe (DataDeclType, ProperName, Type, [Ident])
  getConsInfo con = M.lookup con dce
    where
    dce :: M.Map (Qualified ProperName) (DataDeclType, ProperName, Type, [Ident])
    dce = dataConstructors env

  lnte :: Maybe (Kind, TypeKind) 
  lnte = M.lookup qpn (types env)

  go :: Maybe (Kind, TypeKind) -> [(ProperName, [Type])]
  go (Just (_, DataType _ pt)) = pt
  go _ = []

-- |
-- Replicates a wildcard binder
--
initialize :: Int -> [Binder]
initialize l = replicate l NullBinder

-- |
-- Find the uncovered set between two binders:
-- the first binder is the case we are trying to cover
-- the second one is the matching binder
--
missingCasesSingle :: Environment -> Binder -> Binder -> [Binder]
missingCasesSingle _ _ NullBinder = []
missingCasesSingle _ _ (VarBinder _) = []
missingCasesSingle env NullBinder cb@(ConstructorBinder con bs) =
  concatMap (\cp -> missingCasesSingle env cp cb) all_pat
  where
  all_pat = map (\(p,t) -> ConstructorBinder (qualifyProperName p con) (initialize $ length t)) $ getConstructors env con
missingCasesSingle env cb@(ConstructorBinder con bs) (ConstructorBinder con' bs')
  | con == con' = map (ConstructorBinder con) (missingCasesMultiple env bs bs')
  | otherwise = [cb]
missingCasesSingle env b (PositionedBinder _ _ cb) = missingCasesSingle env b cb
missingCasesSingle env NullBinder (BooleanBinder b) = [BooleanBinder $ not b]
missingCasesSingle env (VarBinder _) (BooleanBinder b) = [BooleanBinder $ not b]
missingCasesSingle _ b _ = [b]

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
    | otherwise = map (: xs) miss ++ map (y :) (go xs ys)
    where
    miss = missingCasesSingle env x y
  go _ _ = error "Argument lengths did not match in missingCasesMultiple."

-- |
-- Is the first list of binders covered by the second?
--
isExhaustive :: Environment -> [Binder] -> [Binder] -> Bool
isExhaustive env bs bs' = null $ missingCasesMultiple env bs' bs

isExhaustiveMultiple :: Environment -> [[Binder]] -> [Binder] -> Bool
isExhaustiveMultiple env bss bs = foldl (\acc bs' -> acc && isExhaustive env bs' bs) True bss

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
  -- ugly def... we might want to change this
  step :: [[Binder]] -> CaseAlternative -> [[Binder]]
  step uncovered ca = concatMap (\u -> filter (\p -> isExhaustive env u p || isExhaustiveMultiple env uncovered p) (missingCases env u ca)) uncovered

  initial :: [Binder]
  initial = initialize numArgs
    where
    numArgs = length . caseAlternativeBinders . head $ cas 

  makeResult :: [[Binder]] -> m ()
  makeResult bss | null bss = return ()
                 | otherwise = tell $ (errorMessage $ NotExhaustivePattern bss) --(error "TODO: add new warning type")

checkExhaustiveModule :: forall m. (Applicative m, MonadWriter MultipleErrors m) => Environment -> Module -> m ()
checkExhaustiveModule env (Module _ _ ds _) = 
  let (f, _, _) = everywhereOnValuesTopDownM return checkExpr return
  in mapM_ f ds
  where
  checkExpr :: Expr -> m Expr
  checkExpr c@(Case _ cas) = checkExhaustive env cas >> return c
  checkExpr other = return other







