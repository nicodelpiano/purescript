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

module Language.PureScript.Linter.Exhaustive where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Declarations
import Language.PureScript.Environment
import Language.PureScript.Names as P
import Language.PureScript.Kinds
import Language.PureScript.Types as P

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
--
--
zipMissing :: Environment -> [Binder] -> [Binder] -> [[Binder]]
zipMissing env bs bs' = map (uncurry $ missingCasesSingle env) $ zip bs bs'

-- |
-- Find the uncovered set between two binders:
-- the first binder is the case we are trying to cover
-- the second one is the matching binder
--
missingCasesSingle :: Environment -> Binder -> Binder -> [Binder]

-- any-wildcard
missingCasesSingle _ _ NullBinder = []

-- any-var
missingCasesSingle _ _ (VarBinder _) = []

-- cons-(wildcard|var)
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

-- undef
missingCasesSingle _ _ _ = undefined

-- |
-- Returns the uncovered set of binders
-- the first argument is the list of uncovered binders
-- the second argument is a clause of a pattern matching def
--
missingCasesMultiple :: [Binder] -> [Binder] -> [[Binder]]
missingCasesMultiple = undefined

-- |
-- Returns the uncovered set of case alternatives
--
missingCases :: CaseAlternative -> CaseAlternative -> [CaseAlternative]
missingCases = undefined

-- |
-- Main exhaustivity checker function
-- Returns the uncovered set of case alternatives.
-- 
checkExhaustive :: Environment -> [CaseAlternative] -> [CaseAlternative] -> [[CaseAlternative]]
checkExhaustive = undefined
