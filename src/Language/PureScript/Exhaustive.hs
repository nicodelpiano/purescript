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

module Language.PureScript.Exhaustive where

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Declarations

-- |
-- Find the uncovered set between two binders:
-- the first binder is the case we are trying to cover
-- the second one is the matching binder
--
missingCasesSingle :: Binder -> Binder -> [Binder]

-- any-wildcard
missingCasesSingle _ NullBinder = []

-- any-var
missingCasesSingle _ (VarBinder _) = []

-- undef
missingCasesSingle _ _ = undefined

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
checkExhaustive :: [CaseAlternative] -> [CaseAlternative] -> [[CaseAlternative]]
checkExhaustive = undefined
