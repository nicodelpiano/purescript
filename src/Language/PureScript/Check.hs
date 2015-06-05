-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Check
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-----------------------------------------------------------------------------

module Language.PureScript.Check where

import Language.PureScript.AST.Binders

-- |
-- Find the uncovered set between two binders:
-- the first binder is the case we are trying to cover
-- the second one is the case we have in a clause of a pm def
--
missingCases :: Binder -> Binder -> [Binder]

-- any-wildcard
missingCases _ NullBinder = []

-- any-var
missingCases _ (VarBinder _) = []

-- undef
missingCases _ _ = undefined 
