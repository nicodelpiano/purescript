-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Linter.Bindable
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- | Module that describes the Bindable class
-- 
-----------------------------------------------------------------------------

module Language.PureScript.Linter.Bindable where

import Language.PureScript.AST.Binders

class Bindable expr where
  -- bind
  bind :: Binder -> Binder -> expr

  -- should this be generalized for mor general seqs?
  multipleBind :: [Binder] -> [Binder] -> [expr]
