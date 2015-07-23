-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Linter.Utils
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- | Module for useful tools for exhaustivity and redundancy checking.
-- 
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Linter.Utils where 

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (foldl', sortBy, nub)
import Data.Function (on)

import Language.PureScript.Environment
import Language.PureScript.Names as P
import Language.PureScript.AST.Binders
import Language.PureScript.AST.Declarations
import Language.PureScript.Kinds
import Language.PureScript.Types as P

-- |
-- Qualifies a propername from a given qualified propername and a default module name
--
qualifyName :: a -> ModuleName -> Qualified a -> Qualified a
qualifyName n defmn qn = Qualified (Just mn) n
  where
  (mn, _) = qualify defmn qn

-- |
-- Given an environment and a datatype or newtype name,
-- this function returns the associated data constructors if it is the case of a datatype
-- where: - ProperName is the name of the constructor (for example, "Nothing" in Maybe)
--        - [Type] is the list of arguments, if it has (for example, "Just" has [TypeVar "a"])
--
getConstructors :: Environment -> ModuleName -> (Qualified ProperName) -> [(ProperName, [Type])]
getConstructors env defmn n = extractConstructors lnte
  where
  qpn :: Qualified ProperName
  qpn = getConsDataName n

  getConsDataName :: (Qualified ProperName) -> (Qualified ProperName)
  getConsDataName con = qualifyName nm defmn con
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

  extractConstructors :: Maybe (Kind, TypeKind) -> [(ProperName, [Type])]
  extractConstructors (Just (_, DataType _ pt)) = pt
  extractConstructors _ = error "Data name not in the scope of the current environment in extractConstructors"

-- |
-- Replicates a wildcard binder
--
initialize :: Int -> [Binder]
initialize l = replicate l NullBinder

-- |
-- Applies a function over two lists of tuples that may lack elements
--
genericMerge :: Ord a =>
  (a -> Maybe b -> Maybe c -> d) ->
  [(a, b)] ->
  [(a, c)] ->
  [d]
genericMerge _ [] [] = []
genericMerge f bs [] = map (\(s, b) -> f s (Just b) Nothing) bs
genericMerge f [] bs = map (\(s, b) -> f s Nothing (Just b)) bs
genericMerge f bsl@((s, b):bs) bsr@((s', b'):bs')
  | s < s' = (f s (Just b) Nothing) : genericMerge f bs bsr
  | s > s' = (f s' Nothing (Just b')) : genericMerge f bsl bs'
  | otherwise = (f s (Just b) (Just b')) : genericMerge f bs bs'
