{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | Helper functions for defining refactorings.
module Language.Haskell.Tools.Refactor.Utils.Helpers where

import Control.Monad.State ()
import Control.Reference
import Data.Function (on)
import Data.List (sortBy, nubBy, groupBy)
import Data.Maybe (Maybe(..))

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.Refactor.Utils.Lists (filterList)
import Language.Haskell.Tools.Rewrite as AST

import SrcLoc (srcSpanStart)

replaceWithJust :: Ann e IdDom SrcTemplateStage -> AnnMaybe e -> AnnMaybe e
replaceWithJust e = annMaybe .= Just e

replaceWithNothing :: AnnMaybe e -> AnnMaybe e
replaceWithNothing = annMaybe .= Nothing

-- | Remove the container (where or let) when the last binding is removed.
removeEmptyBnds :: Simple Traversal Module ValueBind
                     -> Simple Traversal Module Expr
                     -> AST.Module -> AST.Module
removeEmptyBnds binds exprs = (binds .- removeEmptyBindsAndGuards) . (exprs .- removeEmptyLetsAndStmts)
  where removeEmptyBindsAndGuards sb@(SimpleBind _ _ _)
          = (valBindLocals .- removeIfEmpty) . (valBindRhs .- removeEmptyGuards) $ sb
        removeEmptyBindsAndGuards fb@(FunctionBind _)
          = (funBindMatches & annList & matchBinds .- removeIfEmpty) . (funBindMatches & annList & matchRhs .- removeEmptyGuards) $ fb

        removeEmptyGuards rhs = rhsGuards & annList & guardStmts .- filterList (\case GuardLet (AnnList []) -> False; _ -> True) $ rhs

        removeIfEmpty mb@(AnnJust (LocalBinds (AnnList []))) = annMaybe .= Nothing $ mb
        removeIfEmpty mb = mb

        removeEmptyLetsAndStmts (Let (AnnList []) e) = e
        removeEmptyLetsAndStmts e = exprStmts .- removeEmptyStmts $ e

        removeEmptyStmts ls = (annList & cmdStmtBinds .- removeEmptyStmts)
                                . filterList (\case LetStmt (AnnList []) -> False; _ -> True) $ ls

-- | Puts the elements in the orginal order and remove duplicates (elements with the same source range)
normalizeElements :: [Ann e dom SrcTemplateStage] -> [Ann e dom SrcTemplateStage]
normalizeElements elems = nubBy ((==) `on` getRange) $ sortBy (compare `on` srcSpanStart . getRange) elems

-- | Groups elements together into equivalence groups.
groupElemsBy :: Ord k => (a -> k) -> [a] -> [[a]]
groupElemsBy f = map (map snd)
               . groupBy ((==) `on` fst)
               . sortBy (compare `on` fst)
               . map ((,) <$> f <*> id)

-- | Chooses a representative element for each equivalence group,
-- and pairs them with their corresponding group.
reprElems :: [[a]] -> [(a,[a])]
reprElems = map ((,) <$> head <*> id)

-- | Sorts the elements of a list into equivalence groups based on a function,
-- then chooses a representative element for each group,
-- and pairs them with their corresponding group.
equivalenceGroupsBy :: Ord k => (a -> k) -> [a] -> [(a,[a])]
equivalenceGroupsBy f = reprElems . groupElemsBy f
