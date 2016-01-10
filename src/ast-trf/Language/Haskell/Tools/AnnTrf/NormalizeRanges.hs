{-# LANGUAGE FlexibleContexts
           , ScopedTypeVariables
           , LambdaCase #-}
module Language.Haskell.Tools.AnnTrf.NormalizeRanges where

import Control.Monad.State
import Data.StructuralTraversal
import SrcLoc
import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Instances

-- | Expands nodes to contain all their children
normalizeRanges :: forall node . StructuralTraversable node => Ann node SrcSpan -> Ann node SrcSpan
normalizeRanges n = evalState (normalizeRanges' n) [noSrcSpan]
  where normalizeRanges' :: StructuralTraversable node => Ann node SrcSpan -> State [SrcSpan] (Ann node SrcSpan)
        normalizeRanges' = structTraverse desc asc f
        
        -- keep the stack to contain all ranges under it
        desc = modify (noSrcSpan:)
        asc  = modify (\case (x : y : xs) -> combineSrcSpans x y : xs)
        
        -- combine each nodes range info with the top of the stack
        f ni = do top <- gets head
                  let newInfo = combineSrcSpans ni top
                  modify (\case (_ : xs) -> newInfo : xs)
                  return newInfo
