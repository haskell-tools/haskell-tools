{-# LANGUAGE ScopedTypeVariables
           , LambdaCase
           , FlexibleContexts #-}
module Language.Haskell.Tools.AnnTrf.RangeToTemplate where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Instances

import Data.StructuralTraversal
import Control.Monad.State
import SrcLoc

data RangeTemplate = RangeTemplate SrcSpan [RangeTemplateElem]

rangeToTemplate :: SrcSpan -> RangeTemplate
rangeToTemplate span = RangeTemplate span [RangeElem span]

data RangeTemplateElem = RangeElem SrcSpan
                       | RangeChildElem Int

-- | Creates a source template from the ranges and the input file.
-- All source ranges must be good ranges.
cutUpRanges :: forall node . StructuralTraversable node => Ann node SrcSpan -> Ann node RangeTemplate
cutUpRanges n = evalState (cutUpRanges' n) [[]]
  where cutUpRanges' :: StructuralTraversable node => Ann node SrcSpan -> State [[SrcSpan]] (Ann node RangeTemplate)
        cutUpRanges' = structTraverse desc asc f
        
        -- keep the stack to contain all ranges under it
        desc = modify ([]:)
        asc  = modify (\case (x : [y] : xs) -> (y:x) : xs)
        
        -- combine each nodes range info with the top of the stack
        f ni = do top <- gets head
                  modify (\case ([] : xs) -> [ni] : xs)
                  return (cutOutElem ni top)

cutOutElem :: SrcSpan -> [SrcSpan] -> RangeTemplate
cutOutElem sp = RangeTemplate sp . snd . foldl (\(n, temp) spIn -> (n + 1, (concatMap (\t -> breakUpRangeElem n t spIn) temp))) (1, [RangeElem sp])
                  
breakUpRangeElem :: Int -> RangeTemplateElem -> SrcSpan -> [RangeTemplateElem]
breakUpRangeElem n (RangeElem (RealSrcSpan outer)) (RealSrcSpan inner) 
  | outer `containsSpan` inner 
  = [ RangeElem (RealSrcSpan $ mkRealSrcSpan (realSrcSpanStart outer) (realSrcSpanStart inner))
    , RangeChildElem n
    , RangeElem (RealSrcSpan $ mkRealSrcSpan (realSrcSpanEnd inner) (realSrcSpanEnd outer))
    ]
breakUpRangeElem _ outer _ = [ outer ]


