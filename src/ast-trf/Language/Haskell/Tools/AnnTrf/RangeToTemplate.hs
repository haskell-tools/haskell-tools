{-# LANGUAGE ScopedTypeVariables
           , LambdaCase
           , FlexibleContexts #-}
module Language.Haskell.Tools.AnnTrf.RangeToTemplate where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Instances

import Data.StructuralTraversal
import Control.Monad.State
import SrcLoc
import Debug.Trace

-- rangeToTemplate :: forall node . StructuralTraversable node => String -> Ann node SrcSpan -> Ann node SourceTemplate
-- rangeToTemplate =  . cutUpRanges

data RangeTemplate = RangeTemplate SrcSpan [RangeTemplateElem]

instance Show RangeTemplate where
  show (RangeTemplate rng rngs) = show rngs

sourceRangeToTemplate :: SrcSpan -> RangeTemplate
sourceRangeToTemplate span = RangeTemplate span [RangeElem span]

data RangeTemplateElem = RangeElem SrcSpan
                       | RangeChildElem Int
                       

instance Show RangeTemplateElem where
  show (RangeElem sp) = show sp
  show (RangeChildElem i) = "<" ++ show i ++ ">"
              

-- | Creates a source template from the ranges and the input file.
-- All source ranges must be good ranges.
cutUpRanges :: forall node . StructuralTraversable node => Ann node SrcSpan -> Ann node RangeTemplate
cutUpRanges n = evalState (cutUpRanges' n) [[],[]]
  where cutUpRanges' :: StructuralTraversable node => Ann node SrcSpan -> State [[SrcSpan]] (Ann node RangeTemplate)
        cutUpRanges' = structTraverse desc asc f
        
        -- keep the stack to contain the children elements on the place of the parent element
        desc = modify ([]:)
        asc  = modify tail
        
        -- combine the current node with its children, and add it to the list of current nodes
        f ni = do (below : top : xs) <- get
                  put ([] : (top++[ni]) : xs)
                  return (cutOutElem ni below)

-- | Cuts out a list of source ranges from a given range
cutOutElem :: SrcSpan -> [SrcSpan] -> RangeTemplate
cutOutElem sp = RangeTemplate sp . snd . foldl (\(n, temp) spIn -> (n + 1, (concatMap (\t -> breakUpRangeElem n t spIn) temp))) (0, [RangeElem sp])

-- | Breaks the given template element into possibly 2 or 3 parts by cutting out the given part
-- if it is inside the range of the template element.
breakUpRangeElem :: Int -> RangeTemplateElem -> SrcSpan -> [RangeTemplateElem]
breakUpRangeElem n (RangeElem (RealSrcSpan outer)) (RealSrcSpan inner) 
  | outer `containsSpan` inner 
  = (if (realSrcSpanStart outer) < (realSrcSpanStart inner) 
       then [ RangeElem (RealSrcSpan $ mkRealSrcSpan (realSrcSpanStart outer) (realSrcSpanStart inner)) ]
       else []) ++
    [ RangeChildElem n ] ++
    (if (realSrcSpanEnd inner) < (realSrcSpanEnd outer) 
       then [ RangeElem (RealSrcSpan $ mkRealSrcSpan (realSrcSpanEnd inner) (realSrcSpanEnd outer)) ]
       else [])
breakUpRangeElem _ outer _ = [ outer ]


