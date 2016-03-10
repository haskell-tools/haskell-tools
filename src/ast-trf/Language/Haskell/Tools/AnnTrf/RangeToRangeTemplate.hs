{-# LANGUAGE ScopedTypeVariables
           , LambdaCase
           , FlexibleContexts 
           , TemplateHaskell 
           , DeriveDataTypeable 
           #-}
module Language.Haskell.Tools.AnnTrf.RangeToRangeTemplate where

import Language.Haskell.Tools.AST

import Data.Data
import Data.List
import Data.Maybe
import Control.Reference hiding (element)
import Data.StructuralTraversal
import Control.Monad.State
import SrcLoc
import Language.Haskell.Tools.AnnTrf.RangeTemplate

-- | Creates a source template from the ranges and the input file.
-- All source ranges must be good ranges.
cutUpRanges :: forall node sema . StructuralTraversable node 
                 => Ann node (NodeInfo sema SpanInfo) 
                 -> Ann node (NodeInfo sema RangeTemplate)
cutUpRanges n = evalState (cutUpRanges' n) [[],[]]
  where cutUpRanges' :: StructuralTraversable node => Ann node (NodeInfo sema SpanInfo) 
                                                   -> State [[SpanInfo]] (Ann node (NodeInfo sema RangeTemplate))
        cutUpRanges' = traverseUp desc asc f
        
        -- keep the stack to contain the children elements on the place of the parent element
        desc = modify ([]:)
        asc  = modify tail
        
        -- combine the current node with its children, and add it to the list of current nodes
        f ni = do (below : top : xs) <- get
                  put ([] : (top ++ [ expandSourceInfo (ni ^. sourceInfo) below ]) : xs)
                  return (sourceInfo .- cutOutElem below $ ni)

expandSourceInfo :: SpanInfo -> [SpanInfo] -> SpanInfo
expandSourceInfo ns@(NodeSpan _) _ = ns
expandSourceInfo (OptionalPos loc) sps = NodeSpan (RealSrcSpan $ collectSpanRanges loc sps)
expandSourceInfo (ListPos loc) sps = NodeSpan (RealSrcSpan $ collectSpanRanges loc sps)
                  
-- | Cuts out a list of source ranges from a given range
cutOutElem :: [SpanInfo] -> SpanInfo -> RangeTemplate
cutOutElem sps lp@(ListPos loc)
  = let wholeRange = collectSpanRanges loc sps 
     in RangeTemplate wholeRange [RangeListElem (getSeparators wholeRange sps)]
cutOutElem sps op@(OptionalPos loc) 
  = RangeTemplate (collectSpanRanges loc sps) [RangeOptionalElem]
cutOutElem sps (NodeSpan (RealSrcSpan sp))
  = RangeTemplate sp $ foldl breakFirstHit (foldl breakFirstHit [RangeElem sp] loc) span
  where (loc,span) = partition (\sp -> srcSpanStart sp == srcSpanEnd sp) (map spanRange sps)
        breakFirstHit (elem:rest) sp 
          = case breakUpRangeElem elem sp of
             -- only continue if the correct place for the child range is not found
              Just pieces -> pieces ++ rest
              Nothing -> elem : breakFirstHit rest sp
        breakFirstHit [] sp = error ("breakFirstHit: didn't find correct place for " ++ show sp)

collectSpanRanges :: SrcLoc -> [SpanInfo] -> RealSrcSpan
collectSpanRanges (RealSrcLoc loc) [] = realSrcLocSpan loc
collectSpanRanges _ [] = error "collectSpanRanges: No real src loc for empty element"
collectSpanRanges _ ls = case foldl1 combineSrcSpans $ map spanRange ls of RealSrcSpan sp -> sp
                     
getSeparators :: RealSrcSpan -> [SpanInfo] -> [RealSrcSpan]
getSeparators sp infos 
  = catMaybes $ map getRangeElemSpan (cutOutElem infos (NodeSpan (RealSrcSpan sp)) ^. rangeTemplateElems)
                     
-- | Breaks the given template element into possibly 2 or 3 parts by cutting out the given part
-- if it is inside the range of the template element. Returns Nothing if the second argument is not inside.
breakUpRangeElem :: RangeTemplateElem -> SrcSpan -> Maybe [RangeTemplateElem]
breakUpRangeElem (RangeElem outer) (RealSrcSpan inner)
  | outer `containsSpan` inner 
  = Just $ (if (realSrcSpanStart outer) < (realSrcSpanStart inner) 
              then [ RangeElem (mkRealSrcSpan (realSrcSpanStart outer) (realSrcSpanStart inner)) ]
              else []) ++
           [ RangeChildElem ] ++
           (if (realSrcSpanEnd inner) < (realSrcSpanEnd outer) 
              then [ RangeElem (mkRealSrcSpan (realSrcSpanEnd inner) (realSrcSpanEnd outer)) ]
              else [])
breakUpRangeElem outer inner = Nothing


