{-# LANGUAGE ScopedTypeVariables
           , LambdaCase
           , FlexibleContexts 
           , TemplateHaskell 
           , DeriveDataTypeable 
           #-}
module Language.Haskell.Tools.AnnTrf.RangeToRangeTemplate (cutUpRanges, fixRanges) where

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
                  put ([] : (top ++ [ expandAsNodeInfo (ni ^. sourceInfo) below ]) : xs)
                  return (sourceInfo .- cutOutElem below $ ni)

-- | Modifies ranges to contain their children
fixRanges :: StructuralTraversable node 
          => Ann node (NodeInfo sema SpanInfo) 
          -> Ann node (NodeInfo sema SpanInfo)
fixRanges node = evalState (traverseUp desc asc f node) [[],[]]
  where -- keep the stack to contain the children elements on the place of the parent element
        desc = modify ([]:)
        asc  = modify tail
        
        f ni = do (below : top : xs) <- get
                  put ([] : (top ++ [ expandAsNodeToContain (ni ^. sourceInfo) below ]) : xs)
                  return (sourceInfo .- expandToContain below $ ni)
                  
expandAsNodeInfo :: SpanInfo -> [SpanInfo] -> SpanInfo
expandAsNodeInfo ns@(NodeSpan _) _ = ns
expandAsNodeInfo (OptionalPos {_optionalPos = loc}) sps = NodeSpan (RealSrcSpan $ collectSpanRanges loc sps)
expandAsNodeInfo (ListPos {_listPos = loc}) sps = NodeSpan (RealSrcSpan $ collectSpanRanges loc sps)

expandToContain :: [SpanInfo] -> SpanInfo -> SpanInfo
expandToContain cont (NodeSpan sp) = NodeSpan (foldl1 combineSrcSpans $ sp : map spanRange cont)
expandToContain _ oth = oth

expandAsNodeToContain :: SpanInfo -> [SpanInfo] -> SpanInfo
expandAsNodeToContain ns@(NodeSpan _) ls = expandToContain ls ns
expandAsNodeToContain oth ls = expandAsNodeInfo oth ls
                  
-- | Cuts out a list of source ranges from a given range
cutOutElem :: [SpanInfo] -> SpanInfo -> RangeTemplate
cutOutElem sps lp@(ListPos bef aft sep indented loc)
  = let wholeRange = collectSpanRanges loc sps 
     in RangeTemplate wholeRange [RangeListElem bef aft sep indented (getSeparators wholeRange sps)]
cutOutElem sps op@(OptionalPos bef aft loc) 
  = RangeTemplate (collectSpanRanges loc sps) [RangeOptionalElem bef aft]
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
                     
-- | Cuts out all elements from a list, the rest is the list of separators
getSeparators :: RealSrcSpan -> [SpanInfo] -> [RealSrcSpan]
getSeparators sp infos@(_:_:_)
  = catMaybes $ map getRangeElemSpan (cutOutElem infos (NodeSpan (RealSrcSpan sp)) ^. rangeTemplateElems)
-- at least two elements needed or there can be no separators
getSeparators sp _ = []
                     
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


