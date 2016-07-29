{-# LANGUAGE ScopedTypeVariables
           , LambdaCase
           , FlexibleContexts
           #-}
-- | Transform a syntax tree with ranges to a syntax tree that has range templates. Cuts the ranges of children
-- from the ranges of their parents and replaces it with placeholders.
module Language.Haskell.Tools.AnnTrf.RangeToRangeTemplate (cutUpRanges, fixRanges) where

import Language.Haskell.Tools.AST

import Data.Data
import Data.List
import Data.Maybe
import Control.Reference hiding (element)
import Control.Monad.State
import SrcLoc
import Language.Haskell.Tools.AnnTrf.RangeTemplate

import Debug.Trace

-- | Creates a source template from the ranges and the input file.
-- All source ranges must be good ranges.
cutUpRanges :: forall node dom . SourceInfoTraversal node 
                 => Ann node dom RangeStage
                 -> Ann node dom RngTemplateStage
cutUpRanges n = evalState (cutUpRanges' n) [[],[]]
  where cutUpRanges' :: SourceInfoTraversal node => Ann node dom RangeStage
                                                 -> State [[SrcSpan]] (Ann node dom RngTemplateStage)
        cutUpRanges' = sourceInfoTraverseUp (SourceInfoTrf (trf cutOutElemSpan) (trf cutOutElemList) (trf cutOutElemOpt)) desc asc
        
        -- keep the stack to contain the children elements on the place of the parent element
        desc = modify ([]:)
        asc  = modify tail
        
        -- combine the current node with its children, and add it to the list of current nodes
        trf :: (HasRange (x RangeStage), HasRange (x RngTemplateStage)) => ([SrcSpan] -> x RangeStage -> x RngTemplateStage) -> x RangeStage -> State [[SrcSpan]] (x RngTemplateStage)
        trf f ni = do (below : top : xs) <- get
                      let res = f below ni
                      put ([] : (top ++ [ getRange res ]) : xs)
                      return res

-- | Modifies ranges to contain their children
fixRanges :: SourceInfoTraversal node 
          => Ann node dom RangeStage 
          -> Ann node dom RangeStage
fixRanges node = evalState (sourceInfoTraverseUp (SourceInfoTrf (trf expandToContain) (trf (\_ -> id)) (trf (\_ -> id))) desc asc node) [[],[]]
  where -- keep the stack to contain the children elements on the place of the parent element
        desc = modify ([]:)
        asc  = modify tail
        
        trf :: (HasRange (x RangeStage), HasRange (x RngTemplateStage)) => ([SrcSpan] -> x RangeStage -> x RangeStage) -> x RangeStage -> State [[SrcSpan]] (x RangeStage)
        trf f ni = do (below : top : xs) <- get
                      let res = f below ni
                      --trace ("\n### ni: " ++ show ni ++ "\nbelow:" ++ show below ++ "\ntotop:" ++ show (foldl1 combineSrcSpans (getRange ni : below)) ++ "\ncreated: " ++ show ((f below ni)))
                      put ([] : (top ++ [ getRange res ]) : xs)
                      return res

-- | Expand a simple node to contain its children
expandToContain :: [SrcSpan] -> SpanInfo RangeStage -> SpanInfo RangeStage
expandToContain cont (NodeSpan sp) = NodeSpan (foldl1 combineSrcSpans $ sp : cont)
   
-- | Cuts out a list of source ranges from a given range
cutOutElemSpan :: [SrcSpan] -> SpanInfo RangeStage -> SpanInfo RngTemplateStage
cutOutElemSpan sps (NodeSpan (RealSrcSpan sp))
  = RangeTemplateNode sp $ foldl breakFirstHit (foldl breakFirstHit [RangeElem sp] loc) span
  where (loc,span) = partition (\sp -> srcSpanStart sp == srcSpanEnd sp) sps
        breakFirstHit (elem:rest) sp 
          = case breakUpRangeElem elem sp of
             -- only continue if the correct place for the child range is not found
              Just pieces -> pieces ++ rest
              Nothing -> elem : breakFirstHit rest sp
        breakFirstHit [] sp = error ("breakFirstHit: didn't find correct place for " ++ show sp)

cutOutElemList :: [SrcSpan] -> ListInfo RangeStage -> ListInfo RngTemplateStage
cutOutElemList sps lp@(ListPos bef aft sep indented loc)
  = let wholeRange = collectSpanRanges loc sps 
     in RangeTemplateList wholeRange bef aft sep indented (getSeparators wholeRange sps)

cutOutElemOpt :: [SrcSpan] -> OptionalInfo RangeStage -> OptionalInfo RngTemplateStage
cutOutElemOpt sps op@(OptionalPos bef aft loc) 
  = RangeTemplateOpt (collectSpanRanges loc sps) bef aft

collectSpanRanges :: SrcLoc -> [SrcSpan] -> RealSrcSpan
collectSpanRanges (RealSrcLoc loc) [] = realSrcLocSpan loc
collectSpanRanges _ [] = error "collectSpanRanges: No real src loc for empty element"
collectSpanRanges _ ls = case foldl1 combineSrcSpans ls of RealSrcSpan sp -> sp
                     
-- | Cuts out all elements from a list, the rest is the list of separators
getSeparators :: RealSrcSpan -> [SrcSpan] -> [RealSrcSpan]
getSeparators sp infos@(_:_:_)
  = mapMaybe getRangeElemSpan (cutOutElemSpan infos (NodeSpan (RealSrcSpan sp)) ^. rngTemplateNodeElems)
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


