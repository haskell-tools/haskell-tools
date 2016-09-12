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

import FastString as GHC

import Debug.Trace

-- | Creates a source template from the ranges and the input file.
-- All source ranges must be good ranges.
cutUpRanges :: forall node dom . SourceInfoTraversal node 
                 => Ann node dom NormRangeStage
                 -> Ann node dom RngTemplateStage
cutUpRanges n = evalState (cutUpRanges' n) [[],[]]
  where cutUpRanges' :: SourceInfoTraversal node => Ann node dom NormRangeStage
                                                 -> State [[SrcSpan]] (Ann node dom RngTemplateStage)
        cutUpRanges' = sourceInfoTraverseUp (SourceInfoTrf (trf cutOutElemSpan) (trf cutOutElemList) (trf cutOutElemOpt)) desc asc
        
        -- keep the stack to contain the children elements on the place of the parent element
        desc = modify ([]:)
        asc  = modify tail
        
        -- combine the current node with its children, and add it to the list of current nodes
        trf :: (Show (x NormRangeStage), Show (x RngTemplateStage), HasRange (x NormRangeStage), HasRange (x RngTemplateStage)) 
            => ([SrcSpan] -> x NormRangeStage -> x RngTemplateStage) -> x NormRangeStage -> State [[SrcSpan]] (x RngTemplateStage)
        trf f ni = do (below : top : xs) <- get
                      let res = f below ni
                      put ([] : (top ++ [ getRange res ]) : xs)
                      return res

-- | Modifies ranges to contain their children
fixRanges :: SourceInfoTraversal node 
          => Ann node dom RangeStage 
          -> Ann node dom NormRangeStage
fixRanges node = evalState (sourceInfoTraverseUp (SourceInfoTrf (trf expandToContain) (trf expandListToContain) (trf expandOptToContain)) desc asc node) [[],[]]
  where -- keep the stack to contain the children elements on the place of the parent element
        desc = modify ([]:)
        asc  = modify tail
        
        trf :: (HasRange (x RangeStage), HasRange (x NormRangeStage)) 
            => ([SrcSpan] -> x RangeStage -> x NormRangeStage) -> x RangeStage -> State [[SrcSpan]] (x NormRangeStage)
        trf f ni = do (below : top : xs) <- get
                      let res = f below ni
                          resRange = getRange res
                          endOfSiblings = srcSpanEnd (RealSrcSpan $ collectSpanRanges (srcSpanStart resRange) top)
                          correctedRange = if endOfSiblings > srcSpanStart resRange 
                                             then mkSrcSpan endOfSiblings (max endOfSiblings (srcSpanEnd resRange)) 
                                             else resRange
                      put ([] : (top ++ [ correctedRange ]) : xs)
                      return $ setRange correctedRange res

-- | Expand a simple node to contain its children
expandToContain :: [SrcSpan] -> SpanInfo RangeStage -> SpanInfo NormRangeStage
expandToContain cont (NodeSpan sp) = NormNodeInfo (foldl1 combineSrcSpans $ sp : cont)

expandListToContain :: [SrcSpan] -> ListInfo RangeStage -> ListInfo NormRangeStage
expandListToContain cont (ListPos bef aft def ind sp) = NormListInfo bef aft def ind (RealSrcSpan $ collectSpanRanges sp cont)

expandOptToContain :: [SrcSpan] -> OptionalInfo RangeStage -> OptionalInfo NormRangeStage
expandOptToContain cont (OptionalPos bef aft sp) = NormOptInfo bef aft (RealSrcSpan $ collectSpanRanges sp cont)
   
-- | Cuts out a list of source ranges from a given range
cutOutElemSpan :: [SrcSpan] -> SpanInfo NormRangeStage -> SpanInfo RngTemplateStage
cutOutElemSpan sps (NormNodeInfo (RealSrcSpan sp))
  = RangeTemplateNode sp $ foldl breakFirstHit (foldl breakFirstHit [RangeElem sp] loc) span
  where (loc,span) = partition (\sp -> srcSpanStart sp == srcSpanEnd sp) sps
        breakFirstHit (elem:rest) sp 
          = case breakUpRangeElem elem sp of
             -- only continue if the correct place for the child range is not found
              Just pieces -> pieces ++ rest
              Nothing -> elem : breakFirstHit rest sp
        breakFirstHit [] sp = error ("breakFirstHit: " ++ maybe "" unpackFS (srcSpanFileName_maybe sp) ++ " didn't find correct place for " ++ shortShowSpan sp ++ " in " ++ shortShowSpan sp ++ " with [" ++ concat (intersperse "," (map shortShowSpan sps)) ++ "]")

cutOutElemList :: [SrcSpan] -> ListInfo NormRangeStage -> ListInfo RngTemplateStage
cutOutElemList sps lp@(NormListInfo bef aft sep indented sp)
  = let RealSrcSpan wholeRange = foldl1 combineSrcSpans $ sp : sps
     in RangeTemplateList wholeRange bef aft sep indented (getSeparators wholeRange sps)

cutOutElemOpt :: [SrcSpan] -> OptionalInfo NormRangeStage -> OptionalInfo RngTemplateStage
cutOutElemOpt sps op@(NormOptInfo bef aft sp) 
  = let RealSrcSpan wholeRange = foldl1 combineSrcSpans $ sp : sps
     in RangeTemplateOpt wholeRange bef aft

collectSpanRanges :: SrcLoc -> [SrcSpan] -> RealSrcSpan
collectSpanRanges (RealSrcLoc loc) [] = realSrcLocSpan loc
collectSpanRanges _ [] = error "collectSpanRanges: No real src loc for empty element"
collectSpanRanges _ ls = case foldl1 combineSrcSpans ls of RealSrcSpan sp -> sp
                     
-- | Cuts out all elements from a list, the rest is the list of separators
getSeparators :: RealSrcSpan -> [SrcSpan] -> [RealSrcSpan]
getSeparators sp infos@(_:_:_)
  = mapMaybe getRangeElemSpan (cutOutElemSpan infos (NormNodeInfo (RealSrcSpan sp)) ^. rngTemplateNodeElems)
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


