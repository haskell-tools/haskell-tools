{-# LANGUAGE ScopedTypeVariables #-}

-- | Transform a syntax tree with ranges to a syntax tree that has range templates. Cuts the ranges of children
-- from the ranges of their parents and replaces it with placeholders.
module Language.Haskell.Tools.PrettyPrint.Prepare.RangeToRangeTemplate (cutUpRanges, fixRanges, BreakUpProblem(..)) where

import Language.Haskell.Tools.AST

import Control.Exception (Exception, throw)
import Control.Monad.State
import Control.Reference ((^.))
import Data.List
import Data.Maybe (Maybe(..), mapMaybe)

import FastString as GHC (unpackFS)
import SrcLoc

import Language.Haskell.Tools.PrettyPrint.Prepare.RangeTemplate

-- | Creates a source template from the ranges and the input file.
-- All source ranges must be good ranges.
cutUpRanges :: forall node dom . SourceInfoTraversal node
                 => Ann node dom NormRangeStage
                 -> Ann node dom RngTemplateStage
cutUpRanges n = evalState (cutUpRanges' n) [[],[]]
  where cutUpRanges' :: Ann node dom NormRangeStage -> State [[SrcSpan]] (Ann node dom RngTemplateStage)
        cutUpRanges' = sourceInfoTraverseUp (SourceInfoTrf (trf cutOutElemSpan) (trf cutOutElemList) (trf cutOutElemOpt)) desc asc

        -- keep the stack to contain the children elements on the place of the parent element
        desc = modify ([]:)
        asc  = modify tail

        -- combine the current node with its children, and add it to the list of current nodes
        trf :: HasRange (x RngTemplateStage)
            => ([SrcSpan] -> x NormRangeStage -> x RngTemplateStage) -> x NormRangeStage -> State [[SrcSpan]] (x RngTemplateStage)
        trf f ni = do stack <- get
                      case stack of 
                        (below : top : xs) -> do
                          let res = f below ni
                          put ([] : (top ++ [ getRange res ]) : xs)
                          return res
                        _ -> trfProblem "RangeToRangeTemplate.cutUpRanges.trf: stack is not right"

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
        breakFirstHit [] inner = throw $ BreakUpProblem sp inner sps
cutOutElemSpan _ (NormNodeInfo (UnhelpfulSpan {}))
  = trfProblem "cutOutElemSpan: no real span"

data BreakUpProblem = BreakUpProblem { bupOuter :: RealSrcSpan
                                     , bupInner :: SrcSpan
                                     , bupSiblings :: [SrcSpan]
                                     }

instance Show BreakUpProblem where
 show (BreakUpProblem _ (RealSrcSpan inner) _)
   = unpackFS (srcSpanFile inner) ++ ": didn't find correct place for AST element at " ++ shortShowSpan (RealSrcSpan inner)
 show (BreakUpProblem outer _ _)
   = unpackFS (srcSpanFile outer) ++ ": didn't find correct place for AST element in " ++ shortShowSpan (RealSrcSpan outer)

instance Exception BreakUpProblem

cutOutElemList :: [SrcSpan] -> ListInfo NormRangeStage -> ListInfo RngTemplateStage
cutOutElemList sps (NormListInfo bef aft sep indented sp)
  = let RealSrcSpan wholeRange = foldl1 combineSrcSpans $ sp : sps
     in RangeTemplateList wholeRange bef aft sep indented (getSeparators wholeRange sps)

-- | Cuts out all elements from a list, the rest is the list of separators
getSeparators :: RealSrcSpan -> [SrcSpan] -> [RealSrcSpan]
getSeparators sp infos@(_:_:_)
  = mapMaybe getRangeElemSpan (cutOutElemSpan infos (NormNodeInfo (RealSrcSpan sp)) ^. rngTemplateNodeElems)
-- at least two elements needed or there can be no separators
getSeparators _ _ = []

cutOutElemOpt :: [SrcSpan] -> OptionalInfo NormRangeStage -> OptionalInfo RngTemplateStage
cutOutElemOpt sps (NormOptInfo bef aft sp)
  = let RealSrcSpan wholeRange = foldl1 combineSrcSpans $ sp : sps
     in RangeTemplateOpt wholeRange bef aft

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
breakUpRangeElem _ _ = Nothing


-- | Modifies ranges to contain their children
fixRanges :: SourceInfoTraversal node
          => Ann node dom RangeStage
          -> Ann node dom NormRangeStage
fixRanges node = evalState (sourceInfoTraverseUp (SourceInfoTrf (trf expandToContain) (trf expandListToContain) (trf expandOptToContain)) desc asc node) [[],[]]
  where -- keep the stack to contain the children elements on the place of the parent element
        desc = modify ([]:)
        asc  = modify tail

        trf :: HasRange (x NormRangeStage)
            => ([SrcSpan] -> x RangeStage -> x NormRangeStage) -> x RangeStage -> State [[SrcSpan]] (x NormRangeStage)
        trf f ni = do stack <- get
                      case stack of 
                        (below : top : xs) -> do
                          let res = f below ni
                              resRange = getRange res
                              endOfSiblings = srcSpanEnd (collectSpanRanges (srcSpanStart resRange) top)
                              correctedRange = if endOfSiblings > srcSpanStart resRange
                                                 then mkSrcSpan endOfSiblings (max endOfSiblings (srcSpanEnd resRange))
                                                 else resRange
                          put ([] : (top ++ [ correctedRange ]) : xs)
                          return $ setRange correctedRange res
                        _ -> trfProblem "RangeToRangeTemplate.fixRanges.trf: stack is not right"

-- | Expand a simple node to contain its children
expandToContain :: [SrcSpan] -> SpanInfo RangeStage -> SpanInfo NormRangeStage
expandToContain cont (NodeSpan sp)
  = NormNodeInfo (checkSpans cont $ foldl1 combineSrcSpans $ sp : cont)

expandListToContain :: [SrcSpan] -> ListInfo RangeStage -> ListInfo NormRangeStage
expandListToContain cont (ListPos bef aft def ind sp)
  = NormListInfo bef aft def ind (checkSpans cont $ collectSpanRanges sp cont)

expandOptToContain :: [SrcSpan] -> OptionalInfo RangeStage -> OptionalInfo NormRangeStage
expandOptToContain cont (OptionalPos bef aft sp)
  = NormOptInfo bef aft (checkSpans cont $ collectSpanRanges sp cont)

collectSpanRanges :: SrcLoc -> [SrcSpan] -> SrcSpan
collectSpanRanges loc@(RealSrcLoc _) [] = srcLocSpan loc
collectSpanRanges _ ls = foldl combineSrcSpans noSrcSpan ls

-- | Checks the contained source ranges to detect the convertion problems where we can see their location.
checkSpans :: [SrcSpan] -> SrcSpan -> SrcSpan
checkSpans spans res
  = if any (not . isGoodSrcSpan) spans && isGoodSrcSpan res
      then trfProblem $ "Wrong src spans in " ++ show res
      else res
