{-# LANGUAGE ScopedTypeVariables
           , LambdaCase
           , FlexibleContexts 
           , TemplateHaskell 
           , DeriveDataTypeable 
           #-}
module Language.Haskell.Tools.AnnTrf.RangeToTemplate where

import Language.Haskell.Tools.AST

import Data.Data
import Data.List
import Control.Lens
import Data.StructuralTraversal
import Control.Monad.State
import SrcLoc
import Debug.Trace


data RangeTemplateElem = RangeElem RealSrcSpan
                       | RangeChildElem
                       | RangeOptionalElem
                       | RangeListElem
                       deriving Data

instance Show RangeTemplateElem where
  show (RangeElem sp) = show sp
  show RangeChildElem = "«.»"
  show RangeOptionalElem = "«?»"
  show RangeListElem = "«*»"
  
data RangeTemplate = RangeTemplate { _rangeTemplateSpan :: RealSrcSpan
                                   , _rangeTemplateElems :: [RangeTemplateElem] 
                                   } deriving Data
                                   
makeLenses ''RangeTemplate      

instance Show RangeTemplate where
  show (RangeTemplate rng rngs) = show rngs


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
                  return (ni & sourceInfo %~ cutOutElem below)

expandSourceInfo :: SpanInfo -> [SpanInfo] -> SpanInfo
expandSourceInfo ns@(NodeSpan _) _ = ns
expandSourceInfo (OptionalPos loc) sps = NodeSpan (RealSrcSpan $ collectSpanRanges loc sps)
expandSourceInfo (ListPos loc) sps = NodeSpan (RealSrcSpan $ collectSpanRanges loc sps)
                  
-- | Cuts out a list of source ranges from a given range
cutOutElem :: [SpanInfo] -> SpanInfo -> RangeTemplate
cutOutElem sps lp@(ListPos loc)
  = RangeTemplate (collectSpanRanges loc sps) [RangeListElem]
cutOutElem sps op@(OptionalPos loc) 
  = RangeTemplate (collectSpanRanges loc sps) [RangeOptionalElem]
cutOutElem sps (NodeSpan (RealSrcSpan sp))
  = RangeTemplate sp $ breakUpForEvery (breakUpForEvery [RangeElem sp] loc) span
  where (loc,span) = partition (\sp -> srcSpanStart sp == srcSpanEnd sp) (map spanRange sps)
        breakUpForEvery = foldl (\temp spIn -> (concatMap (\t -> breakUpRangeElem t spIn) temp))

collectSpanRanges :: SrcLoc -> [SpanInfo] -> RealSrcSpan
collectSpanRanges (RealSrcLoc loc) [] = realSrcLocSpan loc
collectSpanRanges _ [] = error "collectSpanRanges: No real src loc for empty element"
collectSpanRanges _ ls = case foldl1 combineSrcSpans $ map spanRange ls of RealSrcSpan sp -> sp
                             
-- | Breaks the given template element into possibly 2 or 3 parts by cutting out the given part
-- if it is inside the range of the template element.
breakUpRangeElem :: RangeTemplateElem -> SrcSpan -> [RangeTemplateElem]
breakUpRangeElem (RangeElem outer) (RealSrcSpan inner)
  | outer `containsSpan` inner 
  = (if (realSrcSpanStart outer) < (realSrcSpanStart inner) 
       then [ RangeElem (mkRealSrcSpan (realSrcSpanStart outer) (realSrcSpanStart inner)) ]
       else []) ++
    [ RangeChildElem ] ++
    (if (realSrcSpanEnd inner) < (realSrcSpanEnd outer) 
       then [ RangeElem (mkRealSrcSpan (realSrcSpanEnd inner) (realSrcSpanEnd outer)) ]
       else [])
breakUpRangeElem outer inner = [ outer ]


