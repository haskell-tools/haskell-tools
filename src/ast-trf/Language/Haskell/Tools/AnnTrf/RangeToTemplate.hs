{-# LANGUAGE ScopedTypeVariables
           , LambdaCase
           , FlexibleContexts 
           , TemplateHaskell 
           , DeriveDataTypeable 
           #-}
module Language.Haskell.Tools.AnnTrf.RangeToTemplate where

import Language.Haskell.Tools.AST

import Data.Data
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
  show RangeChildElem = "×"
  
data RangeTemplate = RangeTemplate { _rangeTemplateSpan :: RealSrcSpan
                                   , _rangeTemplateElems :: [RangeTemplateElem] 
                                   } deriving Data
                                   
makeLenses ''RangeTemplate      

instance Show RangeTemplate where
  show (RangeTemplate rng rngs) = show rngs


-- | Creates a source template from the ranges and the input file.
-- All source ranges must be good ranges.
cutUpRanges :: forall node sema . StructuralTraversable node => Ann node (NodeInfo sema SrcSpan) 
                                                             -> Ann node (NodeInfo sema RangeTemplate)
cutUpRanges n = evalState (cutUpRanges' n) [[],[]]
  where cutUpRanges' :: StructuralTraversable node => Ann node (NodeInfo sema SrcSpan) 
                                                   -> State [[SrcSpan]] (Ann node (NodeInfo sema RangeTemplate))
        cutUpRanges' = traverseUp desc asc f
        
        -- keep the stack to contain the children elements on the place of the parent element
        desc = modify ([]:)
        asc  = modify tail
        
        -- combine the current node with its children, and add it to the list of current nodes
        f ni = do (below : top : xs) <- get
                  put ([] : (top ++ [ ni ^. sourceInfo ]) : xs)
                  return (ni & sourceInfo %~ flip cutOutElem below)

-- | Cuts out a list of source ranges from a given range
cutOutElem :: SrcSpan -> [SrcSpan] -> RangeTemplate
cutOutElem (RealSrcSpan sp) 
  = RangeTemplate sp . foldl (\temp spIn -> (concatMap (\t -> breakUpRangeElem t spIn) temp)) 
                             [RangeElem sp]

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
breakUpRangeElem outer _ = [ outer ]


