{-# LANGUAGE LambdaCase 
           , FlexibleContexts
           #-}
module Language.Haskell.Tools.AnnTrf.RangeTemplateToSourceTemplate where

import SrcLoc
import StringBuffer
import Data.StructuralTraversal
import Data.Map
import Data.Monoid
import Control.Reference
import Control.Monad.State
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.RangeToRangeTemplate
import Language.Haskell.Tools.AnnTrf.RangeTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplate

import Debug.Trace

rangeToSource :: StructuralTraversable node => StringBuffer -> Ann node (NodeInfo sema RangeTemplate) 
                                                            -> Ann node (NodeInfo sema SourceTemplate)
rangeToSource srcInput tree = let locIndices = getLocIndices tree
                                  srcMap = mapLocIndices srcInput locIndices
                               in applyFragments (elems srcMap) tree

-- maps could be strict

-- | Assigns an index (in the order they are used) for each range
getLocIndices :: StructuralTraversable e => Ann e (NodeInfo sema RangeTemplate) -> Map OrdSrcSpan Int
getLocIndices = snd . flip execState (0, empty) .
  traverseDown (return ()) (return ()) 
               (mapM_ (\case RangeElem sp               -> modify (insertElem sp)
                             RangeListElem _ _ _ _ seps -> mapM_ (modify . insertElem) seps
                             _                          -> return ()
                      ) . (^. sourceInfo&rangeTemplateElems))
  where insertElem sp (i,m) = (i+1, insert (OrdSrcSpan sp) i m)
                             
                             
-- | Partitions the source file in the order where the parts are used in the AST
mapLocIndices :: Ord k => StringBuffer -> Map OrdSrcSpan k -> Map k String
mapLocIndices inp = fst . foldlWithKey (\(new, str) sp k -> let (rem, val) = takeSpan str sp
                                                             in (insert k (reverse val) new, rem)) (empty, inp)
  where takeSpan :: StringBuffer -> OrdSrcSpan -> (StringBuffer, String)
        takeSpan str (OrdSrcSpan sp) = takeSpan' (realSrcSpanStart sp) (realSrcSpanEnd sp) (str,"")

        takeSpan' :: RealSrcLoc -> RealSrcLoc -> (StringBuffer, String) -> (StringBuffer, String)
        takeSpan' start end (sb, taken) | start < end && not (atEnd sb)
          = let (c,rem) = nextChar sb in takeSpan' (advanceSrcLoc start c) end (rem, c:taken)
        takeSpan' _ _ (rem, taken) = (rem, taken)
        
-- | Replaces the ranges in the AST with the source file parts
applyFragments :: StructuralTraversable node => [String] -> Ann node (NodeInfo sema RangeTemplate) 
                                                         -> Ann node (NodeInfo sema SourceTemplate)
applyFragments srcs = flip evalState srcs
  . traverseDown 
     (return ()) (return ())
     (\ni -> do template <- mapM getTextFor (ni ^. sourceInfo&rangeTemplateElems)
                return $ sourceInfo .= SourceTemplate (RealSrcSpan $ ni ^. sourceInfo&rangeTemplateSpan) template $ ni)
  where getTextFor (RangeElem sp) = do (src:rest) <- get
                                       put rest
                                       return (TextElem src)
        getTextFor RangeChildElem = return ChildElem
        getTextFor (RangeOptionalElem bef aft) = return (OptionalChildElem bef aft)
        getTextFor (RangeListElem bef aft sep indented seps) 
          = do (own, rest) <- splitAt (length seps) <$> get 
               put rest
               return (ChildListElem bef aft sep indented own)
        
