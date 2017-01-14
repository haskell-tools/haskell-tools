{-# LANGUAGE LambdaCase 
           , FlexibleContexts
           #-}
-- | This module converts range templates into source templates. 
-- Basically it reads the source file and attaches parts of the source file to the AST elements that have the range of the given source code fragment.
module Language.Haskell.Tools.Transform.RangeTemplateToSourceTemplate where

import Control.Monad.State
import Control.Reference ((^.))
import Data.Map
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Transform.RangeTemplate
import Language.Haskell.Tools.Transform.SourceTemplate
import SrcLoc
import StringBuffer (StringBuffer(..), nextChar, atEnd)

rangeToSource :: SourceInfoTraversal node => StringBuffer -> Ann node dom RngTemplateStage
                                                          -> Ann node dom SrcTemplateStage
rangeToSource srcInput tree = let locIndices = getLocIndices tree
                                  srcMap = mapLocIndices srcInput locIndices
                               in applyFragments (elems srcMap) tree

-- maps could be strict

-- | Assigns an index (in the order they are used) for each range
getLocIndices :: SourceInfoTraversal e => Ann e dom RngTemplateStage -> Map OrdSrcSpan Int
getLocIndices = snd . flip execState (0, empty) .
  sourceInfoTraverseDown (SourceInfoTrf 
      (\ni -> do { mapM_ (\el -> case getRangeElemSpan el of Just sp -> modify (insertElem sp); _ -> return ()) (ni ^. rngTemplateNodeElems); return ni })
      (\ni -> do { mapM_ (modify . insertElem) (ni ^. rngTmpSeparators); return ni })
      pure ) 
    (return ()) (return ())
  where insertElem sp (i,m) = (i+1, insert (OrdSrcSpan sp) i m)
                             
                             
-- | Partitions the source file in the order where the parts are used in the AST
mapLocIndices :: Ord k => StringBuffer -> Map OrdSrcSpan k -> Map k String
mapLocIndices inp = fst . foldlWithKey (\(new, str) sp k -> let (rem, val) = takeSpan str sp
                                                             in (insert k (reverse val) new, rem)) (empty, inp)
  where takeSpan :: StringBuffer -> OrdSrcSpan -> (StringBuffer, String)
        takeSpan str (OrdSrcSpan sp) = takeSpan' (realSrcSpanStart sp) (realSrcSpanEnd sp) (str,"")
        takeSpan _ (NoOrdSrcSpan {}) = error "takeSpan: missing source span"

        takeSpan' :: RealSrcLoc -> RealSrcLoc -> (StringBuffer, String) -> (StringBuffer, String)
        takeSpan' start end (sb, taken) | start < end && not (atEnd sb)
          = let (c,rem) = nextChar sb in takeSpan' (advanceSrcLoc start c) end (rem, c:taken)
        takeSpan' _ _ (rem, taken) = (rem, taken)
        
-- | Replaces the ranges in the AST with the source file parts
applyFragments :: SourceInfoTraversal node => [String] -> Ann node dom RngTemplateStage
                                                       -> Ann node dom SrcTemplateStage
applyFragments srcs = flip evalState srcs
  . sourceInfoTraverseDown (SourceInfoTrf
     (\ni -> do template <- mapM getTextFor (ni ^. rngTemplateNodeElems)
                return $ SourceTemplateNode (RealSrcSpan $ ni ^. rngTemplateNodeRange) template 0 Nothing)
     (\(RangeTemplateList rng bef aft sep indented seps) 
         -> do (own, rest) <- splitAt (length seps) <$> get 
               put rest
               return (SourceTemplateList (RealSrcSpan rng) bef aft sep indented own 0 Nothing))
     (\(RangeTemplateOpt rng bef aft) -> return (SourceTemplateOpt (RealSrcSpan rng) bef aft 0 Nothing))) 
     (return ()) (return ())
  where getTextFor RangeChildElem = return ChildElem
        getTextFor (RangeElem _) = do (src:rest) <- get
                                      put rest
                                      return (TextElem src)