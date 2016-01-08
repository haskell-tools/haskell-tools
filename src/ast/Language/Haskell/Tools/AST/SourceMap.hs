module Language.Haskell.Tools.AST.SourceMap where

import ApiAnnotation
import Data.Map as Map
import Data.List as List

-- import Language.Haskell.Tools.AST as AST
import SrcLoc as GHC
import FastString as GHC

-- We store tokens in the source map so it is not a problem that they cannot overlap
type SourceMap = Map AnnKeywordId (Map SrcLoc SrcLoc)

getKeywordInside :: AnnKeywordId -> SrcSpan -> SourceMap -> Maybe SrcSpan
getKeywordInside keyw sr srcmap = getSourceElementInside sr =<< Map.lookup keyw srcmap

getSourceElementInside :: SrcSpan -> Map SrcLoc SrcLoc -> Maybe SrcSpan
getSourceElementInside sr srcmap = 
  case lookupGE (srcSpanStart sr) srcmap of
    Just (k, v) -> if k <= srcSpanEnd sr then Just (mkSrcSpan k v)
                                         else Nothing
    Nothing -> Nothing
    
annotationsToSrcMap :: Map ApiAnnKey [SrcSpan] -> Map AnnKeywordId (Map SrcLoc SrcLoc)
annotationsToSrcMap anns = Map.map (List.foldr addToSrcRanges Map.empty) $ mapKeysWith (++) snd anns
  where 
    addToSrcRanges :: SrcSpan -> Map SrcLoc SrcLoc -> Map SrcLoc SrcLoc
    addToSrcRanges span srcmap = Map.insert (srcSpanStart span) (srcSpanEnd span) srcmap
    
    
                