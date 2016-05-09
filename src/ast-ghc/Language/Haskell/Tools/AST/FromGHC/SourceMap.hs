-- | A representation of the tokens that build up the source file.
module Language.Haskell.Tools.AST.FromGHC.SourceMap where

import ApiAnnotation
import Data.Map as Map
import Data.List as List
import Safe

import SrcLoc as GHC
import FastString as GHC

-- We store tokens in the source map so it is not a problem that they cannot overlap
type SourceMap = Map AnnKeywordId (Map SrcLoc SrcLoc)

-- | Returns the first occurrence of the keyword in the whole source file
getKeywordAnywhere :: AnnKeywordId -> SourceMap -> Maybe SrcSpan
getKeywordAnywhere keyw srcmap = return . uncurry mkSrcSpan =<< headMay . assocs =<< (Map.lookup keyw srcmap)

-- | Get the source location of a token restricted to a certain source span
getKeywordInside :: AnnKeywordId -> SrcSpan -> SourceMap -> Maybe SrcSpan
getKeywordInside keyw sr srcmap = getSourceElementInside True sr =<< Map.lookup keyw srcmap

getKeywordInsideBack :: AnnKeywordId -> SrcSpan -> SourceMap -> Maybe SrcSpan
getKeywordInsideBack keyw sr srcmap = getSourceElementInside False sr =<< Map.lookup keyw srcmap

getSourceElementInside :: Bool -> SrcSpan -> Map SrcLoc SrcLoc -> Maybe SrcSpan
getSourceElementInside b sr srcmap = 
  case (if b then lookupGE (srcSpanStart sr) else lookupLE (srcSpanEnd sr)) srcmap of
    Just (k, v) -> let sp = mkSrcSpan k v in if sp `isSubspanOf` sr then Just sp else Nothing
    Nothing -> Nothing
    
-- | Converts GHC Annotations into a convenient format for looking up tokens
annotationsToSrcMap :: Map ApiAnnKey [SrcSpan] -> Map AnnKeywordId (Map SrcLoc SrcLoc)
annotationsToSrcMap anns = Map.map (List.foldr addToSrcRanges Map.empty) $ mapKeysWith (++) snd anns
  where 
    addToSrcRanges :: SrcSpan -> Map SrcLoc SrcLoc -> Map SrcLoc SrcLoc
    addToSrcRanges span srcmap = Map.insert (srcSpanStart span) (srcSpanEnd span) srcmap
    
    
                