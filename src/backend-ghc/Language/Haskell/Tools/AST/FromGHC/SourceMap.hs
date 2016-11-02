{-# LANGUAGE TupleSections #-}
-- | A representation of the tokens that build up the source file.
module Language.Haskell.Tools.AST.FromGHC.SourceMap where

import ApiAnnotation
import Data.Maybe
import Data.Map as Map
import Data.List as List
import Safe

import SrcLoc as GHC
import FastString as GHC

-- We store tokens in the source map so it is not a problem that they cannot overlap
type SourceMap = (Map AnnKeywordId (Map SrcLoc SrcLoc), Map SrcLoc (SrcSpan, AnnKeywordId))

-- | Returns the first occurrence of the keyword in the whole source file
getKeywordAnywhere :: AnnKeywordId -> SourceMap -> Maybe SrcSpan
getKeywordAnywhere keyw srcmap = return . uncurry mkSrcSpan =<< headMay . assocs =<< (Map.lookup keyw (fst srcmap))

-- | Get the source location of a token restricted to a certain source span
getKeywordInside :: AnnKeywordId -> SrcSpan -> SourceMap -> Maybe SrcSpan
getKeywordInside keyw sr srcmap = getSourceElementInside True sr =<< Map.lookup keyw (fst srcmap)

getKeywordsInside :: AnnKeywordId -> SrcSpan -> SourceMap -> [SrcSpan]
getKeywordsInside keyw sr srcmap 
  = let tokensOfType = Map.lookup keyw (fst srcmap)
        (_, startsAtBegin, startAfterBegin) = Map.splitLookup (srcSpanStart sr) $ fromMaybe empty tokensOfType
        (startsBeforeEnd, _) = Map.split (srcSpanEnd sr) $ maybe id (Map.insert (srcSpanStart sr)) startsAtBegin startAfterBegin -- tokens are minimum 1 char long
     in List.map (uncurry mkSrcSpan) $ List.filter (\(_, end) -> end <= srcSpanEnd sr) $ assocs startsBeforeEnd

getKeywordInsideBack :: AnnKeywordId -> SrcSpan -> SourceMap -> Maybe SrcSpan
getKeywordInsideBack keyw sr srcmap = getSourceElementInside False sr =<< Map.lookup keyw (fst srcmap)

getSourceElementInside :: Bool -> SrcSpan -> Map SrcLoc SrcLoc -> Maybe SrcSpan
getSourceElementInside b sr srcmap = 
  case (if b then lookupGE (srcSpanStart sr) else lookupLT (srcSpanEnd sr)) srcmap of
    Just (k, v) -> let sp = mkSrcSpan k v in if sp `isSubspanOf` sr then Just sp else Nothing
    Nothing -> Nothing

-- | Returns the next token on the token stream (including the token that starts on the given location)
getNextToken :: SrcLoc -> SourceMap -> Maybe (SrcSpan, AnnKeywordId)
getNextToken loc srcmap = fmap snd $ Map.lookupGE loc $ snd srcmap

-- | Returns all subsequent tokens (including the token that starts on the given location)
getTokensAfter :: SrcLoc -> SourceMap -> [(SrcSpan, AnnKeywordId)]
getTokensAfter loc srcmap = case Map.splitLookup loc $ snd srcmap of 
    (_, Just elem, after) -> elem : elems after
    (_, Nothing, after) -> elems after
    
-- | Converts GHC Annotations into a convenient format for looking up tokens
annotationsToSrcMap :: Map ApiAnnKey [SrcSpan] -> SourceMap
annotationsToSrcMap anns = (Map.map (List.foldr addToSrcRanges Map.empty) $ mapKeysWith (++) snd anns, tokenMap)
  where 
    addToSrcRanges :: SrcSpan -> Map SrcLoc SrcLoc -> Map SrcLoc SrcLoc
    addToSrcRanges span srcmap = Map.insert (srcSpanStart span) (srcSpanEnd span) srcmap

    tokenMap = Map.fromList $ List.map (\(k,v) -> (srcSpanStart k, (k, v))) $ concatMap (\(key,vals) -> List.map ((, snd key)) vals) $ Map.assocs anns
    
    
                