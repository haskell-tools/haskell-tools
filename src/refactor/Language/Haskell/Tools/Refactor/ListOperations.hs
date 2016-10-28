module Language.Haskell.Tools.Refactor.ListOperations where

import SrcLoc
import Data.String
import Data.List
import Control.Reference
import Data.Function (on)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

filterList :: (Ann e dom SrcTemplateStage -> Bool) -> AnnListG e dom SrcTemplateStage -> AnnListG e dom SrcTemplateStage
-- QUESTION: is it OK? No problem from losing separators?
filterList pred ls = replaceList (filter pred (ls ^. annListElems)) ls   
       
-- | Replaces the list with a new one with the given elements, keeping the most common separator as the new one.
replaceList :: [Ann e dom SrcTemplateStage] -> AnnListG e dom SrcTemplateStage -> AnnListG e dom SrcTemplateStage
replaceList elems (AnnListG (NodeInfo sema src) _)
  = AnnListG (NodeInfo sema (listSep mostCommonSeparator)) elems
  where mostCommonSeparator  
          = case group $ sort (src ^. srcTmpSeparators) of 
                   [] -> src ^. srcTmpDefaultSeparator
                   nonempty@(_:_) -> head $ maximumBy (compare `on` length) nonempty
                            
-- | Inserts the element in the places where the two positioning functions (one checks the element before, one the element after)
-- allows the placement.         
insertWhere :: Ann e dom SrcTemplateStage -> (Maybe (Ann e dom SrcTemplateStage) -> Bool) 
                 -> (Maybe (Ann e dom SrcTemplateStage) -> Bool) -> AnnListG e dom SrcTemplateStage 
                 -> AnnListG e dom SrcTemplateStage
insertWhere e before after al 
  = let index = insertIndex before after (al ^? annList)
     in case index of 
          Nothing -> al
          Just ind -> annListElems .- insertAt ind e 
                        $ (if isEmptyAnnList then id else annListAnnot&sourceInfo .- addDefaultSeparator ind)
                        $ al 
  where addDefaultSeparator i al = srcTmpSeparators .- insertAt i (al ^. srcTmpDefaultSeparator) $ al
        insertAt n e ls = let (bef,aft) = splitAt n ls in bef ++ [e] ++ aft
        isEmptyAnnList = (null :: [x] -> Bool) $ (al ^? annList)

-- | Checks where the element will be inserted given the two positioning functions.
insertIndex :: (Maybe (Ann e dom SrcTemplateStage) -> Bool) -> (Maybe (Ann e dom SrcTemplateStage) -> Bool) -> [Ann e dom SrcTemplateStage] -> Maybe Int
insertIndex before after []
  | before Nothing && after Nothing = Just 0
  | otherwise = Nothing
insertIndex before after list@(first:_)
  | before Nothing && after (Just first) = Just 0
  | otherwise = (+1) <$> insertIndex' before after list 
  where insertIndex' before after (curr:rest@(next:_)) 
          | before (Just curr) && after (Just next) = Just 0
          | otherwise = (+1) <$> insertIndex' before after rest
        insertIndex' before after (curr:[]) 
          | before (Just curr) && after Nothing = Just 0
          | otherwise = Nothing