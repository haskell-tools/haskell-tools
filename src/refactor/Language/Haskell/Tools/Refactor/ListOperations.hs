-- | Defines operation on AST lists. 
-- AST lists carry source information so simple list modification is not enough.
module Language.Haskell.Tools.Refactor.ListOperations where

import Control.Reference

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Rewrite
import Language.Haskell.Tools.Transform

-- | Filters the elements of the list. By default it removes the separator before the element.
-- Of course, if the first element is removed, the following separator is removed as well.
filterList :: (Ann e dom SrcTemplateStage -> Bool) -> AnnListG e dom SrcTemplateStage -> AnnListG e dom SrcTemplateStage
filterList pred (AnnListG (NodeInfo sema src) elems)
  = let (filteredElems, separators) = filterElems elems (src ^. srcTmpSeparators)
     in AnnListG (NodeInfo sema (srcTmpSeparators .= separators $ src)) filteredElems
  where filterElems (elem:ls) (sep:seps) 
          | pred elem = let (elems',seps') = filterElems' ls (sep:seps) in (elem:elems', seps')
          | otherwise = filterElems ls seps
        filterElems elems [] = (filter pred elems, [])
        filterElems [] seps = ([], seps)
        
        filterElems' (elem:ls) (sep:seps) 
          | pred elem = let (elems',seps') = filterElems' ls seps in (elem:elems', sep:seps')
          | otherwise = filterElems' ls seps
        filterElems' elems [] = (filter pred elems, [])
        filterElems' [] seps = ([], seps)
                            
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
        insertIndex' before after [] 
          | before Nothing && after Nothing = Just 0
          | otherwise = Nothing

replaceWithJust :: Ann e dom SrcTemplateStage -> AnnMaybe e dom -> AnnMaybe e dom           
replaceWithJust e (AnnMaybeG temp _) = AnnMaybeG temp (Just e)