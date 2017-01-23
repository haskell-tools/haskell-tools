{-# LANGUAGE TupleSections #-}
-- | Defines operation on AST lists. 
-- AST lists carry source information so simple list modification is not enough.
module Language.Haskell.Tools.Refactor.ListOperations where

import Control.Reference

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Rewrite (AnnMaybe(..), AnnList(..))
import Language.Haskell.Tools.Transform (srcTmpDefaultSeparator, srcTmpSeparators)

-- | Filters the elements of the list. By default it removes the separator before the element.
-- Of course, if the first element is removed, the following separator is removed as well.
filterList :: (Ann e dom SrcTemplateStage -> Bool) -> AnnList e dom -> AnnList e dom
filterList pred = filterListIndexed (const pred)

filterListIndexed :: (Int -> Ann e dom SrcTemplateStage -> Bool) -> AnnList e dom -> AnnList e dom
filterListIndexed pred (AnnListG (NodeInfo sema src) elems)
  = let (filteredElems, separators) = filterElems 0 elems (src ^. srcTmpSeparators)
     in AnnListG (NodeInfo sema (srcTmpSeparators .= separators $ src)) filteredElems
  where filterElems i (elem:ls) (sep:seps) 
          | pred i elem = let (elems',seps') = filterElems' (i+1) ls (sep:seps) in (elem:elems', seps')
          | otherwise = filterElems (i+1) ls seps
        filterElems i elems [] = (filter (pred i) elems, [])
        filterElems _ [] seps = ([], seps)
        
        filterElems' i (elem:ls) (sep:seps) 
          | pred i elem = let (elems',seps') = filterElems' (i+1) ls seps in (elem:elems', sep:seps')
          | otherwise = filterElems' (i+1) ls seps
        filterElems' i elems [] = (filter (pred i) elems, [])
        filterElems' _ [] seps = ([], seps)
                            
-- | Inserts the element in the places where the two positioning functions (one checks the element before, one the element after)
-- allows the placement.         
insertWhere :: Ann e dom SrcTemplateStage -> (Maybe (Ann e dom SrcTemplateStage) -> Bool) 
                 -> (Maybe (Ann e dom SrcTemplateStage) -> Bool) -> AnnList e dom 
                 -> AnnList e dom
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

-- | Gets the elements and separators from a list. The first separator is zipped to the second element.
-- To the first element, the "" string is zipped.
zipWithSeparators :: AnnList e dom -> [(String, Ann e dom SrcTemplateStage)]
zipWithSeparators (AnnListG (NodeInfo _ src) elems) 
  | [] <- src ^. srcTmpSeparators 
  = zip ("" : repeat (src ^. srcTmpDefaultSeparator)) elems
  | otherwise 
  = zip ("" : seps ++ repeat (last seps)) elems
  where seps = src ^. srcTmpSeparators
