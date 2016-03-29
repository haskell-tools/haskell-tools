{-# LANGUAGE OverloadedStrings
           , FlexibleContexts
           #-}
module Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers where

import SrcLoc
import Data.String
import Data.List
import Control.Reference
import Data.Function (on)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate

filterList :: TemplateAnnot a => (Ann e a -> Bool) -> AnnList e a -> AnnList e a
filterList pred ls = replaceList (filter pred (ls ^. annListElems)) ls   
       
replaceList :: TemplateAnnot a => [Ann e a] -> AnnList e a -> AnnList e a
replaceList elems (AnnList a _)
  = AnnList (fromTemplate (listSep mostCommonSeparator)) elems
  where mostCommonSeparator  
          = case getTemplate a ^. sourceTemplateElems of 
              [ChildListElem sep _ seps] -> case maximumBy (compare `on` length) $ group $ sort seps of 
                                             [] -> sep
                                             sep:_ -> sep
                                     
insertWhere :: (TemplateAnnot a) => Ann e a -> (Maybe (Ann e a) -> Bool) -> (Maybe (Ann e a) -> Bool) -> AnnList e a -> AnnList e a
insertWhere e before after al 
  = let index = insertIndex before after (al ^? annList)
     in case index of 
          Nothing -> al
          Just ind -> annListElems .- insertAt ind e 
                        $ (if isEmptyAnnList then id else addDefaultSeparator ind)
                        $ al 
  where addDefaultSeparator i al 
          = srcTemplateElems&srcTmpSeparators 
               .- insertAt i (head $ al ^? srcTemplateElems&srcTmpDefaultSeparator) $ al
        srcTemplateElems :: (TemplateAnnot a) => Simple Traversal (AnnList e a) SourceTemplateElem
        srcTemplateElems = annListAnnot&template&sourceTemplateElems&traversal
        insertAt n e ls = let (bef,aft) = splitAt n ls in bef ++ [e] ++ aft
        isEmptyAnnList = (null :: [x] -> Bool) $ (al ^? annList)

insertIndex :: (Maybe (Ann e a) -> Bool) -> (Maybe (Ann e a) -> Bool) -> [Ann e a] -> Maybe Int
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
                                     
class TemplateAnnot annot where
  template :: Simple Lens annot SourceTemplate
  template = iso getTemplate fromTemplate
  fromTemplate :: SourceTemplate -> annot
  getTemplate :: annot -> SourceTemplate
  
instance IsString SourceTemplate where
  fromString s = SourceTemplate noSrcSpan [TextElem s]
     
child :: SourceTemplate
child = SourceTemplate noSrcSpan [ChildElem]

opt :: SourceTemplate
opt = SourceTemplate noSrcSpan [OptionalChildElem "" ""]

optBefore :: String -> SourceTemplate
optBefore s = SourceTemplate noSrcSpan [OptionalChildElem s ""]

optAfter :: String -> SourceTemplate
optAfter s = SourceTemplate noSrcSpan [OptionalChildElem "" s]

list :: SourceTemplate
list = SourceTemplate noSrcSpan [ChildListElem "" False []]

indentedList :: SourceTemplate
indentedList = SourceTemplate noSrcSpan [ChildListElem "" True []]

listSep :: String -> SourceTemplate
listSep s = SourceTemplate noSrcSpan [ChildListElem s False []]

(<>) :: SourceTemplate -> SourceTemplate -> SourceTemplate
SourceTemplate sp1 el1 <> SourceTemplate sp2 el2 = SourceTemplate (combineSrcSpans sp1 sp2) (el1 ++ el2)
