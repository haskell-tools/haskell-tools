{-# LANGUAGE OverloadedStrings #-}
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
              [ChildListElem sep seps] -> case maximumBy (compare `on` length) $ group $ sort seps of 
                                           [] -> sep
                                           sep:_ -> sep
                                           
class TemplateAnnot annot where
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
list = SourceTemplate noSrcSpan [ChildListElem "" []]

listSep :: String -> SourceTemplate
listSep s = SourceTemplate noSrcSpan [ChildListElem s []]

(<>) :: SourceTemplate -> SourceTemplate -> SourceTemplate
SourceTemplate sp1 el1 <> SourceTemplate sp2 el2 = SourceTemplate (combineSrcSpans sp1 sp2) (el1 ++ el2)
