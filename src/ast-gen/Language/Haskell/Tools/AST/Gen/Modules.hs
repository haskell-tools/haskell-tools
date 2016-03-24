{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Tools.AST.Gen.Modules where

import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AnnTrf.SourceTemplate

mkImportSpecList :: TemplateAnnot a => [Ann IESpec a] -> Ann ImportSpec a
mkImportSpecList specs = Ann (fromTemplate $ "(" <> child <> ")") 
                             (ImportSpecList (AnnList (fromTemplate list) specs))

mkIeSpec :: TemplateAnnot a => Ann Name a -> AnnMaybe SubSpec a -> Ann IESpec a
mkIeSpec name ss = Ann (fromTemplate $ child <> child) (IESpec name ss)
        
mkSubList :: TemplateAnnot a => [Ann Name a] -> Ann SubSpec a
mkSubList names = Ann (fromTemplate $ "(" <> child <> ")") 
                      (SubSpecList (AnnList (fromTemplate list) names))
                      
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

mkUnqualName :: TemplateAnnot a => String -> Ann Name a
mkUnqualName n = Ann (fromTemplate $ child <> child) 
                     (Name emptyList (Ann (fromTemplate (fromString n)) 
                                          (SimpleName n)))
              