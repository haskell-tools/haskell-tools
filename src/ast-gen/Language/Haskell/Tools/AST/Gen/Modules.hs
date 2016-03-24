{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Tools.AST.Gen.Modules where

import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkImportSpecList :: TemplateAnnot a => [Ann IESpec a] -> Ann ImportSpec a
mkImportSpecList specs = Ann (fromTemplate $ "(" <> child <> ")") 
                             (ImportSpecList (AnnList (fromTemplate list) specs))

mkIeSpec :: TemplateAnnot a => Ann Name a -> AnnMaybe SubSpec a -> Ann IESpec a
mkIeSpec name ss = Ann (fromTemplate $ child <> child) (IESpec name ss)
        
mkSubList :: TemplateAnnot a => [Ann Name a] -> Ann SubSpec a
mkSubList names = Ann (fromTemplate $ "(" <> child <> ")") 
                      (SubSpecList (AnnList (fromTemplate list) names))

mkUnqualName :: TemplateAnnot a => String -> Ann Name a
mkUnqualName n = Ann (fromTemplate $ child <> child) 
                     (Name emptyList (Ann (fromTemplate (fromString n)) 
                                          (SimpleName n)))
              