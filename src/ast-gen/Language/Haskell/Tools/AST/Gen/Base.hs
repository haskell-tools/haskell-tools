-- | Generation of basic AST fragments for refactorings
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Tools.AST.Gen.Base where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkUnqualName' :: TemplateAnnot a => GHC.Name -> Ann Name a
mkUnqualName' = mkUnqualName . GHC.occNameString . GHC.getOccName
                      
mkUnqualName :: TemplateAnnot a => String -> Ann Name a
mkUnqualName n = mkAnn (child <> child) 
                       (Name emptyList (Ann (fromTemplate (fromString n)) 
                                            (SimpleName n)))