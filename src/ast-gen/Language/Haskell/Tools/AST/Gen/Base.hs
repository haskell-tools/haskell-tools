-- | Generation of basic AST fragments for refactorings
{-# LANGUAGE OverloadedStrings
           , ViewPatterns
           #-}
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

mkSimpleName :: TemplateAnnot a => String -> Ann SimpleName a
mkSimpleName s = mkAnn (fromString s) (SimpleName s)

mkUnqualName' :: TemplateAnnot a => GHC.Name -> Ann Name a
mkUnqualName' = mkUnqualName . GHC.occNameString . GHC.getOccName
                      
mkUnqualName :: TemplateAnnot a => String -> Ann Name a
mkUnqualName n = mkAnn (child <> child) 
                       (Name emptyList (mkAnn (fromString n) (SimpleName n)))

mkQualifiedName' :: TemplateAnnot a => [String] -> GHC.Name -> Ann Name a
mkQualifiedName' [] n = mkUnqualName' n
mkQualifiedName' quals (GHC.occNameString . GHC.getOccName -> name) 
  = mkAnn (child <> "." <> child)
          (Name (mkAnnList (listSep ".") $ map (\q -> mkAnn (fromString q) (SimpleName q)) quals) 
                (mkAnn (fromString name) (SimpleName name)))