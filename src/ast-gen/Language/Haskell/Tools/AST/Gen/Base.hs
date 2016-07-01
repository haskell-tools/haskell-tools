-- | Generation of basic AST fragments (names for example) for refactorings
{-# LANGUAGE OverloadedStrings
           , ViewPatterns
           #-}
module Language.Haskell.Tools.AST.Gen.Base where

import qualified Name as GHC
import qualified Module as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

-- | Creates an annotated qualified operator: @A.B.+@ or @`A.B.mod`@.
mkQualOp' :: TemplateAnnot a => [String] -> GHC.Name -> Ann Operator a
mkQualOp' quals n | GHC.isSymOcc (GHC.getOccName n) = mkAnn child $ NormalOp $ mkQualifiedName' quals n
                  | otherwise                       = mkAnn ("`" <> child <> "`") $ BacktickOp $ mkQualifiedName' quals n

-- | Creates an annotated unqualified operator: @+@ or @`mod`@.
mkUnqualOp' :: TemplateAnnot a => GHC.Name -> Ann Operator a
mkUnqualOp' n | GHC.isSymOcc (GHC.getOccName n) = mkAnn child $ NormalOp $ mkSimpleName' n
              | otherwise                       = mkAnn ("`" <> child <> "`") $ BacktickOp $ mkSimpleName' n
  
-- | Creates an annotated qualified (non-operator) binding name: @A.B.f@ or @(A.B.+)@
mkQualName' :: TemplateAnnot a => [String] -> GHC.Name -> Ann Name a
mkQualName' quals n | GHC.isSymOcc (GHC.getOccName n) = mkAnn ("(" <> child <> ")") $ ParenName $ mkQualifiedName' quals n
                    | otherwise                       = mkAnn child $ NormalName $ mkQualifiedName' quals n

-- | Creates an annotated unqualified (non-operator) binding name: @f@ or @(+)@
mkUnqualName' :: TemplateAnnot a => GHC.Name -> Ann Name a
mkUnqualName' n | GHC.isSymOcc (GHC.getOccName n) = mkAnn ("(" <> child <> ")") $ ParenName $ mkSimpleName' n
                | otherwise                       = mkAnn child $ NormalName $ mkSimpleName' n

mkNormalName :: TemplateAnnot a => Ann SimpleName a -> Ann Name a
mkNormalName = mkAnn child . NormalName

mkParenName :: TemplateAnnot a => Ann SimpleName a -> Ann Name a
mkParenName = mkAnn ("(" <> child <> ")") . ParenName

-- | Creates an annotated qualified simple name
mkQualifiedName' :: TemplateAnnot a => [String] -> GHC.Name -> Ann SimpleName a
mkQualifiedName' [] n = mkSimpleName' n
mkQualifiedName' quals (GHC.occNameString . GHC.getOccName -> name) 
  = mkAnn (child <> "." <> child)
          (SimpleName (mkAnnList (listSep ".") $ map (\q -> mkAnn (fromString q) (UnqualName q)) quals) 
                      (mkAnn (fromString name) (UnqualName name)))

-- | Creates an annotated part of a name.
mkNamePart :: TemplateAnnot a => String -> Ann UnqualName a
mkNamePart s = mkAnn (fromString s) (UnqualName s)

mkSimpleName' :: TemplateAnnot a => GHC.Name -> Ann SimpleName a
mkSimpleName' = mkSimpleName . GHC.occNameString . GHC.getOccName

mkSimpleName :: TemplateAnnot a => String -> Ann SimpleName a
mkSimpleName n = mkAnn (child <> child) 
                       (SimpleName emptyList (mkAnn (fromString n) (UnqualName n)))