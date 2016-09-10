-- | Generation of basic AST fragments (names for example) for refactorings
{-# LANGUAGE OverloadedStrings
           , ViewPatterns
           , TypeFamilies
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
mkQualOp' :: [String] -> GHC.Name -> Ann Operator dom SrcTemplateStage
mkQualOp' quals n | GHC.isSymOcc (GHC.getOccName n) = mkAnn child $ NormalOp $ mkQualifiedName' quals n
                  | otherwise                       = mkAnn ("`" <> child <> "`") $ BacktickOp $ mkQualifiedName' quals n

-- | Creates an annotated unqualified operator: @+@ or @`mod`@.
mkUnqualOp' :: GHC.Name -> Ann Operator dom SrcTemplateStage
mkUnqualOp' n | GHC.isSymOcc (GHC.getOccName n) = mkAnn child $ NormalOp $ mkSimpleName' n
              | otherwise                       = mkAnn ("`" <> child <> "`") $ BacktickOp $ mkSimpleName' n
  
-- | Creates an annotated qualified (non-operator) binding name: @A.B.f@ or @(A.B.+)@
mkQualName' :: [String] -> GHC.Name -> Ann Name dom SrcTemplateStage
mkQualName' quals n | GHC.isSymOcc (GHC.getOccName n) = mkAnn ("(" <> child <> ")") $ ParenName $ mkQualifiedName' quals n
                    | otherwise                       = mkAnn child $ NormalName $ mkQualifiedName' quals n

-- | Creates an annotated unqualified (non-operator) binding name: @f@ or @(+)@
mkUnqualName' :: GHC.Name -> Ann Name dom SrcTemplateStage
mkUnqualName' n | GHC.isSymOcc (GHC.getOccName n) = mkAnn ("(" <> child <> ")") $ ParenName $ mkSimpleName' n
                | otherwise                       = mkAnn child $ NormalName $ mkSimpleName' n

mkNormalName :: Ann QualifiedName dom SrcTemplateStage -> Ann Name dom SrcTemplateStage
mkNormalName = mkAnn child . NormalName

mkParenName :: Ann QualifiedName dom SrcTemplateStage -> Ann Name dom SrcTemplateStage
mkParenName = mkAnn ("(" <> child <> ")") . ParenName

-- | Creates an annotated qualified simple name
mkQualifiedName' :: [String] -> GHC.Name -> Ann QualifiedName dom SrcTemplateStage
mkQualifiedName' quals n = mkQualifiedName quals (GHC.occNameString $ GHC.getOccName n)

mkQualifiedName :: [String] -> String -> Ann QualifiedName dom SrcTemplateStage
mkQualifiedName [] n = mkSimpleName n
mkQualifiedName quals name
  = mkAnn (child <> "." <> child)
          (QualifiedName (mkAnnList (listSep ".") $ map (\q -> mkAnn (fromString q) (UnqualName q)) quals) 
                      (mkAnn (fromString name) (UnqualName name)))

-- | Creates an annotated part of a name.
mkNamePart :: String -> Ann UnqualName dom SrcTemplateStage
mkNamePart s = mkAnn (fromString s) (UnqualName s)

mkSimpleName' :: GHC.Name -> Ann QualifiedName dom SrcTemplateStage
mkSimpleName' = mkSimpleName . GHC.occNameString . GHC.getOccName

mkSimpleName :: String -> Ann QualifiedName dom SrcTemplateStage
mkSimpleName n = mkAnn (child <> child) 
                       (QualifiedName emptyList (mkAnn (fromString n) (UnqualName n)))

mkModuleName :: String -> Ann ModuleName dom SrcTemplateStage
mkModuleName s = mkAnn (fromString s) (ModuleName s)