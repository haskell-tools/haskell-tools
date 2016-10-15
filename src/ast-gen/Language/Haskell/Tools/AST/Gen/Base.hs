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

-- | Creates a simple, unqualified name
mkName :: String -> Ann Name dom SrcTemplateStage
mkName = mkNormalName . mkSimpleName

mkQualOp :: [String] -> String -> Ann Operator dom SrcTemplateStage
mkQualOp quals = mkAnn child . UNormalOp . mkQualifiedName quals

mkBacktickOp :: [String] -> String -> Ann Operator dom SrcTemplateStage
mkBacktickOp quals = mkAnn ("`" <> child <> "`") . UBacktickOp . mkQualifiedName quals

-- | Creates an annotated qualified operator: @A.B.+@ or @`A.B.mod`@.
mkQualOp' :: [String] -> GHC.Name -> Ann Operator dom SrcTemplateStage
mkQualOp' quals n | GHC.isSymOcc (GHC.getOccName n) = mkAnn child $ UNormalOp $ mkQualifiedName' quals n
                  | otherwise                       = mkAnn ("`" <> child <> "`") $ UBacktickOp $ mkQualifiedName' quals n

-- | Creates an annotated unqualified operator: @+@ or @`mod`@.
mkUnqualOp' :: GHC.Name -> Ann Operator dom SrcTemplateStage
mkUnqualOp' n | GHC.isSymOcc (GHC.getOccName n) = mkAnn child $ UNormalOp $ mkSimpleName' n
              | otherwise                       = mkAnn ("`" <> child <> "`") $ UBacktickOp $ mkSimpleName' n
  
mkUnqualOp :: String -> Ann Operator dom SrcTemplateStage
mkUnqualOp = mkAnn child . UNormalOp . mkSimpleName

-- | Creates an annotated qualified (non-operator) binding name: @A.B.f@ or @(A.B.+)@
mkQualName' :: [String] -> GHC.Name -> Ann Name dom SrcTemplateStage
mkQualName' quals n | GHC.isSymOcc (GHC.getOccName n) = mkAnn ("(" <> child <> ")") $ UParenName $ mkQualifiedName' quals n
                    | otherwise                       = mkAnn child $ UNormalName $ mkQualifiedName' quals n

-- | Creates an annotated unqualified (non-operator) binding name: @f@ or @(+)@
mkUnqualName' :: GHC.Name -> Ann Name dom SrcTemplateStage
mkUnqualName' n | GHC.isSymOcc (GHC.getOccName n) = mkAnn ("(" <> child <> ")") $ UParenName $ mkSimpleName' n
                | otherwise                       = mkAnn child $ UNormalName $ mkSimpleName' n

mkNormalName :: Ann QualifiedName dom SrcTemplateStage -> Ann Name dom SrcTemplateStage
mkNormalName = mkAnn child . UNormalName

mkParenName :: Ann QualifiedName dom SrcTemplateStage -> Ann Name dom SrcTemplateStage
mkParenName = mkAnn ("(" <> child <> ")") . UParenName

-- | Creates an annotated qualified simple name
mkQualifiedName' :: [String] -> GHC.Name -> Ann QualifiedName dom SrcTemplateStage
mkQualifiedName' quals n = mkQualifiedName quals (GHC.occNameString $ GHC.getOccName n)

mkQualifiedName :: [String] -> String -> Ann QualifiedName dom SrcTemplateStage
mkQualifiedName [] n = mkSimpleName n
mkQualifiedName quals name
  = mkAnn (child <> "." <> child)
          (QualifiedName (mkAnnList (listSep ".") $ map mkNamePart quals) (mkNamePart name))

mkNamePart :: String -> Ann NamePart dom SrcTemplateStage
mkNamePart s = mkAnn (fromString s) (UNamePart s)

mkSimpleName' :: GHC.Name -> Ann QualifiedName dom SrcTemplateStage
mkSimpleName' = mkSimpleName . GHC.occNameString . GHC.getOccName

mkSimpleName :: String -> Ann QualifiedName dom SrcTemplateStage
mkSimpleName n = mkAnn (child <> child) 
                       (QualifiedName emptyList (mkNamePart n))

mkStringNode :: String -> Ann StringNode dom SrcTemplateStage
mkStringNode s = mkAnn (fromString s) (UStringNode s)

mkModuleName :: String -> Ann ModuleName dom SrcTemplateStage
mkModuleName s = mkAnn (fromString s) (UModuleName s)

mkDataKeyword :: Ann DataOrNewtypeKeyword dom SrcTemplateStage
mkDataKeyword = mkAnn "data" UDataKeyword

mkNewtypeKeyword :: Ann DataOrNewtypeKeyword dom SrcTemplateStage
mkNewtypeKeyword = mkAnn "newtype" UNewtypeKeyword

