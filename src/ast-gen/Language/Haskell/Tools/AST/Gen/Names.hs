-- | Generation of basic AST fragments (names for example) for refactorings
{-# LANGUAGE OverloadedStrings
           , ViewPatterns
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Names where

import qualified Name as GHC
import qualified Module as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

-- | Creates a simple, unqualified name
mkName :: String -> Name dom
mkName = mkNormalName . mkSimpleName

mkQualOp :: [String] -> String -> Operator dom
mkQualOp quals = mkAnn child . UNormalOp . mkQualifiedName quals

mkBacktickOp :: [String] -> String -> Operator dom
mkBacktickOp quals = mkAnn ("`" <> child <> "`") . UBacktickOp . mkQualifiedName quals

-- | Creates an annotated qualified operator: @A.B.+@ or @`A.B.mod`@.
mkQualOp' :: [String] -> GHC.Name -> Operator dom
mkQualOp' quals n | GHC.isSymOcc (GHC.getOccName n) = mkAnn child $ UNormalOp $ mkQualifiedName' quals n
                  | otherwise                       = mkAnn ("`" <> child <> "`") $ UBacktickOp $ mkQualifiedName' quals n

-- | Creates an annotated unqualified operator: @+@ or @`mod`@.
mkUnqualOp' :: GHC.Name -> Operator dom
mkUnqualOp' n | GHC.isSymOcc (GHC.getOccName n) = mkAnn child $ UNormalOp $ mkSimpleName' n
              | otherwise                       = mkAnn ("`" <> child <> "`") $ UBacktickOp $ mkSimpleName' n
  
mkUnqualOp :: String -> Operator dom
mkUnqualOp = mkAnn child . UNormalOp . mkSimpleName

-- | Creates an annotated qualified (non-operator) binding name: @A.B.f@ or @(A.B.+)@
mkQualName' :: [String] -> GHC.Name -> Name dom
mkQualName' quals n | GHC.isSymOcc (GHC.getOccName n) = mkAnn ("(" <> child <> ")") $ UParenName $ mkQualifiedName' quals n
                    | otherwise                       = mkAnn child $ UNormalName $ mkQualifiedName' quals n

-- | Creates an annotated unqualified (non-operator) binding name: @f@ or @(+)@
mkUnqualName' :: GHC.Name -> Name dom
mkUnqualName' n | GHC.isSymOcc (GHC.getOccName n) = mkAnn ("(" <> child <> ")") $ UParenName $ mkSimpleName' n
                | otherwise                       = mkAnn child $ UNormalName $ mkSimpleName' n

mkNormalName :: QualifiedName dom -> Name dom
mkNormalName = mkAnn child . UNormalName

-- | Creates a parenthesized name: @ foldl (+) 0 @
mkParenName :: QualifiedName dom -> Name dom
mkParenName = mkAnn ("(" <> child <> ")") . UParenName

-- | Creates an implicit name: @ ?var @
mkImplicitName :: QualifiedName dom -> Name dom
mkImplicitName = mkAnn ("?" <> child) . UImplicitName

-- | Creates an annotated qualified simple name
mkQualifiedName' :: [String] -> GHC.Name -> QualifiedName dom
mkQualifiedName' quals n = mkQualifiedName quals (GHC.occNameString $ GHC.getOccName n)

mkQualifiedName :: [String] -> String -> QualifiedName dom
mkQualifiedName [] n = mkSimpleName n
mkQualifiedName quals name
  = mkAnn (child <> "." <> child)
          (UQualifiedName (mkAnnList (listSep ".") $ map mkNamePart quals) (mkNamePart name))

-- | Creates a part of a qualified name.         
mkNamePart :: String -> NamePart dom
mkNamePart s = mkAnn (fromString s) (UNamePart s)

-- | Creates a simple (unqualified) name
mkSimpleName' :: GHC.Name -> QualifiedName dom
mkSimpleName' = mkSimpleName . GHC.occNameString . GHC.getOccName

-- | Creates a simple (unqualified) name
mkSimpleName :: String -> QualifiedName dom
mkSimpleName n = mkAnn (child <> child) 
                       (UQualifiedName emptyList (mkNamePart n))

-- | Creates a quoted text
mkStringNode :: String -> StringNode dom
mkStringNode s = mkAnn (fromString s) (UStringNode s)
