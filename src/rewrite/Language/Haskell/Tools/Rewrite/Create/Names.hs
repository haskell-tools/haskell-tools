-- | Generation of names for refactorings
{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Language.Haskell.Tools.Rewrite.Create.Names where

import Data.String (IsString(..), String)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Rewrite.Create.Utils (emptyList, mkAnn, mkAnnList)
import Language.Haskell.Tools.Rewrite.ElementTypes
import qualified Name as GHC

-- | Creates a simple, unqualified name
mkName :: String -> Name
mkName = mkNormalName . mkSimpleName

mkQualOp :: [String] -> String -> Operator
mkQualOp quals = mkAnn child . UNormalOp . mkQualifiedName quals

mkBacktickOp :: [String] -> String -> Operator
mkBacktickOp quals = mkAnn ("`" <> child <> "`") . UBacktickOp . mkQualifiedName quals

-- | Creates an annotated qualified operator: @A.B.+@ or @\`A.B.mod\`@.
mkQualOp' :: [String] -> GHC.Name -> Operator
mkQualOp' quals n | GHC.isSymOcc (GHC.getOccName n) = mkAnn child $ UNormalOp $ mkQualifiedName' quals n
                  | otherwise                       = mkAnn ("`" <> child <> "`") $ UBacktickOp $ mkQualifiedName' quals n

-- | Creates an annotated unqualified operator: @+@ or @\`mod\`@.
mkUnqualOp' :: GHC.Name -> Operator
mkUnqualOp' n | GHC.isSymOcc (GHC.getOccName n) = mkAnn child $ UNormalOp $ mkSimpleName' n
              | otherwise                       = mkAnn ("`" <> child <> "`") $ UBacktickOp $ mkSimpleName' n
  
mkUnqualOp :: String -> Operator
mkUnqualOp = mkAnn child . UNormalOp . mkSimpleName

-- | Creates an annotated qualified (non-operator) binding name: @A.B.f@ or @(A.B.+)@
mkQualName' :: [String] -> GHC.Name -> Name
mkQualName' quals n | GHC.isSymOcc (GHC.getOccName n) = mkAnn ("(" <> child <> ")") $ UParenName $ mkQualifiedName' quals n
                    | otherwise                       = mkAnn child $ UNormalName $ mkQualifiedName' quals n

-- | Creates an annotated unqualified (non-operator) binding name: @f@ or @(+)@
mkUnqualName' :: GHC.Name -> Name
mkUnqualName' n | GHC.isSymOcc (GHC.getOccName n) = mkAnn ("(" <> child <> ")") $ UParenName $ mkSimpleName' n
                | otherwise                       = mkAnn child $ UNormalName $ mkSimpleName' n

mkNormalName :: QualifiedName -> Name
mkNormalName = mkAnn child . UNormalName

-- | Creates a parenthesized name: @ foldl (+) 0 @
mkParenName :: QualifiedName -> Name
mkParenName = mkAnn ("(" <> child <> ")") . UParenName

-- | Creates an implicit name: @ ?var @
mkImplicitName :: QualifiedName -> Name
mkImplicitName = mkAnn ("?" <> child) . UImplicitName

-- | Creates an annotated qualified simple name
mkQualifiedName' :: [String] -> GHC.Name -> QualifiedName
mkQualifiedName' quals n = mkQualifiedName quals (GHC.occNameString $ GHC.getOccName n)

mkQualifiedName :: [String] -> String -> QualifiedName
mkQualifiedName [] n = mkSimpleName n
mkQualifiedName quals name
  = mkAnn (child <> "." <> child)
          (UQualifiedName (mkAnnList (separatedBy "." list) $ map mkNamePart quals) (mkNamePart name))

-- | Creates a part of a qualified name.         
mkNamePart :: String -> NamePart
mkNamePart s = mkAnn (fromString s) (UNamePart s)

-- | Creates a simple (unqualified) name
mkSimpleName' :: GHC.Name -> QualifiedName
mkSimpleName' = mkSimpleName . GHC.occNameString . GHC.getOccName

-- | Creates a simple (unqualified) name
mkSimpleName :: String -> QualifiedName
mkSimpleName n = mkAnn (child <> child) 
                       (UQualifiedName emptyList (mkNamePart n))

-- | Creates a quoted text
mkStringNode :: String -> StringNode
mkStringNode s = mkAnn (fromString s) (UStringNode s)
