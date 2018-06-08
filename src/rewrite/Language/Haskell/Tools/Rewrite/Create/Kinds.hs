-- | Generation of statement-level AST fragments for refactorings.
-- The bindings defined here are the AST constructor names with an "mk" prefix.
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Tools.Rewrite.Create.Kinds where

import Data.String (IsString(..), String)
import Language.Haskell.Tools.AST (UPromoted(..), UKind(..), UKindConstraint(..))
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Rewrite.Create.Utils (mkAnn, mkAnnList)
import Language.Haskell.Tools.Rewrite.ElementTypes (Name, Kind, KindConstraint)

-- | Kind constraint (@ :: * -> * @)
mkKindConstraint :: Kind -> KindConstraint
mkKindConstraint = mkAnn (" :: " <> child) . UKindConstraint

-- | @*@, the kind of types
mkKindStar :: Kind
mkKindStar = mkAnn "*" UStarKind

-- | @#@, the kind of unboxed types
mkKindUnbox :: Kind
mkKindUnbox = mkAnn "#" UUnboxKind

-- | @->@, the kind of type constructor
mkKindFun :: Kind -> Kind -> Kind
mkKindFun lhs rhs = mkAnn (child <> " -> " <> child) $ UFunKind lhs rhs

-- | A parenthesised kind
mkKindParen :: Kind -> Kind
mkKindParen = mkAnn ("(" <> child <> ")") . UParenKind

-- | Kind variable (using @PolyKinds@ extension)
mkKindVar :: Name -> Kind
mkKindVar = mkAnn child . UVarKind

-- | Kind application (@ k1 k2 @)
mkKindApp :: Kind -> Kind -> Kind
mkKindApp lhs rhs = mkAnn (child <> " " <> child) $ UAppKind lhs rhs

-- | A list kind (@ [k] @)
mkKindList :: Kind -> Kind
mkKindList = mkAnn ("[" <> child <> "]") . UListKind

-- | Numeric value promoted to the kind level.
mkIntKind :: Integer -> Kind
mkIntKind i = mkAnn child $ UPromotedKind $ mkAnn (fromString $ show i) (UPromotedInt i)

-- | String value promoted to the kind level.
mkStringKind :: String -> Kind
mkStringKind i = mkAnn child $ UPromotedKind $ mkAnn (fromString $ show i) (UPromotedString i)

-- | A data constructor value promoted to the kind level.
mkConKind :: Name -> Kind
mkConKind = mkAnn child . UPromotedKind . mkAnn child . UPromotedCon

-- | A list of elements as a kind.
mkListKind :: [Kind] -> Kind
mkListKind = mkAnn child . UPromotedKind . mkAnn ("[" <> child <> "]") . UPromotedList . mkAnnList (separatedBy ", " list)

-- | A tuple of elements as a kind.
mkTupleKind :: [Kind] -> Kind
mkTupleKind = mkAnn child . UPromotedKind . mkAnn ("(" <> child <> ")") . UPromotedTuple . mkAnnList (separatedBy ", " list)

-- | Kind of the unit value @()@. 
mkUnitKind :: Kind
mkUnitKind = mkAnn child $ UPromotedKind $ mkAnn "()" UPromotedUnit