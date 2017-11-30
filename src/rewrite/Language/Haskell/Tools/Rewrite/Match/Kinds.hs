-- | UPattern matching on statement-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.Rewrite.Match.Kinds where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite.ElementTypes

-- | Kind constraint (@ :: * -> * @)
pattern KindConstraint :: Kind -> KindConstraint
pattern KindConstraint k <- Ann _ (UKindConstraint k)

-- | @*@, the kind of types
pattern StarKind :: Kind
pattern StarKind <- Ann _ UStarKind

-- | @#@, the kind of unboxed types
pattern UnboxKind :: Kind
pattern UnboxKind <- Ann _ UUnboxKind

-- | @->@, the kind of type constructor
pattern FunKind :: Kind -> Kind -> Kind
pattern FunKind a r <- Ann _ (UFunKind a r)

-- | A parenthesised kind
pattern ParenKind :: Kind -> Kind
pattern ParenKind k <- Ann _ (UParenKind k)

-- | Kind variable (using @PolyKinds@ extension)
pattern VarKind :: Name -> Kind
pattern VarKind v <- Ann _ (UVarKind v)

-- | Kind application (@ k1 k2 @)
pattern AppKind :: Kind -> Kind -> Kind
pattern AppKind f a <- Ann _ (UAppKind f a)

-- | A list kind (@ [k] @)
pattern ListKind :: Kind -> Kind
pattern ListKind k <- Ann _ (UListKind k)

-- | Numeric value promoted to the kind level.
pattern IntKind :: Integer -> Kind
pattern IntKind i <- Ann _ (UPromotedKind (Ann _ (UPromotedInt i)))

-- | String value promoted to the kind level.
pattern StringKind :: String -> Kind
pattern StringKind s <- Ann _ (UPromotedKind (Ann _ (UPromotedString s)))

-- | A data constructor value promoted to the kind level.
pattern ConKind :: Name -> Kind
pattern ConKind s <- Ann _ (UPromotedKind (Ann _ (UPromotedCon s)))

-- | A list of elements as a kind.
pattern ListKindPromoted :: KindList -> Kind
pattern ListKindPromoted elems <- Ann _ (UPromotedKind (Ann _ (UPromotedList elems)))

-- | A tuple of elements as a kind.
pattern TupleKind :: KindList -> Kind
pattern TupleKind elems <- Ann _ (UPromotedKind (Ann _ (UPromotedTuple elems)))

-- | Kind of the unit value @()@. 
pattern UnitKind :: Kind
pattern UnitKind <- Ann _ (UPromotedKind (Ann _ UPromotedUnit))
