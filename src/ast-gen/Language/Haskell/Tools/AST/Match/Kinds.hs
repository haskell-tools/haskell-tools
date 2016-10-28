-- | UPattern matching on statement-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Kinds where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

-- | Kind constraint (@ :: * -> * @)
pattern KindConstraint :: Kind dom -> KindConstraint dom
pattern KindConstraint k <- Ann _ (UKindConstraint k)

-- | @*@, the kind of types
pattern StarKind :: Kind dom
pattern StarKind <- Ann _ UStarKind

-- | @#@, the kind of unboxed types
pattern UnboxKind :: Kind dom
pattern UnboxKind <- Ann _ UUnboxKind

-- | @->@, the kind of type constructor
pattern FunKind :: Kind dom -> Kind dom -> Kind dom
pattern FunKind a r <- Ann _ (UFunKind a r)

-- | A parenthesised kind
pattern ParenKind :: Kind dom -> Kind dom
pattern ParenKind k <- Ann _ (UParenKind k)

-- | Kind variable (using @PolyKinds@ extension)
pattern VarKind :: Name dom -> Kind dom
pattern VarKind v <- Ann _ (UVarKind v)

-- | Kind application (@ k1 k2 @)
pattern AppKind :: Kind dom -> Kind dom -> Kind dom
pattern AppKind f a <- Ann _ (UAppKind f a)

-- | A list kind (@ [k] @)
pattern ListKind :: Kind dom -> Kind dom
pattern ListKind k <- Ann _ (UListKind k)

-- | Numeric value promoted to the kind level.
pattern IntKind :: Integer -> Kind dom
pattern IntKind i <- Ann _ (UPromotedKind (Ann _ (UPromotedInt i)))

-- | String value promoted to the kind level.
pattern StringKind :: String -> Kind dom
pattern StringKind s <- Ann _ (UPromotedKind (Ann _ (UPromotedString s)))

-- | A data constructor value promoted to the kind level.
pattern ConKind :: Name dom -> Kind dom
pattern ConKind s <- Ann _ (UPromotedKind (Ann _ (UPromotedCon s)))

-- | A list of elements as a kind.
pattern ListKindPromoted :: KindList dom -> Kind dom
pattern ListKindPromoted elems <- Ann _ (UPromotedKind (Ann _ (UPromotedList elems)))

-- | A tuple of elements as a kind.
pattern TupleKind :: KindList dom -> Kind dom
pattern TupleKind elems <- Ann _ (UPromotedKind (Ann _ (UPromotedTuple elems)))

-- | Kind of the unit value @()@. 
pattern UnitKind :: Kind dom
pattern UnitKind <- Ann _ (UPromotedKind (Ann _ UPromotedUnit))
