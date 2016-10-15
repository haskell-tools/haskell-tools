-- | Pattern matching on statement-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Kinds where

import Language.Haskell.Tools.AST

pattern KindConstraint :: Ann Kind dom SrcTemplateStage -> Ann KindConstraint dom SrcTemplateStage
pattern KindConstraint k <- Ann _ (UKindConstraint k)

pattern StarKind :: Ann Kind dom SrcTemplateStage
pattern StarKind <- Ann _ UStarKind

pattern UnboxKind :: Ann Kind dom SrcTemplateStage
pattern UnboxKind <- Ann _ UUnboxKind

pattern FunKind :: Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
pattern FunKind a r <- Ann _ (UFunKind a r)

pattern ParenKind :: Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
pattern ParenKind k <- Ann _ (UParenKind k)

pattern VarKind :: Ann Name dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
pattern VarKind v <- Ann _ (UVarKind v)

pattern AppKind :: Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
pattern AppKind f a <- Ann _ (UAppKind f a)

pattern ListKind :: Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
pattern ListKind k <- Ann _ (UListKind k)

pattern IntKind :: Integer -> Ann Kind dom SrcTemplateStage
pattern IntKind i <- Ann _ (UPromotedKind (Ann _ (UPromotedInt i)))

pattern StringKind :: String -> Ann Kind dom SrcTemplateStage
pattern StringKind s <- Ann _ (UPromotedKind (Ann _ (UPromotedString s)))

pattern ConKind :: Ann Name dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
pattern ConKind s <- Ann _ (UPromotedKind (Ann _ (UPromotedCon s)))

pattern ListKindPromoted :: AnnList Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
pattern ListKindPromoted elems <- Ann _ (UPromotedKind (Ann _ (UPromotedList elems)))

pattern TupleKind :: AnnList Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
pattern TupleKind elems <- Ann _ (UPromotedKind (Ann _ (UPromotedTuple elems)))

pattern UnitKind :: Ann Kind dom SrcTemplateStage
pattern UnitKind <- Ann _ (UPromotedKind (Ann _ UPromotedUnit))
