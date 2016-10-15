-- | Pattern matching on pattern-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Patterns where

import Language.Haskell.Tools.AST

pattern VarPat :: Ann Name dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
pattern VarPat var <- Ann _ (UVarPat var)

pattern LitPat :: Ann Literal dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
pattern LitPat lit <- Ann _ (ULitPat lit)

pattern InfixAppPat :: Ann Pattern dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
pattern InfixAppPat lhs op rhs <- Ann _ (UInfixAppPat lhs op rhs)

pattern AppPat :: Ann Name dom SrcTemplateStage -> AnnList Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
pattern AppPat n pat <- Ann _ (UAppPat n pat)

pattern TuplePat :: AnnList Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
pattern TuplePat pats <- Ann _ (UTuplePat pats)

pattern UnboxTuplePat :: AnnList Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
pattern UnboxTuplePat pats <- Ann _ (UUnboxTuplePat pats)

pattern ListPat :: AnnList Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
pattern ListPat pats <- Ann _ (UListPat pats)

pattern ParenPat :: Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
pattern ParenPat pat <- Ann _ (UParenPat pat)

pattern RecPat :: Ann Name dom SrcTemplateStage -> AnnList PatternField dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
pattern RecPat name flds <- Ann _ (URecPat name flds)

pattern AsPat :: Ann Name dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
pattern AsPat name pat <- Ann _ (UAsPat name pat)

pattern WildPat :: Ann Pattern dom SrcTemplateStage
pattern WildPat <- Ann _ UWildPat

pattern IrrefutablePat :: Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
pattern IrrefutablePat pat <- Ann _ (UIrrefutablePat pat)

pattern BangPat :: Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
pattern BangPat pat <- Ann _ (UBangPat pat)

pattern TypeSigPat :: Ann Pattern dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
pattern TypeSigPat pat typ <- Ann _ (UTypeSigPat pat typ)

pattern ViewPat :: Ann Expr dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
pattern ViewPat name pat <- Ann _ (UViewPat name pat)

pattern PatternField :: Ann Name dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage -> Ann PatternField dom SrcTemplateStage
pattern PatternField name pat <- Ann _ (UNormalFieldPattern name pat)
