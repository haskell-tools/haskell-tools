-- | Pattern matching expression-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Exprs where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Match.Base

-- * Expressions

pattern Var :: Ann Name dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern Var name <- Ann _ (UVar name)

pattern Lit :: Ann Literal dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern Lit lit <- Ann _ (ULit lit)

pattern InfixApp :: Ann Expr dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern InfixApp lhs op rhs <- Ann _ (UInfixApp lhs op rhs)

pattern PrefixApp :: Ann Operator dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern PrefixApp op rhs <- Ann _ (UPrefixApp op rhs)

pattern App :: Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern App f e <- Ann _ (UApp f e)

pattern Lambda :: AnnList Pattern dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern Lambda pats rhs <- Ann _ (ULambda pats rhs)

pattern Let :: AnnList LocalBind dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern Let pats expr <- Ann _ (ULet pats expr)

pattern If :: Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern If cond then_ else_ <- Ann _ (UIf cond then_ else_)

pattern MultiIf :: AnnList GuardedCaseRhs dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern MultiIf cases <- Ann _ (UMultiIf cases)

pattern Case :: Ann Expr dom SrcTemplateStage -> AnnList Alt dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern Case expr cases <- Ann _ (UCase expr cases)

pattern Do :: AnnList Stmt dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern Do stmts <- Ann _ (UDo DoKeyword stmts)

pattern ListComp :: Ann Expr dom SrcTemplateStage -> AnnList ListCompBody dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern ListComp expr stmts <- Ann _ (UListComp expr stmts)

pattern ParArrayComp :: Ann Expr dom SrcTemplateStage -> AnnList ListCompBody dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern ParArrayComp expr stmts <- Ann _ (UParArrayComp expr stmts)

pattern Tuple :: AnnList Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern Tuple exprs <-  Ann _ (UTuple exprs)

pattern UnboxedTuple :: AnnList Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern UnboxedTuple exprs <-  Ann _ (UUnboxedTuple exprs)

pattern List :: AnnList Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern List exprs <-  Ann _ (UList exprs)

pattern ParArray :: AnnList Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern ParArray exprs <-  Ann _ (UParArray exprs)

pattern Paren :: Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern Paren expr <- Ann _ (UParen expr)

pattern LeftSection :: Ann Expr dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern LeftSection lhs op <- Ann _ (ULeftSection lhs op)

pattern RightSection :: Ann Operator dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern RightSection op lhs <- Ann _ (URightSection op lhs)

pattern RecCon :: Ann Name dom SrcTemplateStage -> AnnList FieldUpdate dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern RecCon name flds <- Ann _ (URecCon name flds)

pattern RecUpdate :: Ann Expr dom SrcTemplateStage -> AnnList FieldUpdate dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern RecUpdate expr flds <- Ann _ (URecUpdate expr flds)

pattern Enum :: Ann Expr dom SrcTemplateStage -> AnnMaybe Expr dom SrcTemplateStage -> AnnMaybe Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern Enum from step to <- Ann _ (UEnum from step to)

pattern ParArrayEnum :: Ann Expr dom SrcTemplateStage -> AnnMaybe Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern ParArrayEnum from step to <- Ann _ (UParArrayEnum from step to)

pattern TypeSig :: Ann Expr dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern TypeSig lhs typ <- Ann _ (UTypeSig lhs typ)

pattern BracketExpr :: Ann Bracket dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern BracketExpr brack <- Ann _ (UBracketExpr brack)

pattern Splice :: Ann Splice dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern Splice splice <- Ann _ (USplice splice)

pattern QuasiQuoteExpr :: Ann QuasiQuote dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
pattern QuasiQuoteExpr qq <- Ann _ (UQuasiQuoteExpr qq)

-- * Field updates

pattern NormalFieldUpdate :: Ann Name dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann FieldUpdate dom SrcTemplateStage
pattern NormalFieldUpdate n e <- Ann _ (UNormalFieldUpdate n e)

pattern FieldPun :: Ann Name dom SrcTemplateStage -> Ann FieldUpdate dom SrcTemplateStage
pattern FieldPun n <- Ann _ (UFieldPun n)

pattern FieldWildcard :: Ann FieldWildcard dom SrcTemplateStage -> Ann FieldUpdate dom SrcTemplateStage
pattern FieldWildcard wc <- Ann _ (UFieldWildcard wc)

-- * Pattern matching and guards

pattern Alt :: Ann Pattern dom SrcTemplateStage -> Ann CaseRhs dom SrcTemplateStage -> AnnMaybe LocalBinds dom SrcTemplateStage -> Ann Alt dom SrcTemplateStage
pattern Alt pat rhs locals <- Ann _ (UAlt pat rhs locals) 

pattern CaseRhs :: Ann Expr dom SrcTemplateStage -> Ann CaseRhs dom SrcTemplateStage
pattern CaseRhs e <- Ann _ (UUnguardedCaseRhs e)

pattern GuardedCaseRhss :: AnnList GuardedCaseRhs dom SrcTemplateStage -> Ann CaseRhs dom SrcTemplateStage
pattern GuardedCaseRhss cases <- Ann _ (UGuardedCaseRhss cases)

pattern GuardedCaseRhs :: AnnList RhsGuard dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann GuardedCaseRhs dom SrcTemplateStage
pattern GuardedCaseRhs guards expr <- Ann _ (UGuardedCaseRhs guards expr)

