-- | Pattern matching on binding-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Binds where

import Language.Haskell.Tools.AST

pattern SimpleBind :: Ann Pattern dom SrcTemplateStage -> Ann Rhs dom SrcTemplateStage 
                        -> AnnMaybe LocalBinds dom SrcTemplateStage -> Ann ValueBind dom SrcTemplateStage
pattern SimpleBind p r l <- Ann _ (USimpleBind p r l)

pattern FunctionBind :: AnnList Match dom SrcTemplateStage -> Ann ValueBind dom SrcTemplateStage
pattern FunctionBind matches <- Ann _ (UFunBind matches)

pattern Match :: Ann MatchLhs dom SrcTemplateStage -> Ann Rhs dom SrcTemplateStage 
                   -> AnnMaybe LocalBinds dom SrcTemplateStage -> Ann Match dom SrcTemplateStage
pattern Match lhs rhs locs <- Ann _ (UMatch lhs rhs locs)

pattern MatchLhs :: Ann Name dom SrcTemplateStage -> AnnList Pattern dom SrcTemplateStage 
                       -> Ann MatchLhs dom SrcTemplateStage
pattern MatchLhs n pats <- Ann _ (UNormalLhs n pats)

pattern InfixLhs :: Ann Pattern dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage 
                      -> Ann Pattern dom SrcTemplateStage -> AnnList Pattern dom SrcTemplateStage
                      -> Ann MatchLhs dom SrcTemplateStage
pattern InfixLhs lhs op rhs pats <- Ann _ (UInfixLhs lhs op rhs pats)

pattern LocalBinds :: AnnList LocalBind dom SrcTemplateStage -> Ann LocalBinds dom SrcTemplateStage
pattern LocalBinds binds <- Ann _ (ULocalBinds binds)

pattern LocalValBind :: Ann ValueBind dom SrcTemplateStage -> Ann LocalBind dom SrcTemplateStage
pattern LocalValBind bind <- Ann _ (ULocalValBind bind)

pattern LocalTypeSig :: Ann TypeSignature dom SrcTemplateStage -> Ann LocalBind dom SrcTemplateStage
pattern LocalTypeSig typeSig <- Ann _ (ULocalSignature typeSig)

pattern LocalFixity :: Ann FixitySignature dom SrcTemplateStage -> Ann LocalBind dom SrcTemplateStage
pattern LocalFixity fixity <- Ann _ (ULocalFixity fixity)

pattern TypeSignature :: AnnList Name dom SrcTemplateStage -> Ann Type dom SrcTemplateStage 
                          -> Ann TypeSignature dom SrcTemplateStage
pattern TypeSignature n t <- Ann _ (UTypeSignature n t)

pattern InfixL :: Ann Precedence dom SrcTemplateStage -> AnnList Operator dom SrcTemplateStage -> Ann FixitySignature dom SrcTemplateStage
pattern InfixL prec op <- Ann _ (UFixitySignature (Ann _ AssocLeft) prec op)

pattern InfixR :: Ann Precedence dom SrcTemplateStage -> AnnList Operator dom SrcTemplateStage -> Ann FixitySignature dom SrcTemplateStage
pattern InfixR prec op <- Ann _ (UFixitySignature (Ann _ AssocRight) prec op)

pattern Infix :: Ann Precedence dom SrcTemplateStage -> AnnList Operator dom SrcTemplateStage -> Ann FixitySignature dom SrcTemplateStage
pattern Infix prec op <- Ann _ (UFixitySignature (Ann _ AssocNone) prec op)

pattern UnguardedRhs :: Ann Expr dom SrcTemplateStage -> Ann Rhs dom SrcTemplateStage
pattern UnguardedRhs expr <- Ann _ (UUnguardedRhs expr)

pattern GuardedRhss :: AnnList GuardedRhs dom SrcTemplateStage -> Ann Rhs dom SrcTemplateStage
pattern GuardedRhss rhss <- Ann _ (UGuardedRhss rhss)

pattern GuardedRhs :: AnnList RhsGuard dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann GuardedRhs dom SrcTemplateStage
pattern GuardedRhs guards expr <- Ann _ (UGuardedRhs guards expr)

pattern GuardBind :: Ann Pattern dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann RhsGuard dom SrcTemplateStage
pattern GuardBind pat expr <- Ann _ (UGuardBind pat expr)

pattern GuardLet :: AnnList LocalBind dom SrcTemplateStage -> Ann RhsGuard dom SrcTemplateStage
pattern GuardLet binds <- Ann _ (UGuardLet binds)

pattern GuardCheck :: Ann Expr dom SrcTemplateStage -> Ann RhsGuard dom SrcTemplateStage
pattern GuardCheck expr <- Ann _ (UGuardCheck expr)
