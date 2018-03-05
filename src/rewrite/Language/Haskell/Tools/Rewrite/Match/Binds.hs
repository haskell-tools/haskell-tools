-- | UPattern matching on binding-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}

module Language.Haskell.Tools.Rewrite.Match.Binds where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite.ElementTypes

-- | Non-function binding (@ v = "12" @)
pattern SimpleBind :: Pattern -> Rhs -> MaybeLocalBinds -> ValueBind
pattern SimpleBind p r l <- Ann _ (USimpleBind p r l)

-- | Function binding (@ f 0 = 1; f x = x @). All matches must have the same name.
pattern FunctionBind :: MatchList -> ValueBind
pattern FunctionBind matches <- Ann _ (UFunBind matches)

-- | Clause of function binding
pattern Match :: MatchLhs -> Rhs -> MaybeLocalBinds -> Match
pattern Match lhs rhs locs <- Ann _ (UMatch lhs rhs locs)

-- | A match lhs with the function name and parameter names (@ f a b @)
pattern MatchLhs :: Name -> PatternList -> MatchLhs
pattern MatchLhs n pats <- Ann _ (UNormalLhs n pats)

-- | An infix match lhs for an operator (@ a + b @)
pattern InfixLhs :: Pattern -> Operator -> Pattern -> PatternList -> MatchLhs
pattern InfixLhs lhs op rhs pats <- Ann _ (UInfixLhs lhs op rhs pats)

-- | Local bindings attached to a declaration (@ where x = 42 @)
pattern LocalBinds :: LocalBindList -> LocalBinds
pattern LocalBinds binds <- Ann _ (ULocalBinds binds)

-- | A local binding for a value
pattern LocalValBind :: ValueBind -> LocalBind
pattern LocalValBind bind <- Ann _ (ULocalValBind bind)

-- | A local type signature
pattern LocalTypeSig :: TypeSignature -> LocalBind
pattern LocalTypeSig typeSig <- Ann _ (ULocalSignature typeSig)

-- | A local fixity declaration
pattern LocalFixity :: FixitySignature -> LocalBind
pattern LocalFixity fixity <- Ann _ (ULocalFixity fixity)

-- | A type signature (@ f :: Int -> Int @)
pattern TypeSignature :: NameList -> Type -> TypeSignature
pattern TypeSignature n t <- Ann _ (UTypeSignature n t)

-- | A left-associative fixity declaration (@ infixl 5 +, - @).
pattern InfixL :: OperatorList -> FixitySignature
pattern InfixL op <- Ann _ (UFixitySignature (Ann _ AssocLeft) _ op)

-- | A right-associative fixity declaration (@ infixr 5 +, - @).
pattern InfixR :: OperatorList -> FixitySignature
pattern InfixR op <- Ann _ (UFixitySignature (Ann _ AssocRight) _ op)

-- | A non-associative fixity declaration (@ infix 5 +, - @).
pattern Infix :: OperatorList -> FixitySignature
pattern Infix op <- Ann _ (UFixitySignature (Ann _ AssocNone) _ op)

-- | An unguarded right-hand-side (@ = 3 @)
pattern UnguardedRhs :: Expr -> Rhs
pattern UnguardedRhs expr <- Ann _ (UUnguardedRhs expr)

-- | An unguarded right-hand-side (@ | x == 1 = 3; | otherwise = 4 @)
pattern GuardedRhss :: GuardedRhsList -> Rhs
pattern GuardedRhss rhss <- Ann _ (UGuardedRhss rhss)

-- | A guarded right-hand side of a value binding (@ | x > 3 = 2 @)
pattern GuardedRhs :: RhsGuardList -> Expr -> GuardedRhs
pattern GuardedRhs guards expr <- Ann _ (UGuardedRhs guards expr)

-- | A bind statement in a pattern guard (@ Just v <- x @)
pattern GuardBind :: Pattern -> Expr -> RhsGuard
pattern GuardBind pat expr <- Ann _ (UGuardBind pat expr)

-- | A let statement in a pattern guard (@ let x = 3 @)
pattern GuardLet :: LocalBindList -> RhsGuard
pattern GuardLet binds <- Ann _ (UGuardLet binds)

-- | An expression to check for a pattern guard
pattern GuardCheck :: Expr -> RhsGuard
pattern GuardCheck expr <- Ann _ (UGuardCheck expr)
