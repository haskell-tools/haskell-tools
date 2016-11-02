-- | UPattern matching on binding-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Binds where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

-- | Non-function binding (@ v = "12" @)  
pattern SimpleBind :: Pattern dom -> Rhs dom -> MaybeLocalBinds dom -> ValueBind dom
pattern SimpleBind p r l <- Ann _ (USimpleBind p r l)

-- | Function binding (@ f 0 = 1; f x = x @). All matches must have the same name.
pattern FunctionBind :: MatchList dom -> ValueBind dom
pattern FunctionBind matches <- Ann _ (UFunBind matches)

-- | Clause of function binding 
pattern Match :: MatchLhs dom -> Rhs dom -> MaybeLocalBinds dom -> Match dom
pattern Match lhs rhs locs <- Ann _ (UMatch lhs rhs locs)

-- | A match lhs with the function name and parameter names (@ f a b @)
pattern MatchLhs :: Name dom -> PatternList dom -> MatchLhs dom
pattern MatchLhs n pats <- Ann _ (UNormalLhs n pats)

-- | An infix match lhs for an operator (@ a + b @)
pattern InfixLhs :: Pattern dom -> Operator dom -> Pattern dom -> PatternList dom -> MatchLhs dom
pattern InfixLhs lhs op rhs pats <- Ann _ (UInfixLhs lhs op rhs pats)

-- | Local bindings attached to a declaration (@ where x = 42 @)
pattern LocalBinds :: LocalBindList dom -> LocalBinds dom
pattern LocalBinds binds <- Ann _ (ULocalBinds binds)

-- | A local binding for a value
pattern LocalValBind :: ValueBind dom -> LocalBind dom
pattern LocalValBind bind <- Ann _ (ULocalValBind bind)

-- | A local type signature
pattern LocalTypeSig :: TypeSignature dom -> LocalBind dom
pattern LocalTypeSig typeSig <- Ann _ (ULocalSignature typeSig)

-- | A local fixity declaration
pattern LocalFixity :: FixitySignature dom -> LocalBind dom
pattern LocalFixity fixity <- Ann _ (ULocalFixity fixity)

-- | A type signature (@ f :: Int -> Int @)
pattern TypeSignature :: NameList dom -> Type dom -> TypeSignature dom
pattern TypeSignature n t <- Ann _ (UTypeSignature n t)

-- | A left-associative fixity declaration (@ infixl 5 +, - @).
pattern InfixL :: Int -> OperatorList dom -> FixitySignature dom
pattern InfixL prec op <- Ann _ (UFixitySignature (Ann _ AssocLeft) (Ann _ (Precedence prec)) op)

-- | A right-associative fixity declaration (@ infixr 5 +, - @).
pattern InfixR :: Int -> OperatorList dom -> FixitySignature dom
pattern InfixR prec op <- Ann _ (UFixitySignature (Ann _ AssocRight) (Ann _ (Precedence prec)) op)

-- | A non-associative fixity declaration (@ infix 5 +, - @).
pattern Infix :: Int -> OperatorList dom -> FixitySignature dom
pattern Infix prec op <- Ann _ (UFixitySignature (Ann _ AssocNone) (Ann _ (Precedence prec)) op)

-- | An unguarded right-hand-side (@ = 3 @)
pattern UnguardedRhs :: Expr dom -> Rhs dom
pattern UnguardedRhs expr <- Ann _ (UUnguardedRhs expr)

-- | An unguarded right-hand-side (@ | x == 1 = 3; | otherwise = 4 @)
pattern GuardedRhss :: GuardedRhsList dom -> Rhs dom
pattern GuardedRhss rhss <- Ann _ (UGuardedRhss rhss)

-- | A guarded right-hand side of a value binding (@ | x > 3 = 2 @)    
pattern GuardedRhs :: RhsGuardList dom -> Expr dom -> GuardedRhs dom
pattern GuardedRhs guards expr <- Ann _ (UGuardedRhs guards expr)

-- | A bind statement in a pattern guard (@ Just v <- x @)
pattern GuardBind :: Pattern dom -> Expr dom -> RhsGuard dom
pattern GuardBind pat expr <- Ann _ (UGuardBind pat expr)

-- | A let statement in a pattern guard (@ let x = 3 @)
pattern GuardLet :: LocalBindList dom -> RhsGuard dom
pattern GuardLet binds <- Ann _ (UGuardLet binds)

-- | An expression to check for a pattern guard
pattern GuardCheck :: Expr dom -> RhsGuard dom
pattern GuardCheck expr <- Ann _ (UGuardCheck expr)
