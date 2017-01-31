-- | Representation of Haskell AST value and function bindings (both local and top-level)
module Language.Haskell.Tools.AST.Representation.Binds where

import Language.Haskell.Tools.AST.Ann (Ann, AnnListG, AnnMaybeG)
import Language.Haskell.Tools.AST.Representation.Exprs (UExpr)
import Language.Haskell.Tools.AST.Representation.Names (UName, UOperator)
import Language.Haskell.Tools.AST.Representation.Patterns (UPattern)
import Language.Haskell.Tools.AST.Representation.Types (UType)

-- | Value binding for top-level and local bindings
data UValueBind dom stage
  = USimpleBind { _valBindPat :: Ann UPattern dom stage
                , _valBindRhs :: Ann URhs dom stage
                , _valBindLocals :: AnnMaybeG ULocalBinds dom stage
                } -- ^ Non-function binding (@ v = "12" @)  
  -- TODO: use one name for a function instead of names in each match
  | UFunBind    { _funBindMatches :: AnnListG UMatch dom stage
                } -- ^ Function binding (@ f 0 = 1; f x = x @). All matches must have the same name.

-- | Clause of function binding   
data UMatch dom stage
  = UMatch { _matchLhs :: Ann UMatchLhs dom stage
           , _matchRhs :: Ann URhs dom stage
           , _matchBinds :: AnnMaybeG ULocalBinds dom stage
           } 

-- | Something on the left side of the match
data UMatchLhs dom stage
  = UNormalLhs { _matchLhsName :: Ann UName dom stage
               , _matchLhsArgs :: AnnListG UPattern dom stage
               } -- ^ A match lhs with the function name and parameter names (@ f a b @)
  | UInfixLhs { _matchLhsLhs :: Ann UPattern dom stage
              , _matchLhsOperator :: Ann UOperator dom stage
              , _matchLhsRhs :: Ann UPattern dom stage
              , _matchLhsArgs :: AnnListG UPattern dom stage
              } -- ^ An infix match lhs for an operator (@ a + b @)
    
-- | Local bindings attached to a declaration (@ where x = 42 @)             
data ULocalBinds dom stage
  = ULocalBinds { _localBinds :: AnnListG ULocalBind dom stage
                }
  
-- | Bindings that are enabled in local blocks (where or let).
data ULocalBind dom stage
  -- TODO: check that no other signature can be inside a local binding
  = ULocalValBind   { _localVal :: Ann UValueBind dom stage
                    } -- ^ A local binding for a value
  | ULocalSignature { _localSig :: Ann UTypeSignature dom stage
                    } -- ^ A local type signature
  | ULocalFixity    { _localFixity :: Ann UFixitySignature dom stage
                    } -- ^ A local fixity declaration
  | ULocalInline    { _localInline :: Ann UInlinePragma dom stage
                    } -- ^ A local inline pragma
                   
-- | A type signature (@ f :: Int -> Int @)
data UTypeSignature dom stage
  = UTypeSignature { _tsName :: AnnListG UName dom stage
                   , _tsType :: Ann UType dom stage
                   }     
            
-- * Fixities

-- | A fixity signature (@ infixl 5 +, - @).
data UFixitySignature dom stage
  = UFixitySignature { _fixityAssoc :: Ann Assoc dom stage
                     , _fixityPrecedence :: AnnMaybeG Precedence dom stage
                     , _fixityOperators :: AnnListG UOperator dom stage
                     }

-- | Associativity of an operator.
data Assoc dom stage
  = AssocNone  -- ^ non-associative operator (declared with @infix@)
  | AssocLeft  -- ^ left-associative operator (declared with @infixl@)
  | AssocRight -- ^ right-associative operator (declared with @infixr@)
   
-- | Numeric precedence of an operator
data Precedence dom stage
  = Precedence { _precedenceValue :: Int } 

-- | Right hand side of a value binding (possible with guards): (@ = 3 @ or @ | x == 1 = 3; | otherwise = 4 @)
data URhs dom stage
  = UUnguardedRhs { _rhsExpr :: Ann UExpr dom stage
                  } -- ^ An unguarded right-hand-side (@ = 3 @)
  | UGuardedRhss  { _rhsGuards :: AnnListG UGuardedRhs dom stage
                  } -- ^ An unguarded right-hand-side (@ | x == 1 = 3; | otherwise = 4 @)
      
-- | A guarded right-hand side of a value binding (@ | x > 3 = 2 @)      
data UGuardedRhs dom stage
  = UGuardedRhs { _guardStmts :: AnnListG URhsGuard dom stage -- ^ Cannot be empty.
                , _guardExpr :: Ann UExpr dom stage
                } 

-- | Guards for value bindings and pattern matches (@ Just v <- x, v > 1 @)
data URhsGuard dom stage
  = UGuardBind  { _guardPat :: Ann UPattern dom stage
                , _guardRhs :: Ann UExpr dom stage
                } -- ^ A bind statement in a pattern guard (@ Just v <- x @)
  | UGuardLet   { _guardBinds :: AnnListG ULocalBind dom stage
                } -- ^ A let statement in a pattern guard (@ let x = 3 @)
  | UGuardCheck { _guardCheck :: Ann UExpr dom stage
                } -- ^ An expression to check for a pattern guard

-- | Pragmas that control how the definitions will be inlined
data UInlinePragma dom stage
  = UInlinePragma     { _inlineConlike :: AnnMaybeG UConlikeAnnot dom stage
                      , _inlinePhase :: AnnMaybeG UPhaseControl dom stage
                      , _inlineDef :: Ann UName dom stage
                      } -- ^ A pragma that marks a function for inlining to the compiler (@ {-# INLINE thenUs #-} @)
  | UNoInlinePragma   { _noInlineDef :: Ann UName dom stage
                      } -- ^ A pragma that forbids a function from being inlined by the compiler (@ {-# NOINLINE f #-} @)
  | UInlinablePragma  { _inlinePhase :: AnnMaybeG UPhaseControl dom stage
                      , _inlinableDef :: Ann UName dom stage
                      } -- ^ A pragma that marks a function that it may be inlined by the compiler (@ {-# INLINABLE thenUs #-} @)

-- | A @CONLIKE@ modifier for an @INLINE@ pragma.
data UConlikeAnnot dom stage = UConlikeAnnot

-- | Controls the activation of a rewrite rule (@ [1] @)
data UPhaseControl dom stage
  = UPhaseControl { _phaseUntil :: AnnMaybeG PhaseInvert dom stage
                  , _phaseNumber :: Ann PhaseNumber dom stage
                  } 

-- | Phase number for rewrite rules
data PhaseNumber dom stage
  = PhaseNumber { _phaseNum :: Integer }

-- | A tilde that marks the inversion of the phase number
data PhaseInvert dom stage = PhaseInvert