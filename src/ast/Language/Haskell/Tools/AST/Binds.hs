-- | Representation of Haskell AST value and function bindings (both local and top-level)
module Language.Haskell.Tools.AST.Binds where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Patterns
import Language.Haskell.Tools.AST.Exprs
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Literals
import {-# SOURCE #-} Language.Haskell.Tools.AST.TH

-- | Value binding for top-level and local bindings
data ValueBind a
  = SimpleBind { _valBindPat :: Ann Pattern a
               , _valBindRhs :: Ann Rhs a  
               , _valBindLocals :: AnnMaybe LocalBinds a
               } -- ^ Non-function binding (@ v = "12" @)  
  -- TODO: use one name for a function instead of names in each match
  | FunBind    { _funBindMatches :: AnnList Match a 
               } -- ^ Function binding (@ f 0 = 1; f x = x @). All matches must have the same name.

-- | Clause of function (or value) binding   
data Match a
  = Match { _matchName :: Ann Name a
          , _matchArgs :: AnnList Pattern a
          , _matchRhs :: Ann Rhs a
          , _matchBinds :: AnnMaybe LocalBinds a
          } 
    
-- | Local bindings attached to a declaration (@ where x = 42 @)             
data LocalBinds a
  = LocalBinds { _localBinds :: AnnList LocalBind a 
               }
  
-- | Bindings that are enabled in local blocks (where or let).
data LocalBind a 
  = LocalValBind   { _localVal :: Ann ValueBind a 
                   }
  -- TODO: check that no other signature can be inside a local binding
  | LocalSignature { _localSig :: Ann TypeSignature a 
                   }
  | LocalFixity    { _localFixity :: Ann FixitySignature a
                   }
                   
-- | A type signature (@ _f :: Int -> Int @)
data TypeSignature a 
  = TypeSignature { _tsName :: Ann Name a
                  , _tsType :: Ann Type a
                  }     
                   
-- | A fixity signature (@ infixl 5 +, - @).
data FixitySignature a 
  = FixitySignature { _fixityAssoc :: Ann Assoc a
                    , _fixityPrecedence :: Ann Precedence a
                    , _fixityOperators :: AnnList Name a
                    }
   
-- | Right hand side of a value binding (possible with guards): (@ = 3 @ or @ | x == 1 = 3; | otherwise = 4 @)
data Rhs a
  = UnguardedRhs { _rhsExpr :: Ann Expr a
                 }
  | GuardedRhss  { _rhsGuards :: AnnList GuardedRhs a
                 }
      
-- | A guarded right-hand side of a value binding (@ | x > 3 = 2 @)      
data GuardedRhs a
  = GuardedRhs { _guardStmts :: AnnList RhsGuard a -- ^ Cannot be empty.
               , _guardExpr :: Ann Expr a
               } 

-- | Guards for value bindings and pattern matches (@ Just v <- x, v > 1 @)
data RhsGuard a
  = GuardBind  { _guardPat :: Ann Pattern a
               , _guardRhs :: Ann Expr a
               }
  | GuardLet   { _guardBinds :: AnnList LocalBind a 
               }
  | GuardCheck { _guardCheck :: Ann Expr a 
               }

-- * Pragmas

-- | Top level pragmas
data TopLevelPragma a
  = RulePragma    { _pragmaRule :: AnnList Rule a 
                  }
  | DeprPragma    { _pragmaObjects :: AnnList Name a
                  , _pragmaMessage :: Ann StringNode a
                  }
  | WarningPragma { _pragmaObjects :: AnnList Name a
                  , _pragmaMessage :: Ann StringNode a
                  }
  | AnnPragma     { _pragmaAnnotation :: Ann Annotation a 
                  }
  | MinimalPragma { _pragmaFormula :: AnnMaybe MinimalFormula a 
                  }
 
-- | A rewrite rule (@ "map/map" forall f g xs. map f (map g xs) = map (f.g) xs @)
data Rule a
  = Rule { _ruleName :: Ann StringNode a -- ^ User name of the rule
         , _rulePhase :: AnnMaybe PhaseControl a
         , _ruleBounded :: AnnList Name a
         , _ruleTopLevel :: Ann Name a
         , _ruleApplied :: AnnList Expr a
         , _ruleRhs :: Ann Expr a
         }
 
-- | Annotation allows you to connect an expression to any declaration. 
data Annotation a
  = NameAnnotation   { _annotateType :: AnnMaybe TypeKeyword a
                     , _annotateName :: Ann Name a
                     , _annotateExpr :: Ann Expr a
                     }
  | ModuleAnnotation { _annotateExpr :: Ann Expr a 
                     }

-- | Formulas of minimal annotations declaring which functions should be defined.
data MinimalFormula a
  = MinimalName  { _minimalName :: Ann Name a 
                 }
  | MinimalParen { _minimalInner :: Ann MinimalFormula a 
                 }
  | MinimalOr    { _minimalLhs :: Ann MinimalFormula a
                 , _minimalRhs :: Ann MinimalFormula a
                 } -- ^ One of the minimal formulas are needed (@ min1 | min2 @)
  | MinimalAnd   { _minimalLhs :: Ann MinimalFormula a
                 , _minimalRhs :: Ann MinimalFormula a
                 } -- ^ Both of the minimal formulas are needed (@ min1 , min2 @)
