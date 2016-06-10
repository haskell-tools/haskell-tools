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
  = Match { _matchLhs :: Ann MatchLhs a
          , _matchRhs :: Ann Rhs a
          , _matchBinds :: AnnMaybe LocalBinds a
          } 

-- | Something on the left side of the match
data MatchLhs a 
  = NormalLhs { _matchLhsName :: Ann Name a
              , _matchLhsArgs :: AnnList Pattern a
              }
  | InfixLhs { _matchLhsLhs :: Ann Pattern a
             , _matchLhsOperator :: Ann Operator a
             , _matchLhsRhs :: Ann Pattern a
             , _matchLhsArgs :: AnnList Pattern a
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
  = TypeSignature { _tsName :: AnnList Name a
                  , _tsType :: Ann Type a
                  }     
                   
-- | A fixity signature (@ infixl 5 +, - @).
data FixitySignature a 
  = FixitySignature { _fixityAssoc :: Ann Assoc a
                    , _fixityPrecedence :: Ann Precedence a
                    , _fixityOperators :: AnnList Operator a
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
  = RulePragma       { _pragmaRule :: AnnList Rule a 
                     }
  | DeprPragma       { _pragmaObjects :: AnnList Name a
                     , _pragmaMessage :: Ann StringNode a
                     }
  | WarningPragma    { _pragmaObjects :: AnnList Name a
                     , _pragmaMessage :: Ann StringNode a
                     }
  | AnnPragma        { _annotationSubject :: Ann AnnotationSubject a 
                     , _annotateExpr :: Ann Expr a
                     }
  | InlinePragma     { _pragmaPhase :: AnnMaybe PhaseControl a
                     , _pragmaConlike :: AnnMaybe ConlikeAnnot a
                     , _inlineDef :: Ann Name a 
                     }
  | NoInlinePragma   { _pragmaPhase :: AnnMaybe PhaseControl a
                     , _pragmaConlike :: AnnMaybe ConlikeAnnot a
                     , _noInlineDef :: Ann Name a 
                     }
  | InlinablePragma  { _pragmaPhase :: AnnMaybe PhaseControl a
                     , _inlinableDef :: Ann Name a 
                     }
  | LinePragma       { _pragmaLineNum :: Ann LineNumber a
                     , _pragmaFileName :: AnnMaybe StringNode a 
                     }
  | SpecializePragma { _pragmaPhase :: AnnMaybe PhaseControl a
                     , _specializeDef :: Ann Name a 
                     , _specializeType :: AnnList Type a 
                     }

-- | A rewrite rule (@ "map/map" forall f g xs. map f (map g xs) = map (f.g) xs @)
data Rule a
  = Rule { _ruleName :: Ann StringNode a -- ^ User name of the rule
         , _rulePhase :: AnnMaybe PhaseControl a -- ^ The compilation phases in which the rule can be applied
         , _ruleBounded :: AnnList TyVar a -- ^ Variables bound in the rule
         , _ruleLhs :: Ann Expr a -- ^ The transformed expression
         , _ruleRhs :: Ann Expr a -- ^ The resulting expression
         }
 
-- | Annotation allows you to connect an expression to any declaration. 
data AnnotationSubject a
  = NameAnnotation { _annotateName :: Ann Name a
                   } -- ^ The definition with the given name is annotated
  | TypeAnnotation { _annotateName :: Ann Name a 
                   } -- ^ A type with the given name is annotated
  | ModuleAnnotation -- ^ The whole module is annotated

-- | Formulas of minimal annotations declaring which functions should be defined.
data MinimalFormula a
  = MinimalName  { _minimalName :: Ann Name a 
                 }
  | MinimalParen { _minimalInner :: Ann MinimalFormula a 
                 }
  | MinimalOr    { _minimalOrs :: AnnList MinimalFormula a
                 } -- ^ One of the minimal formulas are needed (@ min1 | min2 @)
  | MinimalAnd   { _minimalAnds :: AnnList MinimalFormula a
                 } -- ^ Both of the minimal formulas are needed (@ min1 , min2 @)
