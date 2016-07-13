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
data ValueBind dom stage
  = SimpleBind { _valBindPat :: Ann Pattern dom stage
               , _valBindRhs :: Ann Rhs dom stage
               , _valBindLocals :: AnnMaybe LocalBinds dom stage
               } -- ^ Non-function binding (@ v = "12" @)  
  -- TODO: use one name for a function instead of names in each match
  | FunBind    { _funBindMatches :: AnnList Match dom stage
               } -- ^ Function binding (@ f 0 = 1; f x = x @). All matches must have the same name.

-- | Clause of function (or value) binding   
data Match dom stage
  = Match { _matchLhs :: Ann MatchLhs dom stage
          , _matchRhs :: Ann Rhs dom stage
          , _matchBinds :: AnnMaybe LocalBinds dom stage
          } 

-- | Something on the left side of the match
data MatchLhs dom stage
  = NormalLhs { _matchLhsName :: Ann Name dom stage
              , _matchLhsArgs :: AnnList Pattern dom stage
              }
  | InfixLhs { _matchLhsLhs :: Ann Pattern dom stage
             , _matchLhsOperator :: Ann Operator dom stage
             , _matchLhsRhs :: Ann Pattern dom stage
             , _matchLhsArgs :: AnnList Pattern dom stage
             }
    
-- | Local bindings attached to a declaration (@ where x = 42 @)             
data LocalBinds dom stage
  = LocalBinds { _localBinds :: AnnList LocalBind dom stage
               }
  
-- | Bindings that are enabled in local blocks (where or let).
data LocalBind dom stage
  = LocalValBind   { _localVal :: Ann ValueBind dom stage
                   }
  -- TODO: check that no other signature can be inside a local binding
  | LocalSignature { _localSig :: Ann TypeSignature dom stage
                   }
  | LocalFixity    { _localFixity :: Ann FixitySignature dom stage
                   }
                   
-- | A type signature (@ _f :: Int -> Int @)
data TypeSignature dom stage
  = TypeSignature { _tsName :: AnnList Name dom stage
                  , _tsType :: Ann Type dom stage
                  }     
                   
-- | A fixity signature (@ infixl 5 +, - @).
data FixitySignature dom stage
  = FixitySignature { _fixityAssoc :: Ann Assoc dom stage
                    , _fixityPrecedence :: Ann Precedence dom stage
                    , _fixityOperators :: AnnList Operator dom stage
                    }
   
-- | Right hand side of a value binding (possible with guards): (@ = 3 @ or @ | x == 1 = 3; | otherwise = 4 @)
data Rhs dom stage
  = UnguardedRhs { _rhsExpr :: Ann Expr dom stage
                 }
  | GuardedRhss  { _rhsGuards :: AnnList GuardedRhs dom stage
                 }
      
-- | A guarded right-hand side of a value binding (@ | x > 3 = 2 @)      
data GuardedRhs dom stage
  = GuardedRhs { _guardStmts :: AnnList RhsGuard dom stage -- ^ Cannot be empty.
               , _guardExpr :: Ann Expr dom stage
               } 

-- | Guards for value bindings and pattern matches (@ Just v <- x, v > 1 @)
data RhsGuard dom stage
  = GuardBind  { _guardPat :: Ann Pattern dom stage
               , _guardRhs :: Ann Expr dom stage
               }
  | GuardLet   { _guardBinds :: AnnList LocalBind dom stage
               }
  | GuardCheck { _guardCheck :: Ann Expr dom stage
               }

-- * Pragmas

-- | Top level pragmas
data TopLevelPragma dom stage
  = RulePragma       { _pragmaRule :: AnnList Rule dom stage
                     }
  | DeprPragma       { _pragmaObjects :: AnnList Name dom stage
                     , _pragmaMessage :: Ann StringNode dom stage
                     }
  | WarningPragma    { _pragmaObjects :: AnnList Name dom stage
                     , _pragmaMessage :: Ann StringNode dom stage
                     }
  | AnnPragma        { _annotationSubject :: Ann AnnotationSubject dom stage
                     , _annotateExpr :: Ann Expr dom stage
                     }
  | InlinePragma     { _pragmaConlike :: AnnMaybe ConlikeAnnot dom stage
                     , _pragmaPhase :: AnnMaybe PhaseControl dom stage
                     , _inlineDef :: Ann Name dom stage
                     }
  | NoInlinePragma   { _pragmaConlike :: AnnMaybe ConlikeAnnot dom stage
                     , _pragmaPhase :: AnnMaybe PhaseControl dom stage
                     , _noInlineDef :: Ann Name dom stage
                     }
  | InlinablePragma  { _pragmaPhase :: AnnMaybe PhaseControl dom stage
                     , _inlinableDef :: Ann Name dom stage
                     }
  | LinePragma       { _pragmaLineNum :: Ann LineNumber dom stage
                     , _pragmaFileName :: AnnMaybe StringNode dom stage
                     }
  | SpecializePragma { _pragmaPhase :: AnnMaybe PhaseControl dom stage
                     , _specializeDef :: Ann Name dom stage
                     , _specializeType :: AnnList Type dom stage
                     }

-- | A rewrite rule (@ "map/map" forall f g xs. map f (map g xs) = map (f.g) xs @)
data Rule dom stage
  = Rule { _ruleName :: Ann StringNode dom stage -- ^ User name of the rule
         , _rulePhase :: AnnMaybe PhaseControl dom stage -- ^ The compilation phases in which the rule can be applied
         , _ruleBounded :: AnnList TyVar dom stage -- ^ Variables bound in the rule
         , _ruleLhs :: Ann Expr dom stage -- ^ The transformed expression
         , _ruleRhs :: Ann Expr dom stage -- ^ The resulting expression
         }
 
-- | Annotation allows you to connect an expression to any declaration. 
data AnnotationSubject dom stage
  = NameAnnotation { _annotateName :: Ann Name dom stage
                   } -- ^ The definition with the given name is annotated
  | TypeAnnotation { _annotateName :: Ann Name dom stage
                   } -- ^ A type with the given name is annotated
  | ModuleAnnotation -- ^ The whole module is annotated

-- | Formulas of minimal annotations declaring which functions should be defined.
data MinimalFormula dom stage
  = MinimalName  { _minimalName :: Ann Name dom stage
                 }
  | MinimalParen { _minimalInner :: Ann MinimalFormula dom stage
                 }
  | MinimalOr    { _minimalOrs :: AnnList MinimalFormula dom stage
                 } -- ^ One of the minimal formulas are needed (@ min1 | min2 @)
  | MinimalAnd   { _minimalAnds :: AnnList MinimalFormula dom stage
                 } -- ^ Both of the minimal formulas are needed (@ min1 , min2 @)
