-- | Representation of Haskell expressions
module Language.Haskell.Tools.AST.Exprs where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Patterns
import Language.Haskell.Tools.AST.Stmts
import {-# SOURCE #-} Language.Haskell.Tools.AST.TH
import {-# SOURCE #-} Language.Haskell.Tools.AST.Binds (LocalBind, LocalBinds, RhsGuard)

-- | Haskell expressions
data Expr a
  = Var            { _exprName :: Ann Name a 
                   } -- ^ A variable or a data constructor (@ a @)
  | Lit            { _exprLit :: Ann Literal a
                   } -- ^ Primitive literal
  | InfixApp       { _exprLhs :: Ann Expr a
                   , _exprOperator :: Ann Name a
                   , _exprRhs :: Ann Expr a
                   } -- ^ Infix operator application (@ a + b @)
  | PrefixApp      { _exprOperator :: Ann Name a
                   , _exprRhs :: Ann Expr a
                   } -- ^ Prefix operator application (@ -x @)
  | App            { _exprFun :: Ann Expr a
                   , _exprArg :: Ann Expr a
                   } -- ^ Function application (@ f 4 @)
                   -- unary minus omitted
  | Lambda         { _exprBindings :: AnnList Pattern a -- ^ at least one
                   , _exprInner :: Ann Expr a
                   } -- ^ Lambda expression (@ \a b -> a + b @)
  | Let            { _exprFunBind :: AnnList LocalBind a -- ^ nonempty
                   , _exprInner :: Ann Expr a
                   } -- ^ Local binding (@ let x = 2; y = 3 in e x y @)
  | If             { _exprCond :: Ann Expr a
                   , _exprThen :: Ann Expr a
                   , _exprElse :: Ann Expr a
                   } -- ^ If expression (@ if a then b else c @)
  | MultiIf        { _exprIfAlts :: AnnList GuardedCaseRhs a 
                   } -- ^ Multi way if expressions with @MultiWayIf@ extension (@ if | guard1 -> expr1; guard2 -> expr2 @)
  | Case           { _exprCase :: Ann Expr a
                   , _exprAlts :: AnnList Alt a
                   } -- ^ Pattern matching expression (@ case expr of pat1 -> expr1; pat2 -> expr2 @)
  | Do             { _doKind :: Ann DoKind a
                   , _exprStmts :: AnnList Stmt a
                   } -- ^ Do-notation expressions (@ do x <- act1; act2 @)
  | Tuple          { _tupleElems :: AnnList Expr a
                   } -- ^ Tuple expression (@ (e1, e2, e3) @)
  | UnboxedTuple   { _tupleElems :: AnnList Expr a 
                   } -- ^ Unboxed tuple expression (@ (# e1, e2, e3 #) @)
  | TupleSection   { _tupleSectionElems :: AnnList TupSecElem a
                   } -- ^ Tuple section, enabled with @TupleSections@ (@ (a,,b) @). One of the elements must be missing.
  | UnboxedTupSec  { _tupleSectionElems :: AnnList TupSecElem a 
                   }
  | List           { _listElems :: AnnList Expr a 
                   } -- ^ List expression: @[1,2,3]@
  | ParArray       { _listElems :: AnnList Expr a 
                   } -- ^ Parallel array expression: @[: 1,2,3 :]@
  | Paren          { _exprInner :: Ann Expr a 
                   }
  | LeftSection    { _exprLhs :: Ann Expr a
                   , _exprOperator :: Ann Name a
                   } -- ^ Left operator section: @(1+)@
  | RightSection   { _exprOperator :: Ann Name a
                   , _exprRhs :: Ann Expr a
                   } -- ^ Right operator section: @(+1)@
  | RecCon         { _exprRecName :: Ann Name a
                   , _exprRecFields :: AnnList FieldUpdate a
                   } -- ^ Record value construction: @Point { x = 3, y = -2 }@
  | RecUpdate      { _exprRecBase :: Ann Expr a
                   , _exprRecFields :: AnnList FieldUpdate a
                   } -- ^ Record value  update: @p1 { x = 3, y = -2 }@
  | Enum           { _enumFrom :: Ann Expr a
                   , _enumThen :: AnnMaybe Expr a
                   , _enumTo :: AnnMaybe Expr a
                   } -- ^ Enumeration expression (@ [1,3..10] @)
  | ParArrayEnum   { _parEnumFrom :: Ann Expr a
                   , _parEnumThen :: AnnMaybe Expr a
                   , _parEnumTo :: Ann Expr a
                   } -- ^ Parallel array enumeration (@ [: 1,3 .. 10 :] @)
  | ListComp       { _compExpr :: Ann Expr a
                   , _compBody :: AnnList ListCompBody a -- ^ Can only have 1 element without @ParallelListComp@
                   } -- ^ List comprehension (@ [ (x, y) | x <- xs | y <- ys ] @)
  | ParArrayComp   { _compExpr :: Ann Expr a
                   , _parCompBody :: AnnList ListCompBody a
                   } -- ^ Parallel array comprehensions @ [: (x, y) | x <- xs , y <- ys :] @ enabled by @ParallelArrays@
  | TypeSig        { _exprInner :: Ann Expr a
                   , _exprSig :: Ann Type a
                   } -- ^ Explicit type signature (@ _x :: Int @)
  | VarQuote       { _quotedName :: Ann Name a 
                   } -- ^ @'x@ for template haskell reifying of expressions
  | TypeQuote      { _quotedName :: Ann Name a 
                   } -- ^ @''T@ for template haskell reifying of types
  | BracketExpr    { _bracket :: Ann Bracket a 
                   } -- ^ Template haskell bracket expression
  | Splice         { _innerExpr :: Ann Splice a 
                   } -- ^ Template haskell splice expression, for example: @$(gen a)@ or @$x@
  | QuasiQuoteExpr { _exprQQ :: Ann QuasiQuote a 
                   } -- ^ Template haskell quasi-quotation: @[$quoter|str]@
  | ExprPragma     { _exprPragma :: Ann ExprPragma a
                   }
  -- Arrows
  | Proc           { _procPattern :: Ann Pattern a
                   , _procExpr :: Ann Cmd a
                   } -- ^ Arrow definition: @proc a -> f -< a+1@
  | ArrowApp       { _exprLhs :: Ann Expr a
                   , _arrowAppl :: Ann ArrowAppl a
                   , _exprRhs :: Ann Expr a
                   } -- ^ Arrow application: @f -< a+1@
  | LamCase        { _exprAlts :: AnnList Alt a
                   } -- ^ Lambda case ( @\case 0 -> 1; 1 -> 2@ )
  | StaticPtr      { _exprStatic :: Ann Expr a
                   } -- ^ Static pointer expression (@ static e @). The inner expression must be closed (cannot have variables bound outside)
  -- XML expressions omitted
                   
-- | Field update expressions
data FieldUpdate a 
  = NormalFieldUpdate { _fieldName :: Ann Name a
                      , _fieldValue :: Ann Expr a
                      } -- ^ Update of a field (@ x = 1 @)
  | FieldPun          { _fieldUpdateName :: Ann Name a 
                      } -- ^ Update the field to the value of the same name (@ x @)
  | FieldWildcard     -- ^ Update the fields of the bounded names to their values (@ .. @). Must be the last update. Cannot be used in a record update expression.
      
-- | An element of a tuple section that can be an expression or missing (indicating a value from a parameter)
data TupSecElem a
  = Present { _tupSecExpr :: Ann Expr a 
            } -- ^ An existing element in a tuple section
  | Missing -- ^ A missing element in a tuple section
  
-- | Clause of case expression          
data Alt' expr a
  = Alt { _altPattern :: Ann Pattern a
        , _altRhs :: Ann (CaseRhs' expr) a
        , _altBinds :: AnnMaybe LocalBinds a
        }
type Alt = Alt' Expr
type CmdAlt = Alt' Cmd

  
-- | Right hand side of a match (possible with guards): (@ = 3 @ or @ | x == 1 = 3; | otherwise = 4 @)
data CaseRhs' expr a
  = UnguardedCaseRhs { _rhsCaseExpr :: Ann expr a 
                     }
  | GuardedCaseRhss  { _rhsCaseGuards :: AnnList (GuardedCaseRhs' expr) a 
                     }
type CaseRhs = CaseRhs' Expr
type CmdCaseRhs = CaseRhs' Cmd
                     
-- | A guarded right-hand side of pattern matches binding (@ | x > 3 -> 2 @)      
data GuardedCaseRhs' expr a
  = GuardedCaseRhs { _caseGuardStmts :: AnnList RhsGuard a -- ^ Cannot be empty.
                   , _caseGuardExpr :: Ann expr a
                   } 
type GuardedCaseRhs = GuardedCaseRhs' Expr
type CmdGuardedCaseRhs = GuardedCaseRhs' Cmd
               
-- | Pragmas that can be applied to expressions
data ExprPragma a
  = CorePragma      { _pragmaStr :: Ann StringNode a 
                    }
  | SccPragma       { _pragmaStr :: Ann StringNode a 
                    }
  | GeneratedPragma { _pragmaSrcRange :: Ann SourceRange a 
                    }

-- | In-AST source ranges (for generated pragmas)
data SourceRange a
  = SourceRange { _srFileName :: Ann StringNode a
                , _srFromLine :: Ann Number a
                , _srFromCol :: Ann Number a
                , _srToLine :: Ann Number a
                , _srToCol :: Ann Number a
                }  
                
data Number a 
  = Number { _numberInteger :: Integer 
           }
        
data Cmd a
  = ArrowAppCmd   { _cmdLhs :: Ann Expr a
                  , _cmdArrowOp :: Ann ArrowAppl a
                  , _cmdRhs :: Ann Expr a
                  }
  | ArrowFormCmd  { _cmdExpr :: Ann Expr a
                  , _cmdInnerCmds :: AnnList Cmd a
                  }
  | AppCmd        { _cmdInnerCmd :: Ann Cmd a
                  , _cmdApplied :: Ann Expr a
                  }
  | InfixCmd      { _cmdLeftCmd :: Ann Cmd a
                  , _cmdOperator :: Ann Name a
                  , _cmdRightCmd :: Ann Cmd a
                  }
  | LambdaCmd     { _cmdBindings :: AnnList Pattern a -- ^ at least one
                  , _cmdInner :: Ann Cmd a
                  }
  | ParenCmd      { _cmdInner :: Ann Cmd a
                  }
  | CaseCmd       { _cmdExpr :: Ann Expr a 
                  , _cmdAlts :: AnnList CmdAlt a
                  }
  | IfCmd         { _cmdExpr :: Ann Expr a 
                  , _cmdThen :: Ann Cmd a
                  , _cmdElse :: Ann Cmd a
                  }
  | LetCmd        { _cmdBinds :: AnnList LocalBind a -- ^ nonempty
                  , _cmdInner :: Ann Cmd a
                  }
  | DoCmd         { _cmdStmts :: AnnList CmdStmt a
                  }
 
