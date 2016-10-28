-- | Representation of Haskell expressions
module Language.Haskell.Tools.AST.Representation.Exprs where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Representation.Names
import Language.Haskell.Tools.AST.Representation.Literals
import Language.Haskell.Tools.AST.Representation.Types
import Language.Haskell.Tools.AST.Representation.Patterns
import Language.Haskell.Tools.AST.Representation.Stmts
import {-# SOURCE #-} Language.Haskell.Tools.AST.Representation.TH
import {-# SOURCE #-} Language.Haskell.Tools.AST.Representation.Binds (ULocalBind, ULocalBinds, URhsGuard)

-- | Haskell expressions
data UExpr dom stage
  = UVar            { _exprName :: Ann UName dom stage 
                    } -- ^ A variable or a data constructor (@ a @)
  | ULit            { _exprLit :: Ann ULiteral dom stage
                    } -- ^ Literal expression (@ 42 @)
  | UInfixApp       { _exprLhs :: Ann UExpr dom stage
                    , _exprOperator :: Ann UOperator dom stage
                    , _exprRhs :: Ann UExpr dom stage
                    } -- ^ An infix operator application (@ a + b @)
  | UPrefixApp      { _exprOperator :: Ann UOperator dom stage
                    , _exprRhs :: Ann UExpr dom stage
                    } -- ^ Prefix operator application (@ -x @)
  | UApp            { _exprFun :: Ann UExpr dom stage
                    , _exprArg :: Ann UExpr dom stage
                    } -- ^ Function application (@ f 4 @)
  | ULambda         { _exprBindings :: AnnListG UPattern dom stage -- ^ at least one
                    , _exprInner :: Ann UExpr dom stage
                    } -- ^ Lambda expression (@ \a b -> a + b @)
  | ULet            { _exprFunBind :: AnnListG ULocalBind dom stage -- ^ nonempty
                    , _exprInner :: Ann UExpr dom stage
                    } -- ^ Local binding (@ let x = 2; y = 3 in e x y @)
  | UIf             { _exprCond :: Ann UExpr dom stage
                    , _exprThen :: Ann UExpr dom stage
                    , _exprElse :: Ann UExpr dom stage
                    } -- ^ If expression (@ if a then b else c @)
  | UMultiIf        { _exprIfAlts :: AnnListG UGuardedCaseRhs dom stage
                    } -- ^ Multi way if expressions with @MultiWayIf@ extension (@ if | guard1 -> expr1; guard2 -> expr2 @)
  | UCase           { _exprCase :: Ann UExpr dom stage
                    , _exprAlts :: AnnListG UAlt dom stage
                    } -- ^ Pattern matching expression (@ case expr of pat1 -> expr1; pat2 -> expr2 @)
  | UDo             { _doKind :: Ann UDoKind dom stage
                    , _exprStmts :: AnnListG UStmt dom stage
                    } -- ^ Do-notation expressions (@ do x <- act1; act2 @)
  | UTuple          { _tupleElems :: AnnListG UExpr dom stage
                    } -- ^ Tuple expression (@ (e1, e2, e3) @)
  | UUnboxedTuple   { _tupleElems :: AnnListG UExpr dom stage
                    } -- ^ Unboxed tuple expression (@ (# e1, e2, e3 #) @)
  | UTupleSection   { _tupleSectionElems :: AnnListG UTupSecElem dom stage
                    } -- ^ Tuple section, enabled with @TupleSections@ (@ (a,,b) @). One of the elements must be missing.
  | UUnboxedTupSec  { _tupleSectionElems :: AnnListG UTupSecElem dom stage
                    } -- ^ Unboxed tuple section enabled with @TupleSections@ (@ (# a,,b #) @). One of the elements must be missing.
  | UList           { _listElems :: AnnListG UExpr dom stage
                    } -- ^ List expression: @[1,2,3]@
  | UParArray       { _listElems :: AnnListG UExpr dom stage
                    } -- ^ Parallel array expression: @[: 1,2,3 :]@
  | UParen          { _exprInner :: Ann UExpr dom stage
                    } -- ^ Parenthesized expression: @( a + b )@
  | ULeftSection    { _exprLhs :: Ann UExpr dom stage
                    , _exprOperator :: Ann UOperator dom stage
                    } -- ^ Left operator section: @(1+)@
  | URightSection   { _exprOperator :: Ann UOperator dom stage
                    , _exprRhs :: Ann UExpr dom stage
                    } -- ^ Right operator section: @(+1)@
  | URecCon         { _exprRecName :: Ann UName dom stage
                    , _exprRecFields :: AnnListG UFieldUpdate dom stage
                    } -- ^ Record value construction: @Point { x = 3, y = -2 }@
  | URecUpdate      { _exprInner :: Ann UExpr dom stage
                    , _exprRecFields :: AnnListG UFieldUpdate dom stage
                    } -- ^ Record value update: @p1 { x = 3, y = -2 }@
  | UEnum           { _enumFrom :: Ann UExpr dom stage
                    , _enumThen :: AnnMaybeG UExpr dom stage
                    , _enumTo :: AnnMaybeG UExpr dom stage
                    } -- ^ Enumeration expression (@ [1,3..10] @)
  | UParArrayEnum   { _enumFrom :: Ann UExpr dom stage
                    , _enumThen :: AnnMaybeG UExpr dom stage
                    , _enumToFix :: Ann UExpr dom stage
                    } -- ^ Parallel array enumeration (@ [: 1,3 .. 10 :] @)
  | UListComp       { _compExpr :: Ann UExpr dom stage
                    , _compBody :: AnnListG UListCompBody dom stage -- ^ Can only have 1 element without @ParallelListComp@
                    } -- ^ List comprehension (@ [ (x, y) | x <- xs | y <- ys ] @)
  | UParArrayComp   { _compExpr :: Ann UExpr dom stage
                    , _compBody :: AnnListG UListCompBody dom stage
                    } -- ^ Parallel array comprehensions @ [: (x, y) | x <- xs , y <- ys :] @ enabled by @ParallelArrays@
  | UTypeSig        { _exprInner :: Ann UExpr dom stage
                    , _exprSig :: Ann UType dom stage
                    } -- ^ Explicit type signature (@ x :: Int @)
  | UExplTypeApp    { _exprInner :: Ann UExpr dom stage
                    , _exprType :: Ann UType dom stage
                    } -- ^ Explicit type application (@ show \@Integer (read "5") @)
  | UVarQuote       { _quotedName :: Ann UName dom stage
                    } -- ^ @'x@ for template haskell reifying of expressions
  | UTypeQuote      { _quotedName :: Ann UName dom stage
                    } -- ^ @''T@ for template haskell reifying of types
  | UBracketExpr    { _exprBracket :: Ann UBracket dom stage
                    } -- ^ Template haskell bracket expression
  | USplice         { _exprSplice :: Ann USplice dom stage
                    } -- ^ Template haskell splice expression, for example: @$(gen a)@ or @$x@
  | UQuasiQuoteExpr { _exprQQ :: Ann UQuasiQuote dom stage
                    } -- ^ Template haskell quasi-quotation: @[$quoter|str]@
  | UExprPragma     { _exprPragma :: Ann UExprPragma dom stage
                    , _innerExpr :: Ann UExpr dom stage
                    }
  -- Arrows
  | UProc           { _procPattern :: Ann UPattern dom stage
                    , _procExpr :: Ann UCmd dom stage
                    } -- ^ Arrow definition: @proc a -> f -< a+1@
  | UArrowApp       { _exprLhs :: Ann UExpr dom stage
                    , _arrowAppl :: Ann UArrowAppl dom stage
                    , _exprRhs :: Ann UExpr dom stage
                    } -- ^ Arrow application: @f -< a+1@
  | ULamCase        { _exprAlts :: AnnListG UAlt dom stage
                    } -- ^ Lambda case ( @\case 0 -> 1; 1 -> 2@ )
  | UStaticPtr      { _exprInner :: Ann UExpr dom stage
                    } -- ^ Static pointer expression (@ static e @). The inner expression must be closed (cannot have variables bound outside)
  -- XML expressions omitted
                   
-- | Field update expressions
data UFieldUpdate dom stage
  = UNormalFieldUpdate { _fieldName :: Ann UName dom stage
                       , _fieldValue :: Ann UExpr dom stage
                       } -- ^ Update of a field (@ x = 1 @)
  | UFieldPun          { _fieldUpdateName :: Ann UName dom stage
                       } -- ^ Update the field to the value of the same name (@ x @)
  | UFieldWildcard     { _fieldWildcard :: Ann UFieldWildcard dom stage
                       } -- ^ Update the fields of the bounded names to their values (@ .. @). Must be the last initializer. Cannot be used in a record update expression.
      
-- | Marker for a field wildcard. Only needed to attach semantic information in a type-safe way.
data UFieldWildcard dom stage = FldWildcard

-- | An element of a tuple section that can be an expression or missing (indicating a value from a parameter)
data UTupSecElem dom stage
  = Present { _tupSecExpr :: Ann UExpr dom stage
            } -- ^ An existing element in a tuple section
  | Missing -- ^ A missing element in a tuple section
  
-- | Clause of case expression (@ Just x -> x + 1 @)
data UAlt' expr dom stage
  = UAlt { _altPattern :: Ann UPattern dom stage
         , _altRhs :: Ann (UCaseRhs' expr) dom stage
         , _altBinds :: AnnMaybeG ULocalBinds dom stage
         }
type UAlt = UAlt' UExpr
type UCmdAlt = UAlt' UCmd

  
-- | Right hand side of a match (possible with guards): (@ -> 3 @ or @ | x == 1 -> 3; | otherwise -> 4 @)
data UCaseRhs' expr dom stage
  = UUnguardedCaseRhs { _rhsCaseExpr :: Ann expr dom stage
                      } -- ^ Unguarded right-hand side a pattern match (@ -> 3 @)
  | UGuardedCaseRhss  { _rhsCaseGuards :: AnnListG (UGuardedCaseRhs' expr) dom stage
                      } -- ^ Guarded right-hand sides of a pattern match (@ | x == 1 -> 3; | otherwise -> 4 @)
type UCaseRhs = UCaseRhs' UExpr
type UCmdCaseRhs = UCaseRhs' UCmd
                     
-- | A guarded right-hand side of pattern matches binding (@ | x > 3 -> 2 @)      
data UGuardedCaseRhs' expr dom stage
  = UGuardedCaseRhs { _caseGuardStmts :: AnnListG URhsGuard dom stage -- ^ Cannot be empty.
                    , _caseGuardExpr :: Ann expr dom stage
                    } 
type UGuardedCaseRhs = UGuardedCaseRhs' UExpr
type UCmdGuardedCaseRhs = UGuardedCaseRhs' UCmd
               
-- | Pragmas that can be applied to expressions
data UExprPragma dom stage
  = UCorePragma      { _pragmaStr :: Ann UStringNode dom stage
                     } -- ^ A @CORE@ pragma for adding notes to expressions.
  | USccPragma       { _pragmaStr :: Ann UStringNode dom stage
                     } -- ^ An @SCC@ pragma for defining cost centers for profiling
  | UGeneratedPragma { _pragmaSrcRange :: Ann USourceRange dom stage
                     } -- ^ A pragma that describes if an expression was generated from a code fragment by an external tool (@ {-# GENERATED "Happy.y" 1:15-1:25 #-} @)

-- | In-AST source ranges (for generated pragmas)
data USourceRange dom stage
  = USourceRange { _srFileName :: Ann UStringNode dom stage
                 , _srFromLine :: Ann Number dom stage
                 , _srFromCol :: Ann Number dom stage
                 , _srToLine :: Ann Number dom stage
                 , _srToCol :: Ann Number dom stage
                 }  

data Number dom stage
  = Number { _numberInteger :: Integer 
           }
        
-- * Arrows

data UCmd dom stage
  = UArrowAppCmd   { _cmdLhs :: Ann UExpr dom stage
                   , _cmdArrowOp :: Ann UArrowAppl dom stage
                   , _cmdRhs :: Ann UExpr dom stage
                   } -- ^ An arrow application command (@ f -< x + 1 @)
  | UArrowFormCmd  { _cmdExpr :: Ann UExpr dom stage
                   , _cmdInnerCmds :: AnnListG UCmd dom stage
                   } -- ^ A form command (@ (|untilA (increment -< x+y) (within 0.5 -< x)|) @)
  | UAppCmd        { _cmdInnerCmd :: Ann UCmd dom stage
                   , _cmdApplied :: Ann UExpr dom stage
                   } -- ^ A function application command
  | UInfixCmd      { _cmdLeftCmd :: Ann UCmd dom stage
                   , _cmdOperator :: Ann UName dom stage
                   , _cmdRightCmd :: Ann UCmd dom stage
                   } -- ^ An infix command application
  | ULambdaCmd     { _cmdBindings :: AnnListG UPattern dom stage -- ^ at least one
                   , _cmdInner :: Ann UCmd dom stage
                   } -- ^ A lambda command 
  | UParenCmd      { _cmdInner :: Ann UCmd dom stage
                   } -- ^ A parenthesized command
  | UCaseCmd       { _cmdExpr :: Ann UExpr dom stage
                   , _cmdAlts :: AnnListG UCmdAlt dom stage
                   } -- ^ A pattern match command
  | UIfCmd         { _cmdExpr :: Ann UExpr dom stage
                   , _cmdThen :: Ann UCmd dom stage
                   , _cmdElse :: Ann UCmd dom stage
                   } -- ^ An if command (@ if f x y then g -< x+1 else h -< y+2 @)
  | ULetCmd        { _cmdBinds :: AnnListG ULocalBind dom stage -- ^ nonempty
                   , _cmdInner :: Ann UCmd dom stage
                   } -- ^ A local binding command (@ let z = x+y @)
  | UDoCmd         { _cmdStmts :: AnnListG UCmdStmt dom stage
                   } -- ^ A do-notation in a command

data UArrowAppl dom stage
  = ULeftAppl -- ^ Left arrow application: @-<@
  | URightAppl -- ^ Right arrow application: @>-@
  | ULeftHighApp -- ^ Left arrow high application: @-<<@
  | URightHighApp -- ^ Right arrow high application: @>>-@
