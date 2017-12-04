-- | UPattern matching expression-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.Rewrite.Match.Exprs where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite.ElementTypes
import Language.Haskell.Tools.Rewrite.Match.Stmts

-- * Expressions

-- | An expression for a variable or a data constructor (@ a @)
pattern Var :: Name -> Expr
pattern Var name <- Ann _ (UVar name)

-- | A literal expression (@ 42 @)
pattern Lit :: Literal -> Expr
pattern Lit lit <- Ann _ (ULit lit)

-- | An infix operator application (@ a + b @)
pattern InfixApp :: Expr -> Operator -> Expr -> Expr
pattern InfixApp lhs op rhs <- Ann _ (UInfixApp lhs op rhs)

-- | Prefix operator application (@ -x @)
pattern PrefixApp :: Operator -> Expr -> Expr
pattern PrefixApp op rhs <- Ann _ (UPrefixApp op rhs)

-- | Function application (@ f 4 @)
pattern App :: Expr -> Expr -> Expr
pattern App f e <- Ann _ (UApp f e)

-- | Lambda expression (@ \\a b -> a + b @)
pattern Lambda :: PatternList -> Expr -> Expr
pattern Lambda pats rhs <- Ann _ (ULambda pats rhs)

-- | Local binding (@ let x = 2; y = 3 in e x y @)
pattern Let :: LocalBindList -> Expr -> Expr
pattern Let pats expr <- Ann _ (ULet pats expr)

-- | If expression (@ if a then b else c @)
pattern If :: Expr -> Expr -> Expr -> Expr
pattern If cond then_ else_ <- Ann _ (UIf cond then_ else_)

-- | Multi way if expressions with @MultiWayIf@ extension (@ if | guard1 -> expr1; guard2 -> expr2 @)
pattern MultiIf :: GuardedCaseRhsList -> Expr
pattern MultiIf cases <- Ann _ (UMultiIf cases)

-- | Pattern matching expression (@ case expr of pat1 -> expr1; pat2 -> expr2 @)
pattern Case :: Expr -> AltList -> Expr
pattern Case expr cases <- Ann _ (UCase expr cases)

-- | Do-notation expressions (@ do x <- act1; act2 @)
pattern Do :: StmtList -> Expr
pattern Do stmts <- Ann _ (UDo DoKeyword stmts)

pattern ParArrayComp :: Expr -> ListCompBodyList -> Expr
pattern ParArrayComp expr stmts <- Ann _ (UParArrayComp expr stmts)

-- | Tuple expression (@ (e1, e2, e3) @)
pattern Tuple :: ExprList -> Expr
pattern Tuple exprs <- Ann _ (UTuple exprs)

-- | Unboxed tuple expression (@ (\# e1, e2, e3 \#) @)
pattern UnboxedTuple :: ExprList -> Expr
pattern UnboxedTuple exprs <- Ann _ (UUnboxedTuple exprs)

-- | Tuple section, enabled with @TupleSections@ (@ (a,,b) @). One of the elements must be missing.
pattern TupleSection :: TupSecElemList -> Expr
pattern TupleSection elems <- Ann _ (UTupleSection elems)

-- | Unboxed tuple section enabled with @TupleSections@ (@ (# a,,b #) @). One of the elements must be missing.
pattern UnboxedTupleSection :: TupSecElemList -> Expr
pattern UnboxedTupleSection elems <- Ann _ (UUnboxedTupSec elems)

-- | List expression: @[1,2,3]@
pattern List :: ExprList -> Expr
pattern List exprs <- Ann _ (UList exprs)

-- | Parallel array expression: @[: 1,2,3 :]@
pattern ParArray :: ExprList -> Expr
pattern ParArray exprs <- Ann _ (UParArray exprs)

-- | Parenthesized expression: @( a + b )@
pattern Paren :: Expr -> Expr
pattern Paren expr <- Ann _ (UParen expr)

-- | Left operator section: @(1+)@
pattern LeftSection :: Expr -> Operator -> Expr
pattern LeftSection lhs op <- Ann _ (ULeftSection lhs op)

-- | Right operator section: @(+1)@
pattern RightSection :: Operator -> Expr -> Expr
pattern RightSection op lhs <- Ann _ (URightSection op lhs)

-- | Record value construction: @Point { x = 3, y = -2 }@
pattern RecCon :: Name -> FieldUpdateList -> Expr
pattern RecCon name flds <- Ann _ (URecCon name flds)

-- | Record value update: @p1 { x = 3, y = -2 }@
pattern RecUpdate :: Expr -> FieldUpdateList -> Expr
pattern RecUpdate expr flds <- Ann _ (URecUpdate expr flds)

-- | Enumeration expression (@ [1,3..10] @)
pattern Enum :: Expr -> MaybeExpr -> MaybeExpr -> Expr
pattern Enum from step to <- Ann _ (UEnum from step to)

-- | Parallel array enumeration (@ [: 1,3 .. 10 :] @)
pattern ParArrayEnum :: Expr -> MaybeExpr -> Expr -> Expr
pattern ParArrayEnum from step to <- Ann _ (UParArrayEnum from step to)

-- | List comprehension (@ [ (x, y) | x <- xs | y <- ys ] @)
pattern ListComp :: Expr -> ListCompBodyList -> Expr
pattern ListComp expr bodies <- Ann _ (UListComp expr bodies)

-- | Parallel array comprehensions @ [: (x, y) | x <- xs , y <- ys :] @ enabled by @ParallelArrays@
pattern ParArrayListComp :: Expr -> ListCompBodyList -> Expr
pattern ParArrayListComp expr bodies <- Ann _ (UParArrayComp expr bodies)

-- | Explicit type signature (@ x :: Int @)
pattern TypeSig :: Expr -> Type -> Expr
pattern TypeSig lhs typ <- Ann _ (UTypeSig lhs typ)

-- | Explicit type application (@ show \@Integer (read "5") @)
pattern ExplicitTypeApp :: Expr -> Type -> Expr
pattern ExplicitTypeApp lhs typ <- Ann _ (UExplTypeApp lhs typ)

-- | @'x@ for template haskell reifying of expressions
pattern VarQuote :: Name -> Expr
pattern VarQuote name <- Ann _ (UVarQuote name)

-- | @''T@ for template haskell reifying of types
pattern TypeQuote :: Name -> Expr
pattern TypeQuote name <- Ann _ (UTypeQuote name)

-- | Template haskell bracket expression
pattern BracketExpr :: Bracket -> Expr
pattern BracketExpr brack <- Ann _ (UBracketExpr brack)

-- | Template haskell splice expression, for example: @$(gen a)@ or @$x@
pattern SpliceExpr :: Splice -> Expr
pattern SpliceExpr splice <- Ann _ (USplice splice)

-- | Template haskell quasi-quotation: @[$quoter|str]@
pattern QuasiQuoteExpr :: QuasiQuote -> Expr
pattern QuasiQuoteExpr qq <- Ann _ (UQuasiQuoteExpr qq)

-- | Template haskell quasi-quotation: @[$quoter|str]@
pattern ExprPragma :: ExprPragma -> Expr -> Expr
pattern ExprPragma pragma expr <- Ann _ (UExprPragma pragma expr)

-- | Arrow definition: @proc a -> f -< a+1@
pattern Proc :: Pattern -> Cmd -> Expr
pattern Proc pat cmd <- Ann _ (UProc pat cmd)

-- | Arrow definition: @proc a -> f -< a+1@
pattern ArrowApp :: Expr -> ArrowApp -> Expr -> Expr
pattern ArrowApp lhs arrow rhs <- Ann _ (UArrowApp lhs arrow rhs)

-- | Lambda case ( @\case 0 -> 1; 1 -> 2@ )
pattern LambdaCase :: AltList -> Expr
pattern LambdaCase alts <- Ann _ (ULamCase alts)

-- | Static pointer expression (@ static e @). The inner expression must be closed (cannot have variables bound outside)
pattern StaticPointer :: Expr -> Expr
pattern StaticPointer expr <- Ann _ (UStaticPtr expr)


-- * Field updates

-- | Update of a field (@ x = 1 @)
pattern NormalFieldUpdate :: Name -> Expr -> FieldUpdate
pattern NormalFieldUpdate n e <- Ann _ (UNormalFieldUpdate n e)

-- | Update the field to the value of the same name (@ x @)
pattern FieldPun :: Name -> FieldUpdate
pattern FieldPun n <- Ann _ (UFieldPun n)

-- | Update the fields of the bounded names to their values (@ .. @). Must be the last initializer. Cannot be used in a record update expression.
pattern FieldWildcard :: FieldWildcard -> FieldUpdate
pattern FieldWildcard wc <- Ann _ (UFieldWildcard wc)

-- * Tuple sections

-- | An existing element in a tuple section
pattern TupSecPresent :: Expr -> TupSecElem
pattern TupSecPresent expr <- Ann _ (Present expr)

-- | A missing element in a tuple section
pattern TupSecMissing :: TupSecElem
pattern TupSecMissing <- Ann _ Missing

-- * Pattern matching and guards

-- | Clause of case expression (@ Just x -> x + 1 @)
pattern Alt :: Pattern -> CaseRhs -> MaybeLocalBinds -> Alt
pattern Alt pat rhs locals <- Ann _ (UAlt pat rhs locals) 

-- | Unguarded right-hand side a pattern match (@ -> 3 @)
pattern CaseRhs :: Expr -> CaseRhs
pattern CaseRhs e <- Ann _ (UUnguardedCaseRhs e)

-- | Guarded right-hand sides of a pattern match (@ | x == 1 -> 3; | otherwise -> 4 @)
pattern GuardedCaseRhss :: GuardedCaseRhsList -> CaseRhs
pattern GuardedCaseRhss cases <- Ann _ (UGuardedCaseRhss cases)

-- | A guarded right-hand side of pattern matches binding (@ | x > 3 -> 2 @)      
pattern GuardedCaseRhs :: RhsGuardList -> Expr -> GuardedCaseRhs
pattern GuardedCaseRhs guards expr <- Ann _ (UGuardedCaseRhs guards expr)

-- * Pragmas that can be applied to expressions

-- | A @CORE@ pragma for adding notes to expressions.
pattern CorePragma :: String -> ExprPragma
pattern CorePragma str <- Ann _ (UCorePragma (Ann _ (UStringNode str)))

-- | An @SCC@ pragma for defining cost centers for profiling
pattern SccPragma :: String -> ExprPragma
pattern SccPragma str <- Ann _ (USccPragma (Ann _ (UStringNode str)))

-- | A pragma that describes if an expression was generated from a code fragment by an external tool (@ {-# GENERATED "Happy.y" 1:15-1:25 #-} @)
pattern GeneratedPragma :: SourceRange -> ExprPragma
pattern GeneratedPragma rng <- Ann _ (UGeneratedPragma rng)


-- | In-AST source ranges (for generated pragmas)
pattern SourceRange :: String -> Integer -> Integer -> Integer -> Integer -> SourceRange
pattern SourceRange file fromLine fromCol toLine toCol 
          <- Ann _ (USourceRange 
                     (Ann _ (UStringNode file))
                     (Ann _ (Number fromLine))
                     (Ann _ (Number fromCol))
                     (Ann _ (Number toLine))
                     (Ann _ (Number toCol)))
-- * Commands

-- | An arrow application command (@ f -< x + 1 @)
pattern ArrowAppCmd :: Expr -> ArrowApp -> Expr -> Cmd
pattern ArrowAppCmd lhs arrow rhs <- Ann _ (UArrowAppCmd lhs arrow rhs)

-- | A form command (@ (|untilA (increment -< x+y) (within 0.5 -< x)|) @)
pattern ArrowFormCmd :: Expr -> CmdList -> Cmd
pattern ArrowFormCmd expr cmds <- Ann _ (UArrowFormCmd expr cmds)

-- | A function application command
pattern AppCmd :: Cmd -> Expr -> Cmd
pattern AppCmd cmd expr <- Ann _ (UAppCmd cmd expr)

-- | An infix command application
pattern InfixCmd :: Cmd -> Name -> Cmd -> Cmd
pattern InfixCmd lhs op rhs <- Ann _ (UInfixCmd lhs op rhs)

-- | An infix command application
pattern LambdaCmd :: PatternList -> Cmd -> Cmd
pattern LambdaCmd pats cmd <- Ann _ (ULambdaCmd pats cmd)

-- | A parenthesized command
pattern ParenCmd :: Cmd -> Cmd
pattern ParenCmd cmd <- Ann _ (UParenCmd cmd)

-- | A pattern match command
pattern CaseCmd :: Expr -> CmdAltList -> Cmd
pattern CaseCmd expr alts <- Ann _ (UCaseCmd expr alts)

-- | An if command (@ if f x y then g -< x+1 else h -< y+2 @)
pattern IfCmd :: Expr -> Cmd -> Cmd -> Cmd
pattern IfCmd pred then_ else_ <- Ann _ (UIfCmd pred then_ else_)

-- | A local binding command (@ let z = x+y @)
pattern LetCmd :: LocalBindList -> Cmd -> Cmd
pattern LetCmd locals cmd <- Ann _ (ULetCmd locals cmd)

-- | A local binding command (@ let z = x+y @)
pattern DoCmd :: CmdStmtList -> Cmd
pattern DoCmd stmts <- Ann _ (UDoCmd stmts)


-- | Left arrow application: @-<@
pattern LeftAppl :: ArrowApp
pattern LeftAppl <- Ann _ ULeftAppl 

-- | Right arrow application: @>-@
pattern RightAppl :: ArrowApp
pattern RightAppl <- Ann _ URightAppl 

-- | Left arrow high application: @-<<@
pattern LeftHighApp :: ArrowApp
pattern LeftHighApp <- Ann _ ULeftHighApp 

-- | Right arrow high application: @>>-@
pattern RightHighApp :: ArrowApp
pattern RightHighApp <- Ann _ URightHighApp 

-- | A hole expression @_@
pattern Hole :: Expr
pattern Hole <- Ann _ UHole



