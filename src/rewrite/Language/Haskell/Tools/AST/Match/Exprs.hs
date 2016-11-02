-- | UPattern matching expression-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Exprs where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Match.Stmts
import Language.Haskell.Tools.AST.Match.Names

-- * Expressions

-- | An expression for a variable or a data constructor (@ a @)
pattern Var :: Name dom -> Expr dom
pattern Var name <- Ann _ (UVar name)

-- | A literal expression (@ 42 @)
pattern Lit :: Literal dom -> Expr dom
pattern Lit lit <- Ann _ (ULit lit)

-- | An infix operator application (@ a + b @)
pattern InfixApp :: Expr dom -> Operator dom -> Expr dom -> Expr dom
pattern InfixApp lhs op rhs <- Ann _ (UInfixApp lhs op rhs)

-- | Prefix operator application (@ -x @)
pattern PrefixApp :: Operator dom -> Expr dom -> Expr dom
pattern PrefixApp op rhs <- Ann _ (UPrefixApp op rhs)

-- | Function application (@ f 4 @)
pattern App :: Expr dom -> Expr dom -> Expr dom
pattern App f e <- Ann _ (UApp f e)

-- | Lambda expression (@ \\a b -> a + b @)
pattern Lambda :: PatternList dom -> Expr dom -> Expr dom
pattern Lambda pats rhs <- Ann _ (ULambda pats rhs)

-- | Local binding (@ let x = 2; y = 3 in e x y @)
pattern Let :: LocalBindList dom -> Expr dom -> Expr dom
pattern Let pats expr <- Ann _ (ULet pats expr)

-- | If expression (@ if a then b else c @)
pattern If :: Expr dom -> Expr dom -> Expr dom -> Expr dom
pattern If cond then_ else_ <- Ann _ (UIf cond then_ else_)

-- | Multi way if expressions with @MultiWayIf@ extension (@ if | guard1 -> expr1; guard2 -> expr2 @)
pattern MultiIf :: GuardedCaseRhsList dom -> Expr dom
pattern MultiIf cases <- Ann _ (UMultiIf cases)

-- | Pattern matching expression (@ case expr of pat1 -> expr1; pat2 -> expr2 @)
pattern Case :: Expr dom -> AltList dom -> Expr dom
pattern Case expr cases <- Ann _ (UCase expr cases)

-- | Do-notation expressions (@ do x <- act1; act2 @)
pattern Do :: StmtList dom -> Expr dom
pattern Do stmts <- Ann _ (UDo DoKeyword stmts)

pattern ParArrayComp :: Expr dom -> ListCompBodyList dom -> Expr dom
pattern ParArrayComp expr stmts <- Ann _ (UParArrayComp expr stmts)

-- | Tuple expression (@ (e1, e2, e3) @)
pattern Tuple :: ExprList dom -> Expr dom
pattern Tuple exprs <- Ann _ (UTuple exprs)

-- | Unboxed tuple expression (@ (\# e1, e2, e3 \#) @)
pattern UnboxedTuple :: ExprList dom -> Expr dom
pattern UnboxedTuple exprs <- Ann _ (UUnboxedTuple exprs)

-- | Tuple section, enabled with @TupleSections@ (@ (a,,b) @). One of the elements must be missing.
pattern TupleSection :: TupSecElemList dom -> Expr dom
pattern TupleSection elems <- Ann _ (UTupleSection elems)

-- | Unboxed tuple section enabled with @TupleSections@ (@ (# a,,b #) @). One of the elements must be missing.
pattern UnboxedTupleSection :: TupSecElemList dom -> Expr dom
pattern UnboxedTupleSection elems <- Ann _ (UUnboxedTupSec elems)

-- | List expression: @[1,2,3]@
pattern List :: ExprList dom -> Expr dom
pattern List exprs <- Ann _ (UList exprs)

-- | Parallel array expression: @[: 1,2,3 :]@
pattern ParArray :: ExprList dom -> Expr dom
pattern ParArray exprs <- Ann _ (UParArray exprs)

-- | Parenthesized expression: @( a + b )@
pattern Paren :: Expr dom -> Expr dom
pattern Paren expr <- Ann _ (UParen expr)

-- | Left operator section: @(1+)@
pattern LeftSection :: Expr dom -> Operator dom -> Expr dom
pattern LeftSection lhs op <- Ann _ (ULeftSection lhs op)

-- | Right operator section: @(+1)@
pattern RightSection :: Operator dom -> Expr dom -> Expr dom
pattern RightSection op lhs <- Ann _ (URightSection op lhs)

-- | Record value construction: @Point { x = 3, y = -2 }@
pattern RecCon :: Name dom -> FieldUpdateList dom -> Expr dom
pattern RecCon name flds <- Ann _ (URecCon name flds)

-- | Record value update: @p1 { x = 3, y = -2 }@
pattern RecUpdate :: Expr dom -> FieldUpdateList dom -> Expr dom
pattern RecUpdate expr flds <- Ann _ (URecUpdate expr flds)

-- | Enumeration expression (@ [1,3..10] @)
pattern Enum :: Expr dom -> MaybeExpr dom -> MaybeExpr dom -> Expr dom
pattern Enum from step to <- Ann _ (UEnum from step to)

-- | Parallel array enumeration (@ [: 1,3 .. 10 :] @)
pattern ParArrayEnum :: Expr dom -> MaybeExpr dom -> Expr dom -> Expr dom
pattern ParArrayEnum from step to <- Ann _ (UParArrayEnum from step to)

-- | List comprehension (@ [ (x, y) | x <- xs | y <- ys ] @)
pattern ListComp :: Expr dom -> ListCompBodyList dom -> Expr dom
pattern ListComp expr bodies <- Ann _ (UListComp expr bodies)

-- | Parallel array comprehensions @ [: (x, y) | x <- xs , y <- ys :] @ enabled by @ParallelArrays@
pattern ParArrayListComp :: Expr dom -> ListCompBodyList dom -> Expr dom
pattern ParArrayListComp expr bodies <- Ann _ (UParArrayComp expr bodies)

-- | Explicit type signature (@ x :: Int @)
pattern TypeSig :: Expr dom -> Type dom -> Expr dom
pattern TypeSig lhs typ <- Ann _ (UTypeSig lhs typ)

-- | Explicit type application (@ show \@Integer (read "5") @)
pattern ExplicitTypeApp :: Expr dom -> Type dom -> Expr dom
pattern ExplicitTypeApp lhs typ <- Ann _ (UExplTypeApp lhs typ)

-- | @'x@ for template haskell reifying of expressions
pattern VarQuote :: Name dom -> Expr dom
pattern VarQuote name <- Ann _ (UVarQuote name)

-- | @''T@ for template haskell reifying of types
pattern TypeQuote :: Name dom -> Expr dom
pattern TypeQuote name <- Ann _ (UTypeQuote name)

-- | Template haskell bracket expression
pattern BracketExpr :: Bracket dom -> Expr dom
pattern BracketExpr brack <- Ann _ (UBracketExpr brack)

-- | Template haskell splice expression, for example: @$(gen a)@ or @$x@
pattern SpliceExpr :: Splice dom -> Expr dom
pattern SpliceExpr splice <- Ann _ (USplice splice)

-- | Template haskell quasi-quotation: @[$quoter|str]@
pattern QuasiQuoteExpr :: QuasiQuote dom -> Expr dom
pattern QuasiQuoteExpr qq <- Ann _ (UQuasiQuoteExpr qq)

-- | Template haskell quasi-quotation: @[$quoter|str]@
pattern ExprPragma :: ExprPragma dom -> Expr dom -> Expr dom
pattern ExprPragma pragma expr <- Ann _ (UExprPragma pragma expr)

-- | Arrow definition: @proc a -> f -< a+1@
pattern Proc :: Pattern dom -> Cmd dom -> Expr dom
pattern Proc pat cmd <- Ann _ (UProc pat cmd)

-- | Arrow definition: @proc a -> f -< a+1@
pattern ArrowApp :: Expr dom -> ArrowApp dom -> Expr dom -> Expr dom
pattern ArrowApp lhs arrow rhs <- Ann _ (UArrowApp lhs arrow rhs)

-- | Lambda case ( @\case 0 -> 1; 1 -> 2@ )
pattern LambdaCase :: AltList dom -> Expr dom
pattern LambdaCase alts <- Ann _ (ULamCase alts)

-- | Static pointer expression (@ static e @). The inner expression must be closed (cannot have variables bound outside)
pattern StaticPointer :: Expr dom -> Expr dom
pattern StaticPointer expr <- Ann _ (UStaticPtr expr)


-- * Field updates

-- | Update of a field (@ x = 1 @)
pattern NormalFieldUpdate :: Name dom -> Expr dom -> FieldUpdate dom
pattern NormalFieldUpdate n e <- Ann _ (UNormalFieldUpdate n e)

-- | Update the field to the value of the same name (@ x @)
pattern FieldPun :: Name dom -> FieldUpdate dom
pattern FieldPun n <- Ann _ (UFieldPun n)

-- | Update the fields of the bounded names to their values (@ .. @). Must be the last initializer. Cannot be used in a record update expression.
pattern FieldWildcard :: FieldWildcard dom -> FieldUpdate dom
pattern FieldWildcard wc <- Ann _ (UFieldWildcard wc)

-- * Tuple sections

-- | An existing element in a tuple section
pattern TupSecPresent :: Expr dom -> TupSecElem dom
pattern TupSecPresent expr <- Ann _ (Present expr)

-- | A missing element in a tuple section
pattern TupSecMissing :: TupSecElem dom
pattern TupSecMissing <- Ann _ Missing

-- * Pattern matching and guards

-- | Clause of case expression (@ Just x -> x + 1 @)
pattern Alt :: Pattern dom -> CaseRhs dom -> MaybeLocalBinds dom -> Alt dom
pattern Alt pat rhs locals <- Ann _ (UAlt pat rhs locals) 

-- | Unguarded right-hand side a pattern match (@ -> 3 @)
pattern CaseRhs :: Expr dom -> CaseRhs dom
pattern CaseRhs e <- Ann _ (UUnguardedCaseRhs e)

-- | Guarded right-hand sides of a pattern match (@ | x == 1 -> 3; | otherwise -> 4 @)
pattern GuardedCaseRhss :: GuardedCaseRhsList dom -> CaseRhs dom
pattern GuardedCaseRhss cases <- Ann _ (UGuardedCaseRhss cases)

-- | A guarded right-hand side of pattern matches binding (@ | x > 3 -> 2 @)      
pattern GuardedCaseRhs :: RhsGuardList dom -> Expr dom -> GuardedCaseRhs dom
pattern GuardedCaseRhs guards expr <- Ann _ (UGuardedCaseRhs guards expr)

-- * Pragmas that can be applied to expressions

-- | A @CORE@ pragma for adding notes to expressions.
pattern CorePragma :: String -> ExprPragma dom
pattern CorePragma str <- Ann _ (UCorePragma (Ann _ (UStringNode str)))

-- | An @SCC@ pragma for defining cost centers for profiling
pattern SccPragma :: String -> ExprPragma dom
pattern SccPragma str <- Ann _ (USccPragma (Ann _ (UStringNode str)))

-- | A pragma that describes if an expression was generated from a code fragment by an external tool (@ {-# GENERATED "Happy.y" 1:15-1:25 #-} @)
pattern GeneratedPragma :: SourceRange dom -> ExprPragma dom
pattern GeneratedPragma rng <- Ann _ (UGeneratedPragma rng)


-- | In-AST source ranges (for generated pragmas)
pattern SourceRange :: String -> Integer -> Integer -> Integer -> Integer -> SourceRange dom
pattern SourceRange file fromLine fromCol toLine toCol 
          <- Ann _ (USourceRange 
                     (Ann _ (UStringNode file))
                     (Ann _ (Number fromLine))
                     (Ann _ (Number fromCol))
                     (Ann _ (Number toLine))
                     (Ann _ (Number toCol)))
-- * Commands

-- | An arrow application command (@ f -< x + 1 @)
pattern ArrowAppCmd :: Expr dom -> ArrowApp dom -> Expr dom -> Cmd dom
pattern ArrowAppCmd lhs arrow rhs <- Ann _ (UArrowAppCmd lhs arrow rhs)

-- | A form command (@ (|untilA (increment -< x+y) (within 0.5 -< x)|) @)
pattern ArrowFormCmd :: Expr dom -> CmdList dom -> Cmd dom
pattern ArrowFormCmd expr cmds <- Ann _ (UArrowFormCmd expr cmds)

-- | A function application command
pattern AppCmd :: Cmd dom -> Expr dom -> Cmd dom
pattern AppCmd cmd expr <- Ann _ (UAppCmd cmd expr)

-- | An infix command application
pattern InfixCmd :: Cmd dom -> Name dom -> Cmd dom -> Cmd dom
pattern InfixCmd lhs op rhs <- Ann _ (UInfixCmd lhs op rhs)

-- | An infix command application
pattern LambdaCmd :: PatternList dom -> Cmd dom -> Cmd dom
pattern LambdaCmd pats cmd <- Ann _ (ULambdaCmd pats cmd)

-- | A parenthesized command
pattern ParenCmd :: Cmd dom -> Cmd dom
pattern ParenCmd cmd <- Ann _ (UParenCmd cmd)

-- | A pattern match command
pattern CaseCmd :: Expr dom -> CmdAltList dom -> Cmd dom
pattern CaseCmd expr alts <- Ann _ (UCaseCmd expr alts)

-- | An if command (@ if f x y then g -< x+1 else h -< y+2 @)
pattern IfCmd :: Expr dom -> Cmd dom -> Cmd dom -> Cmd dom
pattern IfCmd pred then_ else_ <- Ann _ (UIfCmd pred then_ else_)

-- | A local binding command (@ let z = x+y @)
pattern LetCmd :: LocalBindList dom -> Cmd dom -> Cmd dom
pattern LetCmd locals cmd <- Ann _ (ULetCmd locals cmd)

-- | A local binding command (@ let z = x+y @)
pattern DoCmd :: CmdStmtList dom -> Cmd dom
pattern DoCmd stmts <- Ann _ (UDoCmd stmts)


-- | Left arrow application: @-<@
pattern LeftAppl :: ArrowApp dom
pattern LeftAppl <- Ann _ ULeftAppl 

-- | Right arrow application: @>-@
pattern RightAppl :: ArrowApp dom
pattern RightAppl <- Ann _ URightAppl 

-- | Left arrow high application: @-<<@
pattern LeftHighApp :: ArrowApp dom
pattern LeftHighApp <- Ann _ ULeftHighApp 

-- | Right arrow high application: @>>-@
pattern RightHighApp :: ArrowApp dom
pattern RightHighApp <- Ann _ URightHighApp 





