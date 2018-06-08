-- | Generation of expression-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkApp@ creates the annotated version of the @App@ AST constructor.
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Tools.Rewrite.Create.Exprs where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Rewrite.Create.Utils (mkAnn, mkAnnList, mkAnnMaybe)
import Language.Haskell.Tools.Rewrite.ElementTypes

-- * Expressions

-- | Create a expression for a variable or a data constructor (@ a @)
mkVar :: Name -> Expr
mkVar = mkAnn child . UVar

-- | Create a literal expression (@ 42 @)
mkLit :: Literal -> Expr
mkLit = mkAnn child . ULit

-- | Create a infix operator application expression (@ a + b @)
mkInfixApp :: Expr -> Operator -> Expr -> Expr
mkInfixApp lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UInfixApp lhs op rhs

-- | Create a prefix operator application expression (@ -x @)
mkPrefixApp :: Operator -> Expr -> Expr
mkPrefixApp op rhs = mkAnn (child <> child) $ UPrefixApp op rhs

-- | Create a function application expression (@ f 4 @)
mkApp :: Expr -> Expr -> Expr
mkApp f e = mkAnn (child <> " " <> child) (UApp f e)

-- | Create a lambda expression (@ \\a b -> a + b @)
mkLambda :: [Pattern] -> Expr -> Expr
mkLambda pats rhs = mkAnn ("\\" <> child <> " -> " <> child) $ ULambda (mkAnnList (separatedBy " " list) pats) rhs

-- | Create a local binding (@ let x = 2; y = 3 in e x y @)
mkLet :: [LocalBind] -> Expr -> Expr
mkLet pats expr = mkAnn ("let " <> child <> " in " <> child) $ ULet (mkAnnList (indented list) pats) expr

-- | Create a if expression (@ if a then b else c @)
mkIf :: Expr -> Expr -> Expr -> Expr
mkIf cond then_ else_ = mkAnn ("if " <> child <> " then " <> child <> " else " <> child) $ UIf cond then_ else_

-- | Create a multi way if expressions with @MultiWayIf@ extension (@ if | guard1 -> expr1; guard2 -> expr2 @)
mkMultiIf :: [GuardedCaseRhs] -> Expr
mkMultiIf cases = mkAnn ("if" <> child) $ UMultiIf (mkAnnList (indented list) cases)

-- | Create a pattern matching expression (@ case expr of pat1 -> expr1; pat2 -> expr2 @)
mkCase :: Expr -> [Alt] -> Expr
mkCase expr cases = mkAnn ("case " <> child <> " of " <> child) $ UCase expr (mkAnnList (indented list) cases)

-- | Create a do-notation expressions (@ do x <- act1; act2 @)
mkDoBlock :: [Stmt] -> Expr
mkDoBlock stmts = mkAnn (child <> " " <> child) $ UDo (mkAnn "do" UDoKeyword) (mkAnnList (indented list) stmts)

-- | Create a mdo-notation expressions (@ mdo x <- act1; act2 @)
mkMDoBlock :: [Stmt] -> Expr
mkMDoBlock stmts = mkAnn (child <> " " <> child) $ UDo (mkAnn "mdo" UMDoKeyword) (mkAnnList (indented list) stmts)


-- | Create a tuple expression (@ (e1, e2, e3) @)
mkTuple :: [Expr] -> Expr
mkTuple exprs = mkAnn ("(" <> child <> ")") $ UTuple (mkAnnList (separatedBy ", " list) exprs)

-- | Create a unboxed tuple expression (@ (\# e1, e2, e3 \#) @)
mkUnboxedTuple :: [Expr] -> Expr
mkUnboxedTuple exprs = mkAnn ("(# " <> child <> " #)") $ UTuple (mkAnnList (separatedBy ", " list) exprs)

-- | Create a tuple section, enabled with @TupleSections@ (@ (a,,b) @). One of the elements must be missing.
mkTupleSection :: [Maybe Expr] -> Expr
mkTupleSection elems
  = let tupSecs = map (maybe (mkAnn "" Missing) (mkAnn child . Present)) elems
     in mkAnn ("(" <> child <> ")") $ UTupleSection (mkAnnList (separatedBy ", " list) tupSecs)

-- | Create a unboxed tuple section, enabled with @TupleSections@ (@ (\#a,,b\#) @). One of the elements must be missing.
mkTupleUnboxedSection :: [Maybe Expr] -> Expr
mkTupleUnboxedSection elems
  = let tupSecs = map (maybe (mkAnn "" Missing) (mkAnn child . Present)) elems
     in mkAnn ("(" <> child <> ")") $ UTupleSection (mkAnnList (separatedBy ", " list) tupSecs)

-- | Create a list expression: @[1,2,3]@
mkList :: [Expr] -> Expr
mkList exprs = mkAnn ("[" <> child <> "]") $ UList (mkAnnList (separatedBy ", " list) exprs)

-- | Create a parallel array expression: @[: 1,2,3 :]@
mkParArray :: [Expr] -> Expr
mkParArray exprs = mkAnn ("[: " <> child <> " :]") $ UParArray (mkAnnList (separatedBy ", " list) exprs)

-- | Create a parenthesized expression: @( a + b )@
mkParen :: Expr -> Expr
mkParen = mkAnn ("(" <> child <> ")") . UParen

-- | Create a left operator section: @(1+)@
mkLeftSection :: Expr -> Operator -> Expr
mkLeftSection lhs op = mkAnn ("(" <> child <> " " <> child <> ")") $ ULeftSection lhs op

-- | Create a right operator section: @(+1)@
mkRightSection :: Operator -> Expr -> Expr
mkRightSection op rhs = mkAnn ("(" <> child <> " " <> child <> ")") $ URightSection op rhs

-- | Create a record value construction: @Point { x = 3, y = -2 }@
mkRecCon :: Name -> [FieldUpdate] -> Expr
mkRecCon name flds = mkAnn (child <> " { " <> child <> " }") $ URecCon name (mkAnnList (separatedBy ", " list) flds)

-- | Create a record value  update: @p1 { x = 3, y = -2 }@
mkRecUpdate :: Expr -> [FieldUpdate] -> Expr
mkRecUpdate expr flds = mkAnn (child <> " { " <> child <> " }") $ URecUpdate expr (mkAnnList (separatedBy ", " list) flds)

-- | Create a enumeration expression (@ [1,3..10] @)
mkEnum :: Expr -> Maybe (Expr) -> Maybe (Expr) -> Expr
mkEnum from step to = mkAnn ("[" <> child <> child <> ".." <> child <> "]") $ UEnum from (mkAnnMaybe (after "," opt) step) (mkAnnMaybe (after "," opt) to)

-- | Create a parallel array enumeration (@ [: 1,3 .. 10 :] @)
mkParArrayEnum :: Expr -> Maybe (Expr) -> Expr -> Expr
mkParArrayEnum from step to
  = mkAnn ("[: " <> child <> child <> ".." <> child <> " :]")
      $ UParArrayEnum from (mkAnnMaybe (after "," opt) step) to

-- | Create a list comprehension (@ [ (x, y) | x <- xs | y <- ys ] @)
mkListComp :: Expr -> [ListCompBody] -> Expr
mkListComp expr stmts
  = mkAnn ("[ " <> child <> " | " <> child <> " ]")
      $ UListComp expr $ mkAnnList (separatedBy " | " list) stmts

-- | Create a parallel array comprehensions @ [: (x, y) | x <- xs , y <- ys :] @ enabled by @ParallelArrays@
mkParArrayComp :: Expr -> [ListCompBody] -> Expr
mkParArrayComp expr stmts
  = mkAnn ("[: " <> child <> " | " <> child <> " :]")
      $ UParArrayComp expr $ mkAnnList (separatedBy " | " list) stmts

-- | Create a explicit type signature (@ x :: Int @)
mkExprTypeSig :: Expr -> Type -> Expr
mkExprTypeSig lhs typ = mkAnn (child <> " :: " <> child) $ UTypeSig lhs typ

-- | Create a explicit type application (@ show \@Integer (read "5") @)
mkExplicitTypeApp :: Expr -> Type -> Expr
mkExplicitTypeApp expr typ = mkAnn (child <> " @" <> child) $ UExplTypeApp expr typ

-- | @'x@ for template haskell reifying of expressions
mkVarQuote :: Name -> Expr
mkVarQuote = mkAnn ("'" <> child) . UVarQuote

-- | @''T@ for template haskell reifying of types
mkTypeQuote :: Name -> Expr
mkTypeQuote = mkAnn ("''" <> child) . UTypeQuote

-- | Create a template haskell bracket expression
mkBracketExpr :: Bracket -> Expr
mkBracketExpr = mkAnn child . UBracketExpr

-- | Create a template haskell splice expression, for example: @$(gen a)@ or @$x@
mkSpliceExpr :: Splice -> Expr
mkSpliceExpr = mkAnn child . USplice

-- | Create a template haskell quasi quote expression, for example: @[quoter| a + b ]@
mkQuasiQuoteExpr :: QuasiQuote -> Expr
mkQuasiQuoteExpr = mkAnn child . UQuasiQuoteExpr

-- | Creates a pragma that marks an expression.
mkExprPragma :: ExprPragma -> Expr -> Expr
mkExprPragma pragma expr = mkAnn (child <> " " <> child) $ UExprPragma pragma expr

-- | Create a arrow definition: @proc a -> f -< a+1@
mkProcExpr :: Pattern -> Cmd -> Expr
mkProcExpr pat cmd = mkAnn ("proc " <> child <> " -> " <> child) $ UProc pat cmd

-- | Create a arrow definition: @proc a -> f -< a+1@
mkArrowApp :: Expr -> ArrowApp -> Expr -> Expr
mkArrowApp lhs arrow rhs = mkAnn (child <> " " <> child <> " " <> child) $ UArrowApp lhs arrow rhs

-- | Create a lambda case ( @\case 0 -> 1; 1 -> 2@ )
mkLambdaCase :: [Alt] -> Expr
mkLambdaCase = mkAnn ("\\case" <> child) . ULamCase . mkAnnList (indented list)

-- | Create a static pointer expression (@ static e @). The inner expression must be closed (cannot have variables bound outside)
mkStaticPointer :: Expr -> Expr
mkStaticPointer = mkAnn ("static" <> child) . UStaticPtr



-- * Field updates

-- | Create a update of a field (@ x = 1 @)
mkFieldUpdate :: Name -> Expr -> FieldUpdate
mkFieldUpdate name val = mkAnn (child <> " = " <> child) $ UNormalFieldUpdate name val

-- | Create a update the field to the value of the same name (@ x @)
mkFieldPun :: Name -> FieldUpdate
mkFieldPun name = mkAnn child $ UFieldPun name

-- | Create a update the fields of the bounded names to their values (@ .. @). Must be the last initializer. Cannot be used in a record update expression.
mkFieldWildcard :: FieldUpdate
mkFieldWildcard = mkAnn child $ UFieldWildcard $ mkAnn ".." FldWildcard


-- * Pattern matching and guards

-- | Create a clause of case expression (@ Just x -> x + 1 @)
mkAlt :: Pattern -> CaseRhs -> Maybe LocalBinds -> Alt
mkAlt pat rhs locals = mkAnn (child <> child <> child) $ UAlt pat rhs (mkAnnMaybe (after " where " opt) locals)

-- | Create a unguarded right-hand side a pattern match (@ -> 3 @)
mkCaseRhs :: Expr -> CaseRhs
mkCaseRhs = mkAnn (" -> " <> child) . UUnguardedCaseRhs

-- | Create a guarded right-hand sides of a pattern match (@ | x == 1 -> 3; | otherwise -> 4 @)
mkGuardedCaseRhss :: [GuardedCaseRhs] -> CaseRhs
mkGuardedCaseRhss = mkAnn child . UGuardedCaseRhss . mkAnnList (indented list)

-- | Creates a guarded right-hand side of pattern matches binding (@ | x > 3 -> 2 @)
mkGuardedCaseRhs :: [RhsGuard] -> Expr -> GuardedCaseRhs
mkGuardedCaseRhs guards expr = mkAnn (" | " <> child <> " -> " <> child) $ UGuardedCaseRhs (mkAnnList (separatedBy ", " list) guards) expr

-- * Pragmas that can be applied to expressions

-- | Creates a @CORE@ pragma for adding notes to expressions.
mkCorePragma :: String -> ExprPragma
mkCorePragma = mkAnn ("{-# CORE " <> child <> " #-}") . UCorePragma
                 . mkAnn ("\"" <> child <> "\"") . UStringNode

-- | Creates an @SCC@ pragma for defining cost centers for profiling
mkSccPragma :: String -> ExprPragma
mkSccPragma = mkAnn ("{-# SCC " <> child <> " #-}") . USccPragma
                . mkAnn ("\"" <> child <> "\"") . UStringNode

-- | Creates a pragma that describes if an expression was generated from a code fragment by an external tool (@ {-\# GENERATED "Happy.y" 1:15-1:25 \#-} @)
mkGeneratedPragma :: SourceRange -> ExprPragma
mkGeneratedPragma = mkAnn ("{-# GENERATED " <> child <> " #-}") . UGeneratedPragma

-- | Create a in-AST source ranges (for generated pragmas)
mkSourceRange :: String -> Integer -> Integer -> Integer -> Integer -> SourceRange
mkSourceRange file fromLine fromCol toLine toCol
  = mkAnn (child <> " " <> child <> ":" <> child <> "-" <> child <> ":" <> child)
      $ USourceRange (mkAnn ("\"" <> child <> "\"") $ UStringNode file)
          (mkNumber fromLine) (mkNumber fromCol) (mkNumber toLine) (mkNumber toCol)
  where mkNumber = mkAnn child . Number

-- * Commands

-- | An arrow application command (@ f -< x + 1 @)
mkArrowAppCmd :: Expr -> ArrowApp -> Expr -> Cmd
mkArrowAppCmd lhs arrow rhs
  = mkAnn (child <> " " <> child <> " " <> child)
      $ UArrowAppCmd lhs arrow rhs

-- | A form command (@ (|untilA (increment -< x+y) (within 0.5 -< x)|) @)
mkArrowFromCmd :: Expr -> [Cmd] -> Cmd
mkArrowFromCmd expr cmds
  = mkAnn ("(| " <> child <> child <> " |)")
      $ UArrowFormCmd expr $ mkAnnList (after " " $ separatedBy " " list) cmds

-- | A function application command
mkAppCmd :: Cmd -> Expr -> Cmd
mkAppCmd cmd expr = mkAnn (child <> " " <> child)
                      $ UAppCmd cmd expr

-- | An infix command application
mkInfixCmd :: Cmd -> Name -> Cmd -> Cmd
mkInfixCmd lhs op rhs = mkAnn (child <> " " <> child <> " " <> child)
                          $ UInfixCmd lhs op rhs

-- | A lambda command
mkLambdaCmd :: [Pattern] -> Cmd -> Cmd
mkLambdaCmd args cmd = mkAnn ("\\" <> child <> " -> " <> child)
                         $ ULambdaCmd (mkAnnList (separatedBy " " list) args) cmd

-- | A parenthesized command
mkParenCmd :: Cmd -> Cmd
mkParenCmd cmd = mkAnn ("(" <> child <> ")") $ UParenCmd cmd

-- | A pattern match command
mkCaseCmd :: Expr -> [CmdAlt] -> Cmd
mkCaseCmd expr alts
  = mkAnn ("case " <> child <> " of " <> child)
      $ UCaseCmd expr $ mkAnnList (indented list) alts

-- | An if command (@ if f x y then g -< x+1 else h -< y+2 @)
mkIfCmd :: Expr -> Cmd -> Cmd -> Cmd
mkIfCmd pred then_ else_
  = mkAnn ("if " <> child <> " then " <> child <> " else " <> child)
      $ UIfCmd pred then_ else_

-- | A local binding command (@ let z = x+y @)
mkLetCmd :: [LocalBind] -> Cmd -> Cmd
mkLetCmd binds cmd
  = mkAnn ("let " <> child <> " in " <> child)
      $ ULetCmd (mkAnnList (indented list) binds) cmd

-- | A do-notation in a command
mkDoCmd :: [CmdStmt] -> Cmd
mkDoCmd stmts = mkAnn ("do " <> child) $ UDoCmd (mkAnnList (indented list) stmts)

-- | Left arrow application: @-<@
mkLeftAppl :: ArrowApp
mkLeftAppl = mkAnn "-<" ULeftAppl

-- | Right arrow application: @>-@
mkRightAppl :: ArrowApp
mkRightAppl = mkAnn ">-" URightAppl

-- | Left arrow high application: @-<<@
mkLeftHighAppl :: ArrowApp
mkLeftHighAppl = mkAnn "-<<" ULeftHighApp

-- | Right arrow high application: @>>-@
mkRightHighAppl :: ArrowApp
mkRightHighAppl = mkAnn ">>-" URightHighApp

-- | A hole expression @_@
mkHole :: Expr
mkHole = mkAnn "_" UHole
