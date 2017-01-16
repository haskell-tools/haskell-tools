-- | Generation of expression-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkApp@ creates the annotated version of the @App@ AST constructor.
{-# LANGUAGE OverloadedStrings 
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Exprs where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Utils (mkAnn, mkAnnList, mkAnnMaybe)
import Language.Haskell.Tools.Transform

-- * Expressions

-- | Create a expression for a variable or a data constructor (@ a @)
mkVar :: Name dom -> Expr dom
mkVar = mkAnn child . UVar

-- | Create a literal expression (@ 42 @)
mkLit :: Literal dom -> Expr dom
mkLit = mkAnn child . ULit

-- | Create a infix operator application expression (@ a + b @)
mkInfixApp :: Expr dom -> Operator dom -> Expr dom -> Expr dom
mkInfixApp lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UInfixApp lhs op rhs

-- | Create a prefix operator application expression (@ -x @)
mkPrefixApp :: Operator dom -> Expr dom -> Expr dom
mkPrefixApp op rhs = mkAnn (child <> child) $ UPrefixApp op rhs

-- | Create a function application expression (@ f 4 @)
mkApp :: Expr dom -> Expr dom -> Expr dom
mkApp f e = mkAnn (child <> " " <> child) (UApp f e)

-- | Create a lambda expression (@ \\a b -> a + b @)
mkLambda :: [Pattern dom] -> Expr dom -> Expr dom
mkLambda pats rhs = mkAnn ("\\" <> child <> " -> " <> child) $ ULambda (mkAnnList (separatedBy " " list) pats) rhs

-- | Create a local binding (@ let x = 2; y = 3 in e x y @)
mkLet :: [LocalBind dom] -> Expr dom -> Expr dom
mkLet pats expr = mkAnn ("let " <> child <> " in " <> child) $ ULet (mkAnnList (indented list) pats) expr

-- | Create a if expression (@ if a then b else c @)
mkIf :: Expr dom -> Expr dom -> Expr dom -> Expr dom
mkIf cond then_ else_ = mkAnn ("if " <> child <> " then " <> child <> " else " <> child) $ UIf cond then_ else_

-- | Create a multi way if expressions with @MultiWayIf@ extension (@ if | guard1 -> expr1; guard2 -> expr2 @)
mkMultiIf :: [GuardedCaseRhs dom] -> Expr dom
mkMultiIf cases = mkAnn ("if" <> child) $ UMultiIf (mkAnnList (indented list) cases)

-- | Create a pattern matching expression (@ case expr of pat1 -> expr1; pat2 -> expr2 @)
mkCase :: Expr dom -> [Alt dom] -> Expr dom
mkCase expr cases = mkAnn ("case " <> child <> " of " <> child) $ UCase expr (mkAnnList (indented list) cases)

-- | Create a do-notation expressions (@ do x <- act1; act2 @)
mkDoBlock :: [Stmt dom] -> Expr dom
mkDoBlock stmts = mkAnn (child <> " " <> child) $ UDo (mkAnn "do" UDoKeyword) (mkAnnList (indented list) stmts)

-- | Create a tuple expression (@ (e1, e2, e3) @)
mkTuple :: [Expr dom] -> Expr dom
mkTuple exprs = mkAnn ("(" <> child <> ")") $ UTuple (mkAnnList (separatedBy ", " list) exprs)

-- | Create a unboxed tuple expression (@ (\# e1, e2, e3 \#) @)
mkUnboxedTuple :: [Expr dom] -> Expr dom
mkUnboxedTuple exprs = mkAnn ("(# " <> child <> " #)") $ UTuple (mkAnnList (separatedBy ", " list) exprs)

-- | Create a tuple section, enabled with @TupleSections@ (@ (a,,b) @). One of the elements must be missing.
mkTupleSection :: [Maybe (Expr dom)] -> Expr dom
mkTupleSection elems 
  = let tupSecs = map (maybe (mkAnn "" Missing) (mkAnn child . Present)) elems
     in mkAnn ("(" <> child <> ")") $ UTupleSection (mkAnnList (separatedBy ", " list) tupSecs)

-- | Create a unboxed tuple section, enabled with @TupleSections@ (@ (\#a,,b\#) @). One of the elements must be missing.
mkTupleUnboxedSection :: [Maybe (Expr dom)] -> Expr dom
mkTupleUnboxedSection elems 
  = let tupSecs = map (maybe (mkAnn "" Missing) (mkAnn child . Present)) elems
     in mkAnn ("(" <> child <> ")") $ UTupleSection (mkAnnList (separatedBy ", " list) tupSecs)

-- | Create a list expression: @[1,2,3]@
mkList :: [Expr dom] -> Expr dom
mkList exprs = mkAnn ("[" <> child <> "]") $ UList (mkAnnList (separatedBy ", " list) exprs)

-- | Create a parallel array expression: @[: 1,2,3 :]@
mkParArray :: [Expr dom] -> Expr dom
mkParArray exprs = mkAnn ("[: " <> child <> " :]") $ UParArray (mkAnnList (separatedBy ", " list) exprs)

-- | Create a parenthesized expression: @( a + b )@
mkParen :: Expr dom -> Expr dom
mkParen = mkAnn ("(" <> child <> ")") . UParen

-- | Create a left operator section: @(1+)@
mkLeftSection :: Expr dom -> Operator dom -> Expr dom
mkLeftSection lhs op = mkAnn ("(" <> child <> " " <> child <> ")") $ ULeftSection lhs op

-- | Create a right operator section: @(+1)@
mkRightSection :: Operator dom -> Expr dom -> Expr dom
mkRightSection op rhs = mkAnn ("(" <> child <> " " <> child <> ")") $ URightSection op rhs

-- | Create a record value construction: @Point { x = 3, y = -2 }@
mkRecCon :: Name dom -> [FieldUpdate dom] -> Expr dom
mkRecCon name flds = mkAnn (child <> " { " <> child <> " }") $ URecCon name (mkAnnList (separatedBy ", " list) flds)

-- | Create a record value  update: @p1 { x = 3, y = -2 }@
mkRecUpdate :: Expr dom -> [FieldUpdate dom] -> Expr dom
mkRecUpdate expr flds = mkAnn (child <> " { " <> child <> " }") $ URecUpdate expr (mkAnnList (separatedBy ", " list) flds)

-- | Create a enumeration expression (@ [1,3..10] @)
mkEnum :: Expr dom -> Maybe (Expr dom) -> Maybe (Expr dom) -> Expr dom
mkEnum from step to = mkAnn ("[" <> child <> child <> ".." <> child <> "]") $ UEnum from (mkAnnMaybe (after "," opt) step) (mkAnnMaybe (after "," opt) to)

-- | Create a parallel array enumeration (@ [: 1,3 .. 10 :] @)
mkParArrayEnum :: Expr dom -> Maybe (Expr dom) -> Expr dom -> Expr dom
mkParArrayEnum from step to
  = mkAnn ("[: " <> child <> child <> ".." <> child <> " :]") 
      $ UParArrayEnum from (mkAnnMaybe (after "," opt) step) to

-- | Create a list comprehension (@ [ (x, y) | x <- xs | y <- ys ] @)
mkListComp :: Expr dom -> [ListCompBody dom] -> Expr dom
mkListComp expr stmts 
  = mkAnn ("[ " <> child <> " | " <> child <> " ]") 
      $ UListComp expr $ mkAnnList (separatedBy " | " list) stmts

-- | Create a parallel array comprehensions @ [: (x, y) | x <- xs , y <- ys :] @ enabled by @ParallelArrays@
mkParArrayComp :: Expr dom -> [ListCompBody dom] -> Expr dom
mkParArrayComp expr stmts 
  = mkAnn ("[: " <> child <> " | " <> child <> " :]") 
      $ UParArrayComp expr $ mkAnnList (separatedBy " | " list) stmts

-- | Create a explicit type signature (@ x :: Int @)
mkExprTypeSig :: Expr dom -> Type dom -> Expr dom
mkExprTypeSig lhs typ = mkAnn (child <> " :: " <> child) $ UTypeSig lhs typ

-- | Create a explicit type application (@ show \@Integer (read "5") @)
mkExplicitTypeApp :: Expr dom -> Type dom -> Expr dom
mkExplicitTypeApp expr typ = mkAnn (child <> " @" <> child) $ UExplTypeApp expr typ

-- | @'x@ for template haskell reifying of expressions
mkVarQuote :: Name dom -> Expr dom
mkVarQuote = mkAnn ("'" <> child) . UVarQuote

-- | @''T@ for template haskell reifying of types
mkTypeQuote :: Name dom -> Expr dom
mkTypeQuote = mkAnn ("''" <> child) . UTypeQuote

-- | Create a template haskell bracket expression
mkBracketExpr :: Bracket dom -> Expr dom
mkBracketExpr = mkAnn child . UBracketExpr

-- | Create a template haskell splice expression, for example: @$(gen a)@ or @$x@
mkSpliceExpr :: Splice dom -> Expr dom
mkSpliceExpr = mkAnn child . USplice

-- | Create a template haskell quasi quote expression, for example: @[quoter| a + b ]@
mkQuasiQuoteExpr :: QuasiQuote dom -> Expr dom
mkQuasiQuoteExpr = mkAnn child . UQuasiQuoteExpr

-- | Creates a pragma that marks an expression.
mkExprPragma :: ExprPragma dom -> Expr dom -> Expr dom
mkExprPragma pragma expr = mkAnn (child <> " " <> child) $ UExprPragma pragma expr

-- | Create a arrow definition: @proc a -> f -< a+1@
mkProcExpr :: Pattern dom -> Cmd dom -> Expr dom
mkProcExpr pat cmd = mkAnn ("proc " <> child <> " -> " <> child) $ UProc pat cmd

-- | Create a arrow definition: @proc a -> f -< a+1@
mkArrowApp :: Expr dom -> ArrowApp dom -> Expr dom -> Expr dom
mkArrowApp lhs arrow rhs = mkAnn (child <> " " <> child <> " " <> child) $ UArrowApp lhs arrow rhs

-- | Create a lambda case ( @\case 0 -> 1; 1 -> 2@ )
mkLambdaCase :: [Alt dom] -> Expr dom
mkLambdaCase = mkAnn ("\\case" <> child) . ULamCase . mkAnnList (indented list)

-- | Create a static pointer expression (@ static e @). The inner expression must be closed (cannot have variables bound outside)
mkStaticPointer :: Expr dom -> Expr dom
mkStaticPointer = mkAnn ("static" <> child) . UStaticPtr



-- * Field updates

-- | Create a update of a field (@ x = 1 @)
mkFieldUpdate :: Name dom -> Expr dom -> FieldUpdate dom
mkFieldUpdate name val = mkAnn (child <> " = " <> child) $ UNormalFieldUpdate name val

-- | Create a update the field to the value of the same name (@ x @)
mkFieldPun :: Name dom -> FieldUpdate dom
mkFieldPun name = mkAnn child $ UFieldPun name

-- | Create a update the fields of the bounded names to their values (@ .. @). Must be the last initializer. Cannot be used in a record update expression.
mkFieldWildcard :: FieldUpdate dom
mkFieldWildcard = mkAnn child $ UFieldWildcard $ mkAnn ".." FldWildcard


-- * Pattern matching and guards

-- | Create a clause of case expression (@ Just x -> x + 1 @)
mkAlt :: Pattern dom -> CaseRhs dom -> Maybe (LocalBinds dom) -> Alt dom
mkAlt pat rhs locals = mkAnn (child <> child <> child) $ UAlt pat rhs (mkAnnMaybe (after " where " opt) locals)

-- | Create a unguarded right-hand side a pattern match (@ -> 3 @)
mkCaseRhs :: Expr dom -> CaseRhs dom
mkCaseRhs = mkAnn (" -> " <> child) . UUnguardedCaseRhs

-- | Create a guarded right-hand sides of a pattern match (@ | x == 1 -> 3; | otherwise -> 4 @)
mkGuardedCaseRhss :: [GuardedCaseRhs dom] -> CaseRhs dom
mkGuardedCaseRhss = mkAnn child . UGuardedCaseRhss . mkAnnList (indented list)

-- | Creates a guarded right-hand side of pattern matches binding (@ | x > 3 -> 2 @)      
mkGuardedCaseRhs :: [RhsGuard dom] -> Expr dom -> GuardedCaseRhs dom
mkGuardedCaseRhs guards expr = mkAnn (" | " <> child <> " -> " <> child) $ UGuardedCaseRhs (mkAnnList (separatedBy ", " list) guards) expr

-- * Pragmas that can be applied to expressions

-- | Creates a @CORE@ pragma for adding notes to expressions.
mkCorePragma :: String -> ExprPragma dom
mkCorePragma = mkAnn ("{-# CORE " <> child <> " #-}") . UCorePragma 
                 . mkAnn ("\"" <> child <> "\"") . UStringNode

-- | Creates an @SCC@ pragma for defining cost centers for profiling
mkSccPragma :: String -> ExprPragma dom
mkSccPragma = mkAnn ("{-# SCC " <> child <> " #-}") . USccPragma 
                . mkAnn ("\"" <> child <> "\"") . UStringNode

-- | Creates a pragma that describes if an expression was generated from a code fragment by an external tool (@ {-\# GENERATED "Happy.y" 1:15-1:25 \#-} @)
mkGeneratedPragma :: SourceRange dom -> ExprPragma dom
mkGeneratedPragma = mkAnn ("{-# GENERATED " <> child <> " #-}") . UGeneratedPragma 

-- | Create a in-AST source ranges (for generated pragmas)
mkSourceRange :: String -> Integer -> Integer -> Integer -> Integer -> SourceRange dom
mkSourceRange file fromLine fromCol toLine toCol
  = mkAnn (child <> " " <> child <> ":" <> child <> "-" <> child <> ":" <> child)
      $ USourceRange (mkAnn ("\"" <> child <> "\"") $ UStringNode file)
          (mkNumber fromLine) (mkNumber fromCol) (mkNumber toLine) (mkNumber toCol)
  where mkNumber = mkAnn child . Number

-- * Commands

-- | An arrow application command (@ f -< x + 1 @)
mkArrowAppCmd :: Expr dom -> ArrowApp dom -> Expr dom -> Cmd dom
mkArrowAppCmd lhs arrow rhs 
  = mkAnn (child <> " " <> child <> " " <> child)
      $ UArrowAppCmd lhs arrow rhs

-- | A form command (@ (|untilA (increment -< x+y) (within 0.5 -< x)|) @)
mkArrowFromCmd :: Expr dom -> [Cmd dom] -> Cmd dom
mkArrowFromCmd expr cmds 
  = mkAnn ("(| " <> child <> child <> " |)")
      $ UArrowFormCmd expr $ mkAnnList (after " " $ separatedBy " " list) cmds

-- | A function application command
mkAppCmd :: Cmd dom -> Expr dom -> Cmd dom
mkAppCmd cmd expr = mkAnn (child <> " " <> child)
                      $ UAppCmd cmd expr

-- | An infix command application
mkInfixCmd :: Cmd dom -> Name dom -> Cmd dom -> Cmd dom
mkInfixCmd lhs op rhs = mkAnn (child <> " " <> child <> " " <> child)
                          $ UInfixCmd lhs op rhs

-- | A lambda command 
mkLambdaCmd :: [Pattern dom] -> Cmd dom -> Cmd dom
mkLambdaCmd args cmd = mkAnn ("\\" <> child <> " -> " <> child)
                         $ ULambdaCmd (mkAnnList (separatedBy " " list) args) cmd

-- | A parenthesized command
mkParenCmd :: Cmd dom -> Cmd dom
mkParenCmd cmd = mkAnn ("(" <> child <> ")") $ UParenCmd cmd

-- | A pattern match command
mkCaseCmd :: Expr dom -> [CmdAlt dom] -> Cmd dom
mkCaseCmd expr alts 
  = mkAnn ("case " <> child <> " of " <> child) 
      $ UCaseCmd expr $ mkAnnList (indented list) alts

-- | An if command (@ if f x y then g -< x+1 else h -< y+2 @)
mkIfCmd :: Expr dom -> Cmd dom -> Cmd dom -> Cmd dom
mkIfCmd pred then_ else_ 
  = mkAnn ("if " <> child <> " then " <> child <> " else " <> child) 
      $ UIfCmd pred then_ else_

-- | A local binding command (@ let z = x+y @)
mkLetCmd :: [LocalBind dom] -> Cmd dom -> Cmd dom
mkLetCmd binds cmd
  = mkAnn ("let " <> child <> " in " <> child) 
      $ ULetCmd (mkAnnList (indented list) binds) cmd

-- | A do-notation in a command
mkDoCmd :: [CmdStmt dom] -> Cmd dom
mkDoCmd stmts = mkAnn ("do " <> child) $ UDoCmd (mkAnnList (indented list) stmts)

-- | Left arrow application: @-<@
mkLeftAppl :: ArrowApp dom
mkLeftAppl = mkAnn "-<" ULeftAppl

-- | Right arrow application: @>-@
mkRightAppl :: ArrowApp dom
mkRightAppl = mkAnn ">-" URightAppl

-- | Left arrow high application: @-<<@
mkLeftHighAppl :: ArrowApp dom
mkLeftHighAppl = mkAnn "-<<" ULeftHighApp

-- | Right arrow high application: @>>-@
mkRightHighAppl :: ArrowApp dom
mkRightHighAppl = mkAnn ">>-" URightHighApp




