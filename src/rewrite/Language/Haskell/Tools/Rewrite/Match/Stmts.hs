-- | UPattern matching on statement-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}

module Language.Haskell.Tools.Rewrite.Match.Stmts where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite.ElementTypes

-- * Do-notation

-- | Binding statement (@ x <- action @)
pattern BindStmt :: Pattern -> Expr -> Stmt
pattern BindStmt bound expr <- Ann _ (UBindStmt bound expr)

-- | Non-binding statement (@ action @)
pattern ExprStmt :: Expr -> Stmt
pattern ExprStmt expr <- Ann _ (UExprStmt expr)

-- | Let statement (@ let x = 3; y = 4 @)
pattern LetStmt :: LocalBindList -> Stmt
pattern LetStmt binds <- Ann _ (ULetStmt binds)

-- | A recursive binding statement with (@ rec b <- f a c; c <- f b a @)
pattern RecStmt :: StmtList -> Stmt
pattern RecStmt stmts <- Ann _ (URecStmt stmts)

pattern DoKeyword :: DoKind
pattern DoKeyword <- Ann _ UDoKeyword

pattern MDoKeyword :: DoKind
pattern MDoKeyword <- Ann _ UMDoKeyword

-- * List comprehensions

-- | Body of a list comprehension: (@ | x <- [1..10] @)
pattern ListCompBody :: CompStmtList -> ListCompBody
pattern ListCompBody stmts <- Ann _ (UListCompBody stmts)

-- | Normal monadic statement of a list comprehension
pattern CompStmt :: Stmt -> CompStmt
pattern CompStmt stmt <- Ann _ (UCompStmt stmt)

-- | Then statements by @TransformListComp@ (@ then sortWith by (x + y) @)
pattern ThenStmt :: Expr -> MaybeExpr -> CompStmt
pattern ThenStmt then_ by <- Ann _ (UThenStmt then_ by)

-- | Grouping statements by @TransformListComp@ (@ then group by (x + y) using groupWith @) 
pattern GroupStmt :: MaybeExpr -> MaybeExpr -> CompStmt
pattern GroupStmt by using <- Ann _ (UGroupStmt by using)

-- * Commands

-- | Binding statement command (@ x <- action @)
pattern BindStmtCmd :: Pattern -> Cmd -> CmdStmt
pattern BindStmtCmd bound expr <- Ann _ (UBindStmt bound expr)

-- | Non-binding statement command (@ action @)
pattern ExprStmtCmd :: Cmd -> CmdStmt
pattern ExprStmtCmd expr <- Ann _ (UExprStmt expr)

-- | Let statement command (@ let x = 3; y = 4 @)
pattern LetStmtCmd :: LocalBindList -> CmdStmt
pattern LetStmtCmd binds <- Ann _ (ULetStmt binds)

-- | A recursive binding statement command with (@ rec b <- f a c; c <- f b a @)
pattern RecStmtCmd :: CmdStmtList -> CmdStmt
pattern RecStmtCmd stmts <- Ann _ (URecStmt stmts)