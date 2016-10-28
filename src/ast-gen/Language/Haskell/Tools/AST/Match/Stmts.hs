-- | UPattern matching on statement-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Stmts where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

-- * Do-notation

-- | Binding statement (@ x <- action @)
pattern BindStmt :: Pattern dom -> Expr dom -> Stmt dom
pattern BindStmt bound expr <- Ann _ (UBindStmt bound expr)

-- | Non-binding statement (@ action @)
pattern ExprStmt :: Expr dom -> Stmt dom
pattern ExprStmt expr <- Ann _ (UExprStmt expr)

-- | Let statement (@ let x = 3; y = 4 @)
pattern LetStmt :: LocalBindList dom -> Stmt dom
pattern LetStmt binds <- Ann _ (ULetStmt binds)

-- | A recursive binding statement with (@ rec b <- f a c; c <- f b a @)
pattern RecStmt :: StmtList dom -> Stmt dom
pattern RecStmt stmts <- Ann _ (URecStmt stmts)

pattern DoKeyword :: DoKind dom
pattern DoKeyword <- Ann _ UDoKeyword

pattern MDoKeyword :: DoKind dom
pattern MDoKeyword <- Ann _ UMDoKeyword

-- * List comprehensions

-- | Body of a list comprehension: (@ | x <- [1..10] @)
pattern ListCompBody :: CompStmtList dom -> ListCompBody dom
pattern ListCompBody stmts <- Ann _ (UListCompBody stmts)

-- | Normal monadic statement of a list comprehension
pattern CompStmt :: Stmt dom -> CompStmt dom
pattern CompStmt stmt <- Ann _ (UCompStmt stmt)

-- | Then statements by @TransformListComp@ (@ then sortWith by (x + y) @)
pattern ThenStmt :: Expr dom -> MaybeExpr dom -> CompStmt dom
pattern ThenStmt then_ by <- Ann _ (UThenStmt then_ by)

-- | Grouping statements by @TransformListComp@ (@ then group by (x + y) using groupWith @) 
pattern GroupStmt :: MaybeExpr dom -> MaybeExpr dom -> CompStmt dom
pattern GroupStmt by using <- Ann _ (UGroupStmt by using)

-- * Commands

-- | Binding statement command (@ x <- action @)
pattern BindStmtCmd :: Pattern dom -> Cmd dom -> CmdStmt dom
pattern BindStmtCmd bound expr <- Ann _ (UBindStmt bound expr)

-- | Non-binding statement command (@ action @)
pattern ExprStmtCmd :: Cmd dom -> CmdStmt dom
pattern ExprStmtCmd expr <- Ann _ (UExprStmt expr)

-- | Let statement command (@ let x = 3; y = 4 @)
pattern LetStmtCmd :: LocalBindList dom -> CmdStmt dom
pattern LetStmtCmd binds <- Ann _ (ULetStmt binds)

-- | A recursive binding statement command with (@ rec b <- f a c; c <- f b a @)
pattern RecStmtCmd :: CmdStmtList dom -> CmdStmt dom
pattern RecStmtCmd stmts <- Ann _ (URecStmt stmts)