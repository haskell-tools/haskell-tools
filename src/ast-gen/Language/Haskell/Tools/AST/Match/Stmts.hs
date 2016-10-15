-- | Pattern matching on statement-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Stmts where

import Language.Haskell.Tools.AST

pattern BindStmt :: Ann Pattern dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Stmt dom SrcTemplateStage
pattern BindStmt bound expr <- Ann _ (UBindStmt bound expr)

pattern ExprStmt :: Ann Expr dom SrcTemplateStage -> Ann Stmt dom SrcTemplateStage
pattern ExprStmt expr <- Ann _ (UExprStmt expr)

pattern LetStmt :: AnnList LocalBind dom SrcTemplateStage -> Ann Stmt dom SrcTemplateStage
pattern LetStmt binds <- Ann _ (ULetStmt binds)

pattern ListCompBody :: AnnList CompStmt dom SrcTemplateStage -> Ann ListCompBody dom SrcTemplateStage
pattern ListCompBody stmts <- Ann _ (UListCompBody stmts)

pattern CompStmt :: Ann Stmt dom SrcTemplateStage -> Ann CompStmt dom SrcTemplateStage
pattern CompStmt stmt <- Ann _ (UCompStmt stmt)