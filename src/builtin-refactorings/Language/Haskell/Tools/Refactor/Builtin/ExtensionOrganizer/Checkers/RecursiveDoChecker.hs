module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.RecursiveDoChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkRecursiveDoExpr :: CheckNode Expr
chkRecursiveDoExpr e@MDo{} = addEvidence RecursiveDo e
chkRecursiveDoExpr e = return e

chkRecursiveDoStmt :: CheckNode Stmt
chkRecursiveDoStmt s@RecStmt{} = addEvidence RecursiveDo s
chkRecursiveDoStmt s = return s
