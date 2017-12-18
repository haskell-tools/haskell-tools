module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.RecursiveDoChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkRecursiveDoExpr :: CheckNode Expr
chkRecursiveDoExpr e@MDo{} = addOccurence RecursiveDo e
chkRecursiveDoExpr e = return e

chkRecursiveDoStmt :: CheckNode Stmt
chkRecursiveDoStmt s@RecStmt{} = addOccurence RecursiveDo s
chkRecursiveDoStmt s = return s
