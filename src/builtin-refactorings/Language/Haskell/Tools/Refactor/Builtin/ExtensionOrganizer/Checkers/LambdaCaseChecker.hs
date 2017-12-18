module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.LambdaCaseChecker where

import Language.Haskell.Tools.Refactor as Refact
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad as Ext

chkLambdaCase :: CheckNode Expr
chkLambdaCase = conditional chkLambdaCase' Ext.LambdaCase

chkLambdaCase' :: CheckNode Expr
chkLambdaCase' e@(Refact.LambdaCase _) = addOccurence Ext.LambdaCase e
chkLambdaCase' e = return e


{-
  TopLevelPragma,
  Rule,

  Splice,
  Bracket,

  Cmd,

  Rhs DONE
  RhsGuard DONE
  Expr DONE
  FieldUpdate DONE
  TupSecElem DONE
  CaseRhs DONE
  Stmt DONE
  CompStmt DONE
  Pattern DONE
-}
