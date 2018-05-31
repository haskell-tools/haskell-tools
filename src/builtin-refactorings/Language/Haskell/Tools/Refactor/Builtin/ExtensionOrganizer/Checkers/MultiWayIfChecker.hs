module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.MultiWayIfChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkMultiWayIfExpr :: CheckNode Expr
chkMultiWayIfExpr = conditional chkMultiWayIfExpr' MultiWayIf

chkMultiWayIfExpr' :: CheckNode Expr
chkMultiWayIfExpr' e@(MultiIf _) = addEvidence MultiWayIf e
chkMultiWayIfExpr' x = return x
