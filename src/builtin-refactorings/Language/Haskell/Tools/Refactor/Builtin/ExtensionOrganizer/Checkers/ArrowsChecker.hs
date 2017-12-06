module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.ArrowsChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkArrowsExpr :: CheckNode Expr
-- chkArrowsExpr e@Proc{}     = addOccurence Arrows e
chkArrowsExpr e@ArrowApp{} = addOccurence Arrows e
chkArrowsExpr e            = return e

chkArrowsCmd :: CheckNode Cmd
chkArrowsCmd = addOccurence Arrows
