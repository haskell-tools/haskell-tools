module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.ArrowsChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkArrowsExpr :: CheckNode Expr
-- chkArrowsExpr e@Proc{}     = addEvidence Arrows e
chkArrowsExpr e@ArrowApp{} = addEvidence Arrows e
chkArrowsExpr e            = return e

chkArrowsCmd :: CheckNode Cmd
chkArrowsCmd = addEvidence Arrows
