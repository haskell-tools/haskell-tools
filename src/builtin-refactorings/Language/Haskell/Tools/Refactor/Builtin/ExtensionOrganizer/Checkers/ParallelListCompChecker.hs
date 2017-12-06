module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.ParallelListCompChecker where

import Language.Haskell.Tools.AST  (annLength)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkParallelListComp :: CheckNode Expr
chkParallelListComp e@(ListComp _ body)
  | annLength body > 1 = addOccurence ParallelListComp e
chkParallelListComp e = return e
