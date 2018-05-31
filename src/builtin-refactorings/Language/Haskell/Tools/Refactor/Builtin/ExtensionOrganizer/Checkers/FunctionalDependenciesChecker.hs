module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.FunctionalDependenciesChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkFunDeps :: CheckNode FunDepList
chkFunDeps = addEvidence FunctionalDependencies
