module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.ExplicitNamespacesChecker where

import Control.Reference ((^.))

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkExplicitNamespacesIESpec :: CheckNode IESpec
chkExplicitNamespacesIESpec = conditional chkIESpec ExplicitNamespaces
  where chkIESpec :: CheckNode IESpec
        chkIESpec x@(IESpec (AnnJust imod) _ _)
          | UImportType <- imod ^. element = addOccurence ExplicitNamespaces x
        chkIESpec x = return x
