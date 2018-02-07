module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.ExplicitForAllChecker where

import Control.Reference ((^.))

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkExplicitForAllType :: CheckNode Type
chkExplicitForAllType = conditional chkType ExplicitForAll
  where chkType :: CheckNode Type
        chkType t
          | UTyForall{} <- t ^. element
          = addOccurence ExplicitForAll t
          | otherwise = return t
