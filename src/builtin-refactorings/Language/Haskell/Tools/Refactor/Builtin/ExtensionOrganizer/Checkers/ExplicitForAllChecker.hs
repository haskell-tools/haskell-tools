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
          = addEvidence ExplicitForAll t
          | otherwise = return t

chkExplicitForAllConDecl :: CheckNode ConDecl
chkExplicitForAllConDecl = conditional (chkQuantifiedTyVarsWith (^. conTypeArgs)) ExplicitForAll

chkExplicitForAllGadtConDecl :: CheckNode GadtConDecl
chkExplicitForAllGadtConDecl = conditional (chkQuantifiedTyVarsWith (^. gadtConTypeArgs)) ExplicitForAll

chkQuantifiedTyVarsWith :: HasRange a => (a -> TyVarList) -> CheckNode a
chkQuantifiedTyVarsWith getTyVars conDecl =
  if (annLength . getTyVars $ conDecl) > 0
    then addEvidence ExplicitForAll conDecl
    else return conDecl
