module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.ConstrainedClassMethodsChecker where

import qualified GHC
import qualified Class  as GHC
import qualified VarSet as GHC
import qualified TcType as GHC

import Control.Monad.Trans.Maybe (MaybeT(..))

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkConstrainedClassMethodsDecl :: CheckNode Decl
chkConstrainedClassMethodsDecl = conditional chkCCMDecl ConstrainedClassMethods
  where chkCCMDecl cd@(ClassDecl _ dh _ _) = chkCCMDeclHead dh >> return cd
        chkCCMDecl x = return x

-- | Check a DeclHead for ConstrainedClassMethods.
-- Adds the extension if it is needed or the lookup fails.
chkCCMDeclHead :: CheckNode DeclHead
chkCCMDeclHead dh = do
  mNeedsCCM <- runMaybeT . chkCCMDeclHead' $ dh
  case mNeedsCCM of
    Just False -> return dh
    _          -> addEvidence ConstrainedClassMethods dh


-- | Helper function for chkCCMDeclHead.
-- True  <=> Lookup is succesful and ConstrainedClassMethods is needed
-- False <=> Lookup is succesful, but CCM is not needed, or the argument is not a class DeclHead
-- fails <=> Lookup is unsuccesful (either name or type lookup)
chkCCMDeclHead' :: DeclHead -> MaybeT ExtMonad Bool
chkCCMDeclHead' dh = do
  sname   <- liftMaybe . declHeadSemName $ dh
  tything <- MaybeT . GHC.lookupName $ sname
  case tything of
    GHC.ATyCon tc | GHC.isClassTyCon tc -> liftMaybe . fmap classNeedsCCM . GHC.tyConClass_maybe $ tc
    _ -> return False


-- | Decides whether a class really needs the ConstrainedClassMethods extension
-- A class needs CCM iff at least one of its class methods
-- has a constraint with a non-empty type variable set, that contains only class type variables.
classNeedsCCM :: GHC.Class -> Bool
classNeedsCCM cls = any methodNeedsCCM methods
  where
    methods     = GHC.classMethods cls
    tyvars      = GHC.classTyVars cls
    clsTyVarSet = GHC.mkVarSet tyvars

    methodNeedsCCM :: GHC.Id -> Bool
    methodNeedsCCM methodId = any constraintNeedsCCM constraints
      where
        (_,_,tau)         = GHC.tcSplitMethodTy . GHC.idType $ methodId
        (_,constraints,_) = GHC.tcSplitNestedSigmaTys tau

        constraintNeedsCCM :: GHC.TcPredType -> Bool
        constraintNeedsCCM pred = not (GHC.isEmptyVarSet predTyVars)
                                  && predTyVars `GHC.subVarSet` clsTyVarSet
          where predTyVars = GHC.tyCoVarsOfType pred
