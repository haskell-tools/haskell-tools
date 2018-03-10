module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.UndecidableInstancesChecker where

import Name
import CoAxiom
import FamInstEnv
import InstEnv
import TcType
import GHC hiding (Module, ClassDecl, ClosedTypeFamily)

import Control.Reference ((^.))
import Control.Monad.Trans.Maybe (MaybeT(..))

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.GHCHelpers


-- | If UndecidableInstances is turned on,
-- it checks whether any type class or type family instances needs
-- UndecidableInstances in the module.
gblChkUndecidableInstances :: CheckNode Module
gblChkUndecidableInstances = conditional gblChkUndecidableInstances' UndecidableInstances

-- | Checks whether any type class or type family instances needs
-- UndecidableInstances in the module.
gblChkUndecidableInstances' :: CheckNode Module
gblChkUndecidableInstances' m = do
  (clsInsts, famInsts) <- getInstances [semanticsModule m]
  mapM_ chkClsInst clsInsts
  mapM_ chkFamInst famInsts
  return m

-- | If the type class instance requires UndecidableInstances,
-- it adds the occurence with location of the instance.
chkClsInst :: ClsInst -> ExtMonad ()
chkClsInst inst = when (clsInstNeedsUD inst)
                       (addOccurenceLoc UndecidableInstances (getSrcSpan inst))

-- | Decides whether a type class instance requires UndecidableInstances.
clsInstNeedsUD :: ClsInst -> Bool
clsInstNeedsUD inst = checkInstTermination args theta
  where (_, theta, _, args) = instanceSig inst

-- | If the type class instance requires UndecidableInstances,
-- it adds the occurence with location of the instance.
chkFamInst :: FamInst -> ExtMonad ()
chkFamInst inst = when (famInstNeedsUD inst)
                       (addOccurenceLoc UndecidableInstances (getSrcSpan inst))

-- | Decides whether a family instance requires UndecidableInstances.
-- If it is a data family instance, it does not need the extension.
famInstNeedsUD :: FamInst -> Bool
famInstNeedsUD inst
  | isTyFamInst inst
  , lhs <- fi_tys inst
  , rhs <- tcTyFamInsts . fi_rhs $ inst
  = checkFamEq lhs rhs
  | otherwise = False

-- | Checks a declaration whether it needs UndecidableInstances.
-- If a lookup is not successful, it keeps the extension.
chkUndecidableInstancesDecl :: CheckNode Decl
chkUndecidableInstancesDecl d = do
  mDecl <- runMaybeT (chkUndecidableInstancesDecl' d)
  maybe (addOccurence UndecidableInstances d) return mDecl

-- | Checks a class declaration whether it has a type function in its context,
-- or a closed type family declaration needs UndecidableInstances.
-- For more information on the family check, see checkFamInstRhs.
-- May fail on lookup.
chkUndecidableInstancesDecl' :: Decl -> MaybeT ExtMonad Decl
chkUndecidableInstancesDecl' d@(ClassDecl mCtx _ _ _) = do
  ctx <- liftMaybe $ mCtx ^. annMaybe
  let assert = ctx ^. contextAssertion
      names  = assertionQNames assert
  types <- mapM lookupTypeFromId names
  if any hasTyFunHead types then addOccurence UndecidableInstances d
                            else return d
chkUndecidableInstancesDecl' d@(ClosedTypeFamily dh _ _) = do
  tyFam <- lookupClosedTyFam dh
  let brs    = fromBranches . coAxiomBranches $ tyFam
      famEqs = map ((,) <$> coAxBranchLHS <*> tcTyFamInsts . coAxBranchRHS) brs
  mapM_ (lift . uncurry chkFamEqM) famEqs >> return d
  where
    chkFamEqM :: [GHC.Type] -> [(GHC.TyCon, [GHC.Type])] -> ExtMonad ()
    chkFamEqM lhs rhs = when (checkFamEq lhs rhs)
                             (addOccurence_ UndecidableInstances d)
chkUndecidableInstancesDecl' d = return d
