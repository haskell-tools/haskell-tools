{-# LANGUAGE TypeApplications #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.TypeFamiliesChecker where

import Data.Maybe (isJust, fromMaybe)
import Control.Reference ((^.), (&))
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Map.Strict as SMap

import Type (expandTypeSynonyms, tyConAppTyCon_maybe, isEqPred, getEqPredTys_maybe, getEqPredTys)
import PrelNames (eqTyConName)
import Unique (hasKey, getUnique)
import qualified GHC (lookupName, synTyConRhs_maybe)

import Data.Generics.ClassyPlate

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.TypeLookup
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.GHCTypeTraversal.AppSelector()
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.GHCTypeTraversal.Checkable()
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.GHCTypeTraversal.ClassyPlate()


import qualified TyCoRep as GHC (Type(..))
import Debug.Trace

debugM :: (Monad m, Show a) => m a -> m a
debugM m = do
  x <- m
  traceShow x m

debug :: Show a => a -> a
debug x = traceShow x x

debugMaybeT :: Monad m => MaybeT m a -> MaybeT m a
debugMaybeT m = MaybeT $ do
  x <- runMaybeT m
  traceShow (isJust x) (return x)

-- | Checks a declaration if TypeFamilies is turned on.
chkTypeFamiliesDecl :: CheckNode Decl
chkTypeFamiliesDecl = conditional chkTypeFamiliesDecl' TypeFamilies

-- | Checks a class element if TypeFamilies is turned on.
chkTypeFamiliesClassElement :: CheckNode ClassElement
chkTypeFamiliesClassElement = conditional chkTypeFamiliesClassElement' TypeFamilies

-- | Checks an instance body if TypeFamilies is turned on.
chkTypeFamiliesInstBodyDecl :: CheckNode InstBodyDecl
chkTypeFamiliesInstBodyDecl = conditional chkTypeFamiliesInstBodyDecl' TypeFamilies

-- | Checks an assertion (needed for a ~ b type equalities) if TypeFamilies is turned on.
chkTypeFamiliesAssertion :: CheckNode Assertion
chkTypeFamiliesAssertion = conditional chkTypeFamiliesAssertion' TypeFamilies

-- | Checks a type for infix type applications (needed for a ~ b type equalities) if TypeFamilies is turned on.
chkTypeFamiliesType :: CheckNode Type
chkTypeFamiliesType = conditional chkTypeFamiliesType' TypeFamilies


chkTypeFamiliesDecl' :: CheckNode Decl
chkTypeFamiliesDecl' d@TypeFamily{}       = addOccurence TypeFamilies d
chkTypeFamiliesDecl' d@DataFamily{}       = addOccurence TypeFamilies d
chkTypeFamiliesDecl' d@ClosedTypeFamily{} = addOccurence TypeFamilies d
chkTypeFamiliesDecl' d@TypeInstance{}     = addOccurence TypeFamilies d
chkTypeFamiliesDecl' d@DataInstance{}     = addOccurence TypeFamilies d
chkTypeFamiliesDecl' d@GadtDataInstance{} = addOccurence TypeFamilies d
chkTypeFamiliesDecl' d                    = return d

chkTypeFamiliesClassElement' :: CheckNode ClassElement
chkTypeFamiliesClassElement' ce@ClassElemTypeFam{} = addOccurence TypeFamilies ce
chkTypeFamiliesClassElement' ce@ClassElemDataFam{} = addOccurence TypeFamilies ce
chkTypeFamiliesClassElement' ce@ClsDefaultType{}   = addOccurence TypeFamilies ce
chkTypeFamiliesClassElement' ce                    = return ce

chkTypeFamiliesInstBodyDecl' :: CheckNode InstBodyDecl
chkTypeFamiliesInstBodyDecl' b@InstanceTypeFamilyDef{}     = addOccurence TypeFamilies b
chkTypeFamiliesInstBodyDecl' b@InstanceDataFamilyDef{}     = addOccurence TypeFamilies b
chkTypeFamiliesInstBodyDecl' b@InstanceDataFamilyGADTDef{} = addOccurence TypeFamilies b
chkTypeFamiliesInstBodyDecl' b                             = return b

chkTypeFamiliesAssertion' :: CheckNode Assertion
chkTypeFamiliesAssertion' a@(InfixAssert _ op _)
  | Just name <- semanticsName (op ^. operatorName)
  , name == eqTyConName
  = addOccurence TypeFamilies a
chkTypeFamiliesAssertion' a@(ClassAssert n _) = do
  traceShow (n ^. simpleName & unqualifiedName & simpleNameStr) (chkTyConName n)
  return a
chkTypeFamiliesAssertion' a = return a

chkTypeFamiliesType' :: CheckNode Type
chkTypeFamiliesType' t@(InfixTypeApp _ op _)
  | Just name <- semanticsName (op ^. operatorName)
  , name == eqTyConName
  = addOccurence TypeFamilies t
chkTypeFamiliesType' t@(VarType n) = do
  traceShow (n ^. simpleName & unqualifiedName & simpleNameStr) (chkTyConName n)
  return t
chkTypeFamiliesType' t = return t

chkTyConName :: CheckNode Name
chkTyConName name = do -- fmap (fromMaybe False) . runMaybeT . eqPredLookup $ name
  mx <- runMaybeT . eqPredLookup $ name
  case mx of
    Nothing -> return name
    Just ty -> do
      exts <- get
      put SMap.empty
      topDownM @Checkable check . expandTypeSynonyms $ ty
      newExts <- get
      traceShow newExts (return ())
      let xs    = zip (SMap.keys newExts) (repeat [getRange name])
          exts' = SMap.unionWith (++) exts (SMap.fromList xs)
      put exts'
      traceShow exts' (return ())
      return name
  where
  --eqPredLookup :: Name -> MaybeT ExtMonad TyCon
  eqPredLookup name = do
    sname <- debugMaybeT . liftMaybe . getSemName $ name
    tt    <- debugMaybeT . MaybeT    . GHC.lookupName $ sname
    tc    <- debugMaybeT . liftMaybe . tyconFromTyThing $ tt
    debugMaybeT . liftMaybe . GHC.synTyConRhs_maybe $ tc
    --debugMaybeT . liftMaybe . tyConAppTyCon_maybe $ rhs

instance Show GHC.Type where
  show GHC.TyVarTy{} = "TyVarTy"
  show GHC.AppTy{} = "AppTy "
  show GHC.TyConApp{} = "TyConApp"
  show GHC.ForAllTy{} = "ForAllTy"
  show GHC.FunTy{} = "FunTy"
  show GHC.LitTy{} = "LitTy"
  show GHC.CastTy{} = "CastTy"
  show GHC.CoercionTy{} = "CoercionTy"
