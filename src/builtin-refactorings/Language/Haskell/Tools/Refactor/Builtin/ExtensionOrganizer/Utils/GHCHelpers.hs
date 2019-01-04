module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.GHCHelpers where

import Class
import FamInstEnv (FamFlavor(..),FamInst(..))
import PrelNames
import TcType hiding (sizeType, sizeTypes)
import TyCoRep
import Type
import Var
import GHC

import Data.List

-- This module contains private helper functions from GHC (mostly from TcValidity).
-- There are other similar functions that are accessible through the API,
-- but these ones have slighty different behaviour.

-- Free variables of a type, retaining repetitions, and expanding synonyms
fvType :: Type -> [TyCoVar]
fvType ty | Just exp_ty <- tcView ty = fvType exp_ty
fvType (TyVarTy tv)          = [tv]
fvType (TyConApp _ tys)      = fvTypes tys
fvType LitTy{}               = []
fvType (AppTy fun arg)       = fvType fun ++ fvType arg
fvType (FunTy arg res)       = fvType arg ++ fvType res
fvType (ForAllTy (TvBndr tv _) ty)
  = fvType (tyVarKind tv) ++
    filter (/= tv) (fvType ty)
fvType (CastTy ty co)        = fvType ty ++ fvCo co
fvType (CoercionTy co)       = fvCo co

fvTypes :: [Type] -> [TyVar]
fvTypes = concatMap fvType

fvCo :: Coercion -> [TyCoVar]
fvCo (Refl _ ty)            = fvType ty
fvCo (TyConAppCo _ _ args)  = concatMap fvCo args
fvCo (AppCo co arg)         = fvCo co ++ fvCo arg
fvCo (ForAllCo tv h co)     = filter (/= tv) (fvCo co) ++ fvCo h
fvCo (FunCo _ co1 co2)      = fvCo co1 ++ fvCo co2
fvCo (CoVarCo v)            = [v]
fvCo (AxiomInstCo _ _ args) = concatMap fvCo args
fvCo (UnivCo p _ t1 t2)     = fvProv p ++ fvType t1 ++ fvType t2
fvCo (SymCo co)             = fvCo co
fvCo (TransCo co1 co2)      = fvCo co1 ++ fvCo co2
fvCo (NthCo _ _ co)           = fvCo co
fvCo (LRCo _ co)            = fvCo co
fvCo (InstCo co arg)        = fvCo co ++ fvCo arg
fvCo (CoherenceCo co1 co2)  = fvCo co1 ++ fvCo co2
fvCo (KindCo co)            = fvCo co
fvCo (SubCo co)             = fvCo co
fvCo (AxiomRuleCo _ cs)     = concatMap fvCo cs

fvProv :: UnivCoProvenance -> [TyCoVar]
fvProv UnsafeCoerceProv    = []
fvProv (PhantomProv co)    = fvCo co
fvProv (ProofIrrelProv co) = fvCo co
fvProv (PluginProv _)      = []

sizeType :: Type -> Int
-- Size of a type: the number of variables and constructors
sizeType ty | Just exp_ty <- tcView ty = sizeType exp_ty
sizeType TyVarTy{}         = 1
sizeType (TyConApp _ tys)  = sizeTypes tys + 1
sizeType LitTy{}           = 1
sizeType (AppTy fun arg)   = sizeType fun + sizeType arg
sizeType (FunTy arg res)   = sizeType arg + sizeType res + 1
sizeType (ForAllTy _ ty)   = sizeType ty
sizeType (CastTy ty _)     = sizeType ty
sizeType (CoercionTy _)    = 1

sizeTypes :: [Type] -> Int
sizeTypes = sum . map sizeType


checkInstTermination :: [TcType] -> ThetaType -> Bool
checkInstTermination tys = checkPreds
  where
   headFvs  = fvTypes tys
   headSize = sizeTypes tys

   checkPreds :: [PredType] -> Bool
   checkPreds = any check

   check :: PredType -> Bool
   check predTy
     = case classifyPredType predTy of
         EqPred {}    -> False
         IrredPred {} -> check2 predTy (sizeType predTy)
         ClassPred cls tys
           | isTerminatingClass cls -> False
           | isCTupleClass cls -> checkPreds tys
           | otherwise
           -> check2 predTy (sizeTypes . filterOutInvisibleTypes (classTyCon cls) $ tys)

   check2 predTy predSize = not (null badTvs) || predSize >= headSize
     where badTvs = fvType predTy \\ headFvs
     -- Tyvars occurring more often in the context than in the head

isTerminatingClass :: Class -> Bool
isTerminatingClass cls = isIPClass cls
  || cls `hasKey` typeableClassKey
  || cls `hasKey` coercibleTyConKey
  || cls `hasKey` eqTyConKey
  || cls `hasKey` heqTyConKey


-- | Checks whether a Type has a type function application in its head.
hasTyFunHead :: Type -> Bool
hasTyFunHead ty = case tcSplitTyConApp_maybe ty of
                    Just (tc, _) -> isTypeFamilyTyCon tc
                    Nothing      -> False

-- | Given the lhs type arguments and rhs tycon and its arguments
-- of a type family instance, it determines whether it needs UndecidableInstances.
checkFamEq :: [Type] -> [(TyCon, [Type])] -> Bool
checkFamEq lhsTys = any check
  where
   size = sizeTypes lhsTys
   fvs  = fvTypes lhsTys
   check (_, tys)
     | bad_tvs <- fvTypes tys \\ fvs
     = not (all isTyFamFree tys) || not (null bad_tvs) || size <= sizeTypes tys

isTyFamInst :: FamInst -> Bool
isTyFamInst inst
  | SynFamilyInst <- fi_flavor inst = True
  | otherwise                       = False
