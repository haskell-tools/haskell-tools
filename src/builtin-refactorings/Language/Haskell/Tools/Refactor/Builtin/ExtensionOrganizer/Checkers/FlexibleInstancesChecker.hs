{-# LANGUAGE FlexibleContexts, GADTs, MultiWayIf #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.FlexibleInstancesChecker where

import qualified GHC
import qualified Class   as GHC
import qualified TcType  as GHC
import qualified Type    as GHC
import qualified TyCoRep as GHC
import qualified Name    as GHC (isTyVarName, isTyConName, isWiredInName)

import Util (equalLength)
import ListSetOps (hasNoDups)

import Data.Maybe (mapMaybe)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Data (Data(..))
import Data.List (nubBy)
import Data.Function (on)
import Control.Reference ((^.), (.-), biplateRef)

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad hiding (StandaloneDeriving)
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.TypeLookup


{-# ANN module "HLint: ignore Redundant bracket" #-}

-- TODO: write "deriving instance ..." tests (should work)
-- TODO: should expand type synonyms  !!!

-- | We need to check declarations because we want to avoid checking type family instances
chkFlexibleInstancesDecl :: CheckNode Decl
chkFlexibleInstancesDecl = conditional chkFlexibleInstancesDecl' FlexibleInstances

-- | We need to check declarations because we want to avoid checking type family instances
chkFlexibleInstancesDecl' :: CheckNode Decl
chkFlexibleInstancesDecl' d@(StandaloneDeriving _ _ rule) = chkInstanceRule rule >> return d
chkFlexibleInstancesDecl' d@(InstanceDecl rule _)         = chkInstanceRule rule >> return d
chkFlexibleInstancesDecl' d = return d

-- this check DOES transform the AST for its internal computations
-- but returns the original one in the end
-- NOTE: There are two traversals:
--       First one on the class level, and the second one one on the type level.
--       Since biplateRef is lazy, it won't go down to the type level in the first traversal
chkInstanceRule :: CheckNode InstanceRule
chkInstanceRule r@(InstanceRule _ _ ihead) = do
  chkInstanceHead ihead
  return $! r
chkInstanceRule r = return r

refact ::
     (Data.Data.Data (node dom stage), Data.Data.Data (inner dom stage)) =>
     (inner dom stage -> inner dom stage) ->
      node dom stage -> node dom stage
refact op = biplateRef .- op


-- | Checks every single type argument in an instance declaration
-- If the class being instantiated could not have been looked up, it keeps FlexibleInstances
chkInstanceHead :: CheckNode InstanceHead
chkInstanceHead ih = do
  let types = collectTyArgs ih
  mCls <- runMaybeT . lookupClassFromInstance $ ih
  case mCls of
    Just cls -> mapM_ (chkTypeArg cls) types >> return ih
    Nothing  -> addOccurence FlexibleInstances ih

-- | Checks a type argument of class whether it needs FlexibleInstances
-- First checks the structure of the type argument opaquely
-- Then, for type synonyms, it checks whether their GHC representation itself
-- needs the extension.
--
-- Might find false positive occurences for phantom types:
-- > type Phatom a b = [a]
-- > instance C (Phatnom a a)
-- > instance C (Phantom a Int)
chkTypeArg :: GHC.Class -> Type -> ExtMonad Type
chkTypeArg cls ty = do
  chkNormalTypeArg ty
  maybeTM (return ty) (chkSynonymTypeArg cls) (semanticsTypeSynRhs ty)
  where chkSynonymTypeArg :: GHC.Class -> GHC.Type -> ExtMonad Type
        chkSynonymTypeArg cls' ty'
          | tyArgNeedsFI cls' ty' = addOccurence FlexibleInstances ty
          | otherwise             = return ty

-- | Checks a type argument of class whether it has only (distinct) type variable arguments.
chkNormalTypeArg :: CheckNode Type
chkNormalTypeArg vars = performCheck . refact rmTypeMisc $ vars

  where performCheck vars = do
          (isOk, (_, vs)) <- runStateT (runMaybeT (chkAll vars)) ([],[])
          case isOk of
            Just isOk ->
              unless (isOk && length vs == (length . nubBy ((==) `on` (semanticsName . (^. simpleName))) $ vs)) --tyvars are different
                (addOccurence_ FlexibleInstances vars)
            Nothing   -> error "chkNormalTypeArg: Couldn't look up something"
          return vars

        chkAll x =
          ifM (chkTopLevel x) $
            chkOnlyApp x

        chkTopLevel x = -- NOTE: this resembles a monadic bind ... (Cont?)
          ifM (chkListType x) .
            ifM (chkTupleType x) .
              ifM (chkUnitTyCon x) $
                return False

        ifM cond f = do b <- cond; if b then (return b) else f

        chkUnitTyCon (VarType x) = do
          sname <- tyVarSemNameM x
          -- standalone top-level type variables are not accepted
          -- NOTE: -XHaskell98 operator type variables??
          -- NOTE VarType is either TyCon or TyVar
          --      if it is a TyCon, it cannot be wired in (Int, Char, etc)
          if | GHC.isTyVarName   sname -> addTyVarM x >> return False
             | GHC.isWiredInName sname -> addTyConM x >> return False
             | GHC.isTyConName   sname -> addTyConM x >> return True
             | otherwise               -> return True -- NEVER
        chkUnitTyCon _ = return False


        chkSingleTyVar (VarType x) = do
          sname <- tyVarSemNameM x
          if (GHC.isTyVarName sname)
            then addTyVarM x >> return True
            else addTyConM x >> return False
        chkSingleTyVar _ = return False


        chkTupleType (TupleType args) = do
          let xs  = args ^. annListElems
          bs <- mapM chkSingleTyVar xs
          return $! and bs
        chkTupleType _ = return False

        chkListType (ListType v) = chkSingleTyVar v
        chkListType _            = return False

        chkOnlyApp :: (MonadState ([Name],[Name]) (m1 m2),
                       MonadTrans m1,
                       MonadState ExtMap m2) =>
                       Type -> MaybeT (m1 m2) Bool
        chkOnlyApp (TypeApp f v@(VarType _)) = do
          isTyVar <- chkSingleTyVar v
          if isTyVar
            then case f of
              (VarType c) -> addTyConM c >> return True
              _           -> chkOnlyApp f
            else return False
        chkOnlyApp (InfixTypeApp lhs op rhs) = do
          addTyConM . mkNormalName $ (op ^. operatorName)
          lOK <- chkSingleTyVar lhs
          rOK <- chkSingleTyVar rhs
          return $! lOK && rOK
        chkOnlyApp _ = return False

        addTyCon  n (ctors, vars) = (n:ctors, vars)
        addTyVar  n (ctors, vars) = (ctors, n:vars)
        addTyConM n               = modify $ addTyCon n
        addTyVarM n               = modify $ addTyVar  n

        tyVarSemNameM x = MaybeT . return . semanticsName $ x ^. simpleName

rmTypeMisc :: Type -> Type
rmTypeMisc (KindedType t _) = t
rmTypeMisc (ParenType x)    = x
rmTypeMisc x                = x

-- | Collects the type arguments in an instance declaration
-- Type arguments are the the types that the class is being instantiated with
collectTyArgs :: InstanceHead -> [Type]
collectTyArgs (InstanceHead _)        = []
collectTyArgs (InfixInstanceHead t _) = [t]
collectTyArgs (ParenInstanceHead ih)  = collectTyArgs ih
collectTyArgs (AppInstanceHead ih t)  = t : collectTyArgs ih

-- Decides whether a type argument of a type class constructor need FlexibleInstances
tyArgNeedsFI :: GHC.Class -> GHC.Type -> Bool
tyArgNeedsFI cls arg = not . hasOnlyDistinctTyVars $ tyArg
  where [tyArg] = GHC.filterOutInvisibleTypes (GHC.classTyCon cls) [arg]

-- | Checks whether a GHC.Type is an application, and has only (distinct) type variable arguments
-- Logic from TcValidity.tcInstHeadTyAppAllTyVars
hasOnlyDistinctTyVars :: GHC.Type -> Bool
hasOnlyDistinctTyVars ty
  | Just (tc, tys) <- GHC.tcSplitTyConApp_maybe (dropCasts ty)
  , tys'           <- GHC.filterOutInvisibleTypes tc tys
  , tyVars         <- mapMaybe GHC.tcGetTyVar_maybe tys'
  = tyVars `equalLength` tys' && hasNoDups tyVars
  | otherwise = False


-- | A local helper function from TcValidity
dropCasts :: GHC.Type -> GHC.Type
dropCasts (GHC.CastTy ty _)     = dropCasts ty
dropCasts (GHC.AppTy t1 t2)     = GHC.mkAppTy (dropCasts t1) (dropCasts t2)
dropCasts (GHC.FunTy t1 t2)     = GHC.mkFunTy (dropCasts t1) (dropCasts t2)
dropCasts (GHC.TyConApp tc tys) = GHC.mkTyConApp tc (map dropCasts tys)
dropCasts (GHC.ForAllTy b ty)   = GHC.ForAllTy (dropCastsB b) (dropCasts ty)
dropCasts ty                    = ty

-- | A local helper function from TcValidity
dropCastsB :: GHC.TyVarBinder -> GHC.TyVarBinder
dropCastsB b = b
