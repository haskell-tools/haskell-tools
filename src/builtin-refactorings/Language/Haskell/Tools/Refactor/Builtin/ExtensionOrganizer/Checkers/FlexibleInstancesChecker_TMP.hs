module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.FlexibleInstancesChecker_TMP where

import qualified GHC
import qualified Class   as GHC
import qualified TcType  as GHC
import qualified Type    as GHC
import qualified TyCoRep as GHC

import Util (equalLength)
import ListSetOps (hasNoDups)

import Data.Maybe (mapMaybe)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.NameLookup
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad hiding (StandaloneDeriving)


chkFlexibleInstancesDecl :: CheckNode Decl
chkFlexibleInstancesDecl = conditional chkFlexibleInstancesDecl' FlexibleInstances

-- | We need to check declarations because we want to avoid checking type family instances
chkFlexibleInstancesDecl' :: CheckNode Decl
chkFlexibleInstancesDecl' d@(StandaloneDeriving _ _ rule) = chkFIRule rule >> return d
chkFlexibleInstancesDecl' d@(InstanceDecl rule _)         = chkFIRule rule >> return d
chkFlexibleInstancesDecl' d = return d

chkFIRule :: CheckNode InstanceRule
chkFIRule = return

chkFIRule' :: InstanceRule -> MaybeT ExtMonad Bool
chkFIRule' rule = return False

collectTypes :: InstanceHead -> [Type]
collectTypes (InstanceHead _)        = []
collectTypes (InfixInstanceHead t _) = [t]
collectTypes (ParenInstanceHead ih)  = collectTypes ih
collectTypes (AppInstanceHead ih t)  = t : collectTypes ih

-- | Decides whether a class instance need FlexibleInstances
-- Checks every single argument independently (in case of MultiParamTypeClasses)
instanceNeedsFI :: GHC.Class -> [GHC.Type] -> Bool
instanceNeedsFI cls args = all hasOnlyDistinctTyVars tyArgs
  where tyArgs = GHC.filterOutInvisibleTypes (GHC.classTyCon cls) args


-- | Checks whether a GHC.Type is an application, and has only (distinct) type variable arguments
-- Logic from TcValidity.tcInstHeadTyAppAllTyVars
hasOnlyDistinctTyVars :: GHC.Type -> Bool
hasOnlyDistinctTyVars ty
  | Just (_, tys) <- GHC.tcSplitTyConApp_maybe (dropCasts ty)
  , tyVars        <- mapMaybe GHC.tcGetTyVar_maybe tys
  = tyVars `equalLength` tys && hasNoDups tyVars
  | otherwise = False


-- | A local helper function in TcValidity
dropCasts :: GHC.Type -> GHC.Type
dropCasts (GHC.CastTy ty _)     = dropCasts ty
dropCasts (GHC.AppTy t1 t2)     = GHC.mkAppTy (dropCasts t1) (dropCasts t2)
dropCasts (GHC.FunTy t1 t2)     = GHC.mkFunTy (dropCasts t1) (dropCasts t2)
dropCasts (GHC.TyConApp tc tys) = GHC.mkTyConApp tc (map dropCasts tys)
dropCasts (GHC.ForAllTy b ty)   = GHC.ForAllTy (dropCastsB b) (dropCasts ty)
dropCasts ty                    = ty

-- | A local helper function in TcValidity
dropCastsB :: GHC.TyVarBinder -> GHC.TyVarBinder
dropCastsB b = b
