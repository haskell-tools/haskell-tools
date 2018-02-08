{-# LANGUAGE FlexibleContexts, GADTs, MultiWayIf #-}


module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.FlexibleInstancesChecker where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Data (Data(..))
import Data.List (nubBy)
import Data.Function (on)
import Control.Reference ((^.), (!~), biplateRef)

import Language.Haskell.Tools.Refactor as Refact
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.TypeLookup (lookupSynDefM)

import Name as GHC (isTyVarName, isTyConName, isWiredInName)

{-# ANN module "HLint: ignore Redundant bracket" #-}

-- TODO: write "deriving instance ..." tests (should work)
-- TODO: should expand type synonyms  !!!

-- NOTE: Here we implicitly constrained the type with ExtDomain.
--       but we only really need HasNameInfo.

-- NOTE: We need Decl level checking, in order to distinguish
--       class instances from data and type family instances.

chkFlexibleInstances :: CheckNode Decl
chkFlexibleInstances = conditional chkFlexibleInstances' FlexibleInstances

chkFlexibleInstances' :: CheckNode Decl
chkFlexibleInstances' d@(Refact.StandaloneDeriving _ _ rule) = checkedReturn rule d
chkFlexibleInstances' d@(InstanceDecl rule _)                = checkedReturn rule d
chkFlexibleInstances' d = return d

checkedReturn :: InstanceRule -> a -> ExtMonad a
checkedReturn rule x = chkInstanceRule rule >> return x

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
     (Data.Data.Data (node dom stage), Data.Data.Data (inner dom stage),
      Monad m) =>
     (inner dom stage -> m (inner dom stage))
     -> node dom stage -> m (node dom stage)
refact op = biplateRef !~ op


-- one IHApp will only check its own tyvars (their structure and uniqueness)
-- thus with MultiParamTypeclasses each param will be checked independently
-- (so the same type variable can appear in multiple params)
chkInstanceHead :: CheckNode InstanceHead
chkInstanceHead x@(InfixInstanceHead tyvars _) = do
  tyvars' <- refact rmTypeMisc tyvars
  chkTyVars tyvars'
  addOccurence_ TypeOperators x
  return x
chkInstanceHead app@(AppInstanceHead f tyvars) = do
  tyvars' <- refact rmTypeMisc tyvars
  chkTyVars tyvars'
  chkInstanceHead f
  return app
chkInstanceHead x@(ParenInstanceHead h) = do
  chkInstanceHead h
  return x
chkInstanceHead app = return app

-- TODO: skip other unnecessary parts of the AST (eg.: UType ctors)
-- where can UTyPromoted appear?
-- can i write forall in instance heads?
-- unboxed tuple (has different kind, can't use in ihead), par array?
-- TH ctors
-- other misc ...
-- synonym expansion (runMaybeT . lookupSynDefM $ vars) (now: if synonym, keep FC)
chkTyVars :: CheckNode Type
chkTyVars vars = do
  msyn <- runMaybeT . lookupSynDefM $ vars
  maybe (performCheck vars) (const $ addOccurence FlexibleInstances vars) msyn

  where performCheck vars = do
          (isOk, (_, vs)) <- runStateT (runMaybeT (chkAll vars)) ([],[])
          case isOk of
            Just isOk ->
              unless (isOk && length vs == (length . nubBy ((==) `on` (semanticsName . (^. simpleName))) $ vs)) --tyvars are different
                (addOccurence_ FlexibleInstances vars)
            Nothing   -> error "chkTyVars: Couldn't look up something"
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
          if | isTyVarName   sname -> addTyVarM x >> return False
             | isWiredInName sname -> addTyConM x >> return False
             | isTyConName   sname -> addTyConM x >> return True
             | otherwise           -> return True -- NEVER
        chkUnitTyCon _ = return False


        chkSingleTyVar (VarType x) = do
          sname <- tyVarSemNameM x
          if (isTyVarName sname)
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
        chkOnlyApp x@(InfixTypeApp lhs op rhs) = do
          lift . lift $ addOccurence_ TypeOperators x
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

rmTypeMisc :: CheckNode Type
rmTypeMisc = rmTParens >=> rmTKinded

rmTKinded :: CheckNode Type
rmTKinded (KindedType t _) = return t
rmTKinded x                = return x

-- removes Parentheses from the AST
-- the structure is reserved
rmTParens :: CheckNode Type
rmTParens (ParenType x) = return x
rmTParens x             = return x
