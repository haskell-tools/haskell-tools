{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.TypeLookup where


import Language.Haskell.Tools.AST (simpleName)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

import Control.Reference ((^.))
import Control.Monad.Trans.Maybe (MaybeT(..))

import qualified GHC     hiding (typeKind)
import qualified TyCoRep as GHC (Type(..), TyThing(..))
import qualified Kind    as GHC (isConstraintKind, typeKind)
import qualified ConLike as GHC (ConLike(..))
import qualified DataCon as GHC (dataConUserType)
import qualified PatSyn  as GHC (patSynBuilder)
import qualified Var     as GHC (varType)



hasConstraintKind :: GHC.Type -> Bool
hasConstraintKind = GHC.isConstraintKind . GHC.typeKind


chkSynonym :: CheckNode Type
chkSynonym t = do
  mtycon <- runMaybeT . lookupType $ t
  case mtycon of
    Nothing    -> return t
    Just tycon -> chkSynonym' tycon
  where chkSynonym' x = case lookupSynDef x of
                          Nothing -> return t
                          Just _  -> addOccurence TypeSynonymInstances t

-- | Looks up a GHC Type from a Haskell Tools Name
-- For an identifier, it returns its type.
-- For a data constructor, it returns its type.
-- For a pattern synonym, it returns its builder's type.
-- For a type synonym constructor, it returns its right-hand side.
-- For a coaxiom, it fails.
lookupTypeFromName :: HasNameInfo' n => n -> MaybeT ExtMonad GHC.Type
lookupTypeFromName name = do
  sname <- liftMaybe . getSemName $ name
  tt    <- MaybeT    . GHC.lookupName $ sname
  case tt of
    GHC.AnId     idn  -> return . GHC.varType $ idn
    GHC.AConLike con  -> handleCon con
    GHC.ATyCon   tc   -> liftMaybe . GHC.synTyConRhs_maybe $ tc
    GHC.ACoAxiom coax -> fail "CoAxioms are not supported for type lookup"
  where handleCon (GHC.RealDataCon dc) = return . GHC.dataConUserType $ dc
        handleCon (GHC.PatSynCon   pc) = do
          (idn,_) <- liftMaybe . GHC.patSynBuilder $ pc
          return . GHC.varType $ idn


-- | Looks up a GHC Type from a Haskell Tools Name (if available)
lookupTypeSynRhs :: Name -> MaybeT ExtMonad GHC.Type
lookupTypeSynRhs name = do
  sname <- liftMaybe . getSemName $ name
  tt    <- MaybeT    . GHC.lookupName $ sname
  tc    <- liftMaybe . tyconFromTyThing $ tt
  liftMaybe . GHC.synTyConRhs_maybe $ tc

lookupSynDefM :: Type -> MaybeT ExtMonad GHC.TyCon
lookupSynDefM t = do
  tything <- lookupType t
  liftMaybe $ lookupSynDef tything
  where liftMaybe = MaybeT . return

-- NOTE: Returns Nothing if it is not a type synonym
lookupSynDef :: GHC.TyThing -> Maybe GHC.TyCon
lookupSynDef syn = do
  tycon <- tyconFromTyThing syn
  rhs   <- GHC.synTyConRhs_maybe tycon
  tyconFromGHCType rhs

tyconFromTyThing :: GHC.TyThing -> Maybe GHC.TyCon
tyconFromTyThing (GHC.ATyCon tycon) = Just tycon
tyconFromTyThing _ = Nothing

-- won't bother
tyconFromGHCType :: GHC.Type -> Maybe GHC.TyCon
tyconFromGHCType (GHC.AppTy t1 _) = tyconFromGHCType t1
tyconFromGHCType (GHC.TyConApp tycon _) = Just tycon
tyconFromGHCType _ = Nothing


-- NOTE: Returns false if the type is certainly not a newtype
--       Returns true if it is a newtype or it could not have been looked up
isNewtype :: Type -> ExtMonad Bool
isNewtype t = do
  tycon <- runMaybeT . lookupType $ t
  return $! maybe True isNewtypeTyCon tycon



lookupType :: Type -> MaybeT ExtMonad GHC.TyThing
lookupType t = do
  name  <- liftMaybe . nameFromType $ t
  sname <- liftMaybe . getSemName   $ name
  MaybeT . GHC.lookupName $ sname

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

-- NOTE: gives just name if the type being scrutinised can be newtype
--       else it gives nothing
nameFromType :: Type -> Maybe Name
nameFromType (TypeApp f _)    = nameFromType f
nameFromType (ParenType x)    = nameFromType x
nameFromType (KindedType t _) = nameFromType t
nameFromType (VarType x)      = Just x
nameFromType _                = Nothing

isNewtypeTyCon :: GHC.TyThing -> Bool
isNewtypeTyCon (GHC.ATyCon tycon) = GHC.isNewTyCon tycon
isNewtypeTyCon _ = False

getSemName :: HasNameInfo' n => n -> Maybe GHC.Name
getSemName = semanticsName
