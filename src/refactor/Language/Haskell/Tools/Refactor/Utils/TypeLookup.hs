{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Language.Haskell.Tools.Refactor.Utils.TypeLookup where

import qualified TyCoRep   as GHC (Type(..), TyThing(..))
import qualified Kind      as GHC (isConstraintKind, typeKind)
import qualified ConLike   as GHC (ConLike(..))
import qualified DataCon   as GHC (dataConUserType, isVanillaDataCon)
import qualified PatSyn    as GHC (patSynBuilder)
import qualified Var       as GHC (varType)
import qualified GHC       hiding (typeKind)
import           GHC       (GhcMonad)

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite
import Language.Haskell.Tools.Refactor.Utils.NameLookup
import Language.Haskell.Tools.Refactor.Utils.Maybe


hasConstraintKind :: GHC.Type -> Bool
hasConstraintKind = GHC.isConstraintKind . GHC.typeKind


-- | Looks up the Type of an entity with an Id of any locality.
-- If the entity being scrutinised is a type variable, it fails.
lookupTypeFromId :: (HasIdInfo' id, GhcMonad m) => id -> MaybeT m GHC.Type
lookupTypeFromId idn
  | GHC.isLocalId  . semanticsId $ idn = return . typeOrKindFromId $ idn
  | GHC.isGlobalId . semanticsId $ idn = lookupTypeFromGlobalName idn
  | otherwise = fail "Couldn't lookup name"

-- | Looks up the Type or the Kind of an entity that has an Id.
-- Note: In some cases we only get the Kind of the Id (e.g. for type constructors)
typeOrKindFromId :: HasIdInfo' id => id -> GHC.Type
typeOrKindFromId idn = GHC.varType . semanticsId $ idn

-- | Extracts a Type from a TyThing when possible.
typeFromTyThing :: GHC.TyThing -> Maybe GHC.Type
typeFromTyThing (GHC.AnId     idn)  = Just . GHC.varType $ idn
typeFromTyThing (GHC.ATyCon   tc)   = GHC.synTyConRhs_maybe tc
typeFromTyThing (GHC.ACoAxiom _)    = fail "CoAxioms are not supported for type lookup"
typeFromTyThing (GHC.AConLike con)  = handleCon con
  where handleCon (GHC.RealDataCon dc) = Just . GHC.dataConUserType $ dc
        handleCon (GHC.PatSynCon   pc) = do
          (idn,_) <- GHC.patSynBuilder pc
          return . GHC.varType $ idn

-- | Looks up a GHC Type from a Haskell Tools Name (given the name is global)
-- For an identifier, it returns its type.
-- For a data constructor, it returns its type.
-- For a pattern synonym, it returns its builder's type.
-- For a type synonym constructor, it returns its right-hand side.
-- For a coaxiom, it fails.
lookupTypeFromGlobalName :: (HasNameInfo' n, GhcMonad m) => n -> MaybeT m GHC.Type
lookupTypeFromGlobalName name = do
  sname <- liftMaybe . semanticsName $ name
  tt    <- MaybeT    . GHC.lookupName $ sname
  liftMaybe . typeFromTyThing $ tt


-- | Looks up the right-hand side (GHC representation)
-- of a Haskell Tools Name corresponding to a type synonym
lookupTypeSynRhs :: (HasNameInfo' n, GhcMonad m) => n -> MaybeT m GHC.Type
lookupTypeSynRhs name = do
  sname <- liftMaybe . semanticsName $ name
  tt    <- MaybeT    . GHC.lookupName $ sname
  tc    <- liftMaybe . tyconFromTyThing $ tt
  liftMaybe . GHC.synTyConRhs_maybe $ tc

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
isNewtype :: GhcMonad m => Type -> m Bool
isNewtype t = do
  tycon <- runMaybeT . lookupType $ t
  return $! maybe True isNewtypeTyCon tycon



lookupType :: GhcMonad m => Type -> MaybeT m GHC.TyThing
lookupType t = do
  name  <- liftMaybe . nameFromType $ t
  sname <- liftMaybe . semanticsName   $ name
  MaybeT . GHC.lookupName $ sname

-- | Looks up a GHC.Class from something that has a type class constructor in it
-- Fails if the argument does not contain a class type constructor
lookupClassWith :: GhcMonad m => (a -> MaybeT m GHC.Name) -> a -> MaybeT m GHC.Class
lookupClassWith getName x = do
  sname   <- getName x
  tything <- MaybeT . GHC.lookupName $ sname
  case tything of
    GHC.ATyCon tc | GHC.isClassTyCon tc -> liftMaybe . GHC.tyConClass_maybe $ tc
    _ -> fail "TypeLookup.lookupClassWith: Argument does not contain a class type constructor"

lookupClassFromInstance :: GhcMonad m => InstanceHead -> MaybeT m GHC.Class
lookupClassFromInstance = lookupClassWith instHeadSemName

lookupClassFromDeclHead :: GhcMonad m => DeclHead -> MaybeT m GHC.Class
lookupClassFromDeclHead = lookupClassWith declHeadSemName

-- | Looks up the right-hand side (GHC representation)
-- of a Haskell Tools Type corresponding to a type synonym
semanticsTypeSynRhs :: GhcMonad m => Type -> MaybeT m GHC.Type
semanticsTypeSynRhs ty = (liftMaybe . nameFromType $ ty) >>= lookupTypeSynRhs

-- | Converts a global Haskell Tools type to a GHC type
semanticsType :: GhcMonad m => Type -> MaybeT m GHC.Type
semanticsType ty = (liftMaybe . nameFromType $ ty) >>= lookupTypeFromGlobalName

-- | Extracts the name of a type
-- In case of a type application, it finds the type being applied
nameFromType :: Type -> Maybe Name
nameFromType (TypeApp f _)    = nameFromType f
nameFromType (ParenType x)    = nameFromType x
nameFromType (KindedType t _) = nameFromType t
nameFromType (BangType t)     = nameFromType t
nameFromType (LazyType t)     = nameFromType t
nameFromType (UnpackType t)   = nameFromType t
nameFromType (NoUnpackType t) = nameFromType t
nameFromType (VarType x)      = Just x
nameFromType _                = Nothing

isNewtypeTyCon :: GHC.TyThing -> Bool
isNewtypeTyCon (GHC.ATyCon tycon) = GHC.isNewTyCon tycon
isNewtypeTyCon _ = False

-- | Decides whether a given name is a standard Haskell98 data constructor.
-- Fails if not given a proper name.
isVanillaDataConNameM :: (HasNameInfo' n, GhcMonad m) => n -> MaybeT m Bool
isVanillaDataConNameM name = do
  sname <- liftMaybe . semanticsName  $ name
  tt    <- MaybeT    . GHC.lookupName $ sname
  dc    <- liftMaybe . extractDataCon $ tt
  return . GHC.isVanillaDataCon $ dc
  where extractDataCon (GHC.AConLike (GHC.RealDataCon dc)) = Just dc
        extractDataCon  _                                  = Nothing
