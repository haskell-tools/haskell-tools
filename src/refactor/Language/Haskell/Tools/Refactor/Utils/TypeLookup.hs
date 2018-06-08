{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Tools.Refactor.Utils.TypeLookup where

import qualified TyCoRep   as GHC (Type(..), TyThing(..))
import qualified Kind      as GHC (isConstraintKind)
import qualified ConLike   as GHC (ConLike(..))
import qualified DataCon   as GHC (dataConUserType, isVanillaDataCon)
import qualified Kind      as GHC (isConstraintKind)
import qualified Name      as GHC (isTyVarName)
import qualified PatSyn    as GHC (patSynBuilder)
import qualified TyCon     as GHC (isClosedSynFamilyTyConWithAxiom_maybe, isClassTyCon)
import qualified TyCoRep   as GHC (Type(..), TyThing(..))
import qualified Type      as GHC (eqType, typeKind)
import qualified Var       as GHC (varType)
import qualified CoAxiom   as GHC
import qualified GHC       hiding (typeKind)
import           GHC       (GhcMonad)

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.Rewrite as AST
import Language.Haskell.Tools.Refactor.Utils.NameLookup as AST
import Language.Haskell.Tools.Refactor.Utils.Maybe as AST

instance Eq GHC.Type where
  (==) = GHC.eqType

type ClosedTyFam = GHC.CoAxiom GHC.Branched

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
isNewtype :: GhcMonad m => AST.Type -> m Bool
isNewtype t = do
  tycon <- runMaybeT . lookupType $ t
  return $! maybe True isNewtypeTyCon tycon



lookupType :: GhcMonad m => AST.Type -> MaybeT m GHC.TyThing
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

lookupClass :: (GhcMonad m, HasNameInfo' n) => n -> MaybeT m GHC.Class
lookupClass = lookupClassWith (liftMaybe . semanticsName)

lookupClassFromInstance :: GhcMonad m => InstanceHead -> MaybeT m GHC.Class
lookupClassFromInstance = lookupClassWith (liftMaybe . instHeadSemName)

lookupClassFromDeclHead :: GhcMonad m => DeclHead -> MaybeT m GHC.Class
lookupClassFromDeclHead = lookupClassWith (liftMaybe . declHeadSemName)

-- | Looks up the right-hand side (GHC representation)
-- of a Haskell Tools Type corresponding to a type synonym
semanticsTypeSynRhs :: GhcMonad m => AST.Type -> MaybeT m GHC.Type
semanticsTypeSynRhs ty = (liftMaybe . nameFromType $ ty) >>= lookupTypeSynRhs

-- | Converts a global Haskell Tools type to a GHC type
semanticsType :: GhcMonad m => AST.Type -> MaybeT m GHC.Type
semanticsType ty = (liftMaybe . nameFromType $ ty) >>= lookupTypeFromGlobalName

isNewtypeTyCon :: GHC.TyThing -> Bool
isNewtypeTyCon (GHC.ATyCon tycon) = GHC.isNewTyCon tycon
isNewtypeTyCon _ = False

-- | Looks up the given name, extracts something out of it.
-- If the extraction is not succesful, it returns False,
-- if it is successful, then checks the result against the predicate.
-- The reasoning behind this, is that the predicate can only be
-- satisfied by a proper name.
satisfies :: (HasNameInfo' n, GhcMonad m) =>
             (GHC.TyThing -> Maybe a) -> (a -> Bool) -> n -> MaybeT m Bool
satisfies extract pred name = do
  sname <- liftMaybe . semanticsName  $ name
  tt    <- MaybeT    . GHC.lookupName $ sname
  return $ maybe False pred (extract tt)

-- | Decides whether a given name is a type family constructor.
-- Fails if the lookup is not successful.
isClassTyConNameM :: (HasNameInfo' n, GhcMonad m) => n -> MaybeT m Bool
isClassTyConNameM = satisfies extractTyCon GHC.isClassTyCon
  where extractTyCon (GHC.ATyCon tc) = Just tc
        extractTyCon  _              = Nothing

-- | Decides whether a given name is a standard Haskell98 data constructor.
-- Fails if the lookup is not successful.
isVanillaDataConNameM :: (HasNameInfo' n, GhcMonad m) => n -> MaybeT m Bool
isVanillaDataConNameM = satisfies extractDataCon GHC.isVanillaDataCon
  where extractDataCon (GHC.AConLike (GHC.RealDataCon dc)) = Just dc
        extractDataCon  _                                  = Nothing

-- | Looks up a closed type family from a name.
lookupClosedTyFam :: (HasNameInfo' n, GhcMonad m) => n -> MaybeT m ClosedTyFam
lookupClosedTyFam name = do
  sname <- liftMaybe . semanticsName $ name
  tt    <- MaybeT    . GHC.lookupName $ sname
  liftMaybe . coAxiomFromTyThing $ tt

-- | Extract the CoAxioms from a TyThing representing a closed type family.
coAxiomFromTyThing :: GHC.TyThing -> Maybe (GHC.CoAxiom GHC.Branched)
coAxiomFromTyThing (GHC.ATyCon tc)   = GHC.isClosedSynFamilyTyConWithAxiom_maybe tc
coAxiomFromTyThing (GHC.ACoAxiom ax) = Just ax
coAxiomFromTyThing _                 = Nothing

-- | Determines whether a Type itself has a type variable head.
hasTyVarHead :: Type -> Bool
hasTyVarHead (ForallType _ t) = hasTyVarHead t
hasTyVarHead (CtxType _ t) = hasTyVarHead t
hasTyVarHead FunctionType{} = False
hasTyVarHead TupleType{} = False
hasTyVarHead UnboxedTupleType{} = False
hasTyVarHead ListType{} = False
hasTyVarHead ParArrayType{} = False
hasTyVarHead (TypeApp f _) = hasTyVarHead f
hasTyVarHead InfixTypeApp{} = False
hasTyVarHead (ParenType t) = hasTyVarHead t
hasTyVarHead (VarType n) = maybe False GHC.isTyVarName (semanticsName n)
hasTyVarHead (KindedType t _) = hasTyVarHead t
hasTyVarHead (BangType t) = hasTyVarHead t
hasTyVarHead (LazyType t) = hasTyVarHead t
hasTyVarHead (UnpackType t) = hasTyVarHead t
hasTyVarHead (NoUnpackType t) = hasTyVarHead t
hasTyVarHead WildcardType{} = False
hasTyVarHead NamedWildcardType{} = False
hasTyVarHead SpliceType{} = False
hasTyVarHead QuasiQuoteType{} = False
hasTyVarHead PromotedIntType{} = False
hasTyVarHead PromotedStringType{} = False
hasTyVarHead PromotedConType{} = False
hasTyVarHead PromotedListType{} = False
hasTyVarHead PromotedTupleType{} = False
hasTyVarHead PromotedUnitType{} = False
hasTyVarHead UnboxedSumType{} = False
