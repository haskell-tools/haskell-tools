{-# LANGUAGE MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleInstances
           , ScopedTypeVariables
           #-}
module Language.Haskell.Tools.AST.FromGHC.GHCUtils where

import GHC
import Bag
import RdrName
import OccName
import Name
import Outputable

class OutputableBndr name => GHCName name where 
  rdrName :: name -> RdrName
  getBindsAndSigs :: HsValBinds name -> ([LSig name], LHsBinds name)
  nameFromId :: Id -> name
  unpackPostRn :: RdrName -> PostRn name name -> name
  unpackPostTc :: RdrName -> GHC.Name -> PostTc name name -> name

  gunpackPostRn :: a -> (name -> a) -> PostRn name name -> a

instance GHCName RdrName where
  rdrName = id
  getBindsAndSigs (ValBindsIn binds sigs) = (sigs, binds)
  nameFromId = nameRdrName . getName
  unpackPostRn rdr _ = rdr
  unpackPostTc rdr _ _ = rdr

  gunpackPostRn a _ _ = a

occName :: GHCName n => n -> OccName
occName = rdrNameOcc . rdrName 
    
instance GHCName GHC.Name where
  rdrName = nameRdrName
  getBindsAndSigs (ValBindsOut bindGroups sigs) = (sigs, unionManyBags (map snd bindGroups))
  nameFromId = getName
  unpackPostRn _ a = a
  unpackPostTc _ nm _ = nm

  gunpackPostRn _ f pr = f pr

getFieldOccName :: GHCName n => Located (FieldOcc n) -> Located n
getFieldOccName (L l (FieldOcc (L _ rdr) postRn)) = L l (unpackPostRn rdr postRn)

getFieldOccName' :: GHCName n => FieldOcc n -> n
getFieldOccName' (FieldOcc (L _ rdr) postRn) = unpackPostRn rdr postRn

getAmbiguousFieldName :: GHCName n => Located (AmbiguousFieldOcc n) -> Located n
getAmbiguousFieldName (L l af) = L l (getAmbiguousFieldName' af)

getAmbiguousFieldName' :: GHCName n => AmbiguousFieldOcc n -> n
getAmbiguousFieldName' (Unambiguous (L _ rdr) pr) = unpackPostRn rdr pr
getAmbiguousFieldName' (Ambiguous (L _ rdr) pt) = unpackPostTc rdr undefined pt

class HsHasName a where
  hsGetNames :: a -> [GHC.Name]

instance HsHasName RdrName where
  hsGetNames _ = [] 

instance HsHasName Name where
  hsGetNames n = [n] 

instance HsHasName Id where
  hsGetNames n = [getName n] 

instance HsHasName e => HsHasName [e] where
  hsGetNames es = concatMap hsGetNames es

instance HsHasName e => HsHasName (Located e) where
  hsGetNames (L _ e) = hsGetNames e

instance HsHasName n => HsHasName (HsLocalBinds n) where
  hsGetNames (HsValBinds bnds) = hsGetNames bnds
  hsGetNames _ = []

instance (GHCName n, HsHasName n) => HsHasName (HsDecl n) where
  hsGetNames (TyClD tycl) = hsGetNames tycl
  hsGetNames (ValD vald) = hsGetNames vald
  hsGetNames (ForD ford) = hsGetNames ford
  hsGetNames _ = []

instance (GHCName n, HsHasName n) => HsHasName (TyClGroup n) where
  hsGetNames (TyClGroup tycls _) = hsGetNames tycls

instance (GHCName n, HsHasName n) => HsHasName (TyClDecl n) where
  hsGetNames (FamDecl (FamilyDecl {fdLName = name})) = hsGetNames name
  hsGetNames (SynDecl {tcdLName = name}) = hsGetNames name
  hsGetNames (DataDecl {tcdLName = name, tcdDataDefn = datadef}) = hsGetNames name ++ hsGetNames datadef
  hsGetNames (ClassDecl {tcdLName = name, tcdSigs = sigs}) = hsGetNames name ++ hsGetNames sigs

instance (GHCName n, HsHasName n) => HsHasName (HsDataDefn n) where
  hsGetNames (HsDataDefn {dd_cons = ctors}) = hsGetNames ctors

instance (GHCName n, HsHasName n) => HsHasName (ConDecl n) where
  hsGetNames (ConDeclGADT {con_names = names, con_type = (HsIB _ (L l (HsRecTy flds)))}) = hsGetNames names ++ hsGetNames flds
  hsGetNames (ConDeclGADT {con_names = names}) = hsGetNames names
  hsGetNames (ConDeclH98 {con_name = name, con_details = details}) = hsGetNames name ++ hsGetNames details

instance (GHCName n, HsHasName n) => HsHasName (HsConDeclDetails n) where
  hsGetNames (RecCon rec) = hsGetNames rec
  hsGetNames _ = []

instance (GHCName n, HsHasName n) => HsHasName (ConDeclField n) where
  hsGetNames (ConDeclField name _ _) = hsGetNames name

instance (GHCName n, HsHasName n) => HsHasName (FieldOcc n) where 
  hsGetNames (FieldOcc _ pr) = gunpackPostRn [] (hsGetNames :: n -> [Name]) pr

instance (GHCName n, HsHasName n) => HsHasName (Sig n) where
  hsGetNames (TypeSig n _) = hsGetNames n
  hsGetNames (PatSynSig n _) = hsGetNames n
  hsGetNames _ = []

instance HsHasName n => HsHasName (ForeignDecl n) where
  hsGetNames (ForeignImport n _ _ _) = hsGetNames n
  hsGetNames _ = []

instance HsHasName n => HsHasName (HsValBinds n) where
  hsGetNames (ValBindsIn bnds _) = hsGetNames bnds
  hsGetNames (ValBindsOut bnds _) = hsGetNames $ map snd bnds

instance HsHasName n => HsHasName (Bag n) where
  hsGetNames = hsGetNames . bagToList

instance HsHasName n => HsHasName (HsBind n) where
  hsGetNames (FunBind {fun_id = lname}) = hsGetNames lname
  hsGetNames (PatBind {pat_lhs = pat}) = hsGetNames pat
  hsGetNames (VarBind {var_id = id}) = hsGetNames id
  hsGetNames (PatSynBind (PSB {psb_id = id})) = hsGetNames id

instance HsHasName n => HsHasName (ParStmtBlock l n) where
  hsGetNames (ParStmtBlock _ binds _) = hsGetNames binds

--instance HsHasName n => HsHasName (LHsTyVarBndrs n) where
--  hsGetNames (HsQTvs kvs tvs) = hsGetNames kvs ++ hsGetNames tvs

instance HsHasName n => HsHasName (HsTyVarBndr n) where
  hsGetNames (UserTyVar n) = hsGetNames n
  hsGetNames (KindedTyVar n _) = hsGetNames n

instance HsHasName n => HsHasName (Stmt n b) where
  hsGetNames (LetStmt binds) = hsGetNames binds
  hsGetNames (BindStmt pat _ _ _ _) = hsGetNames pat
  hsGetNames (RecStmt {recS_rec_ids = ids}) = hsGetNames ids
  hsGetNames _ = []

instance HsHasName n => HsHasName (Pat n) where
  hsGetNames (VarPat id) = hsGetNames id
  hsGetNames (LazyPat p) = hsGetNames p
  hsGetNames (AsPat lname p) = hsGetNames lname ++ hsGetNames p
  hsGetNames (ParPat p) = hsGetNames p
  hsGetNames (BangPat p) = hsGetNames p
  hsGetNames (ListPat pats _ _) = concatMap hsGetNames pats
  hsGetNames (TuplePat pats _ _) = concatMap hsGetNames pats
  hsGetNames (PArrPat pats _) = concatMap hsGetNames pats
  hsGetNames (ConPatIn _ details) = concatMap hsGetNames (hsConPatArgs details)
  hsGetNames (ConPatOut {pat_args = details}) = concatMap hsGetNames (hsConPatArgs details)
  hsGetNames (ViewPat _ p _) = hsGetNames p
  hsGetNames (NPlusKPat lname _ _ _ _ _) = hsGetNames lname
  hsGetNames (SigPatIn p _) = hsGetNames p
  hsGetNames (SigPatOut p _) = hsGetNames p
  hsGetNames _ = []

-- | Get the original form of a name
rdrNameStr :: RdrName -> String
rdrNameStr name = showSDocUnsafe $ ppr name