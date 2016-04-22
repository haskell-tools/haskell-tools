{-# LANGUAGE MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleInstances
           #-}
module Language.Haskell.Tools.AST.FromGHC.GHCUtils where

import GHC
import Bag

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

instance HsHasName n => HsHasName (HsDecl n) where
  hsGetNames (TyClD tycl) = hsGetNames tycl
  hsGetNames (ValD vald) = hsGetNames vald
  hsGetNames (ForD ford) = hsGetNames ford
  hsGetNames _ = []

instance HsHasName n => HsHasName (TyClGroup n) where
  hsGetNames (TyClGroup tycls _) = hsGetNames tycls

instance HsHasName n => HsHasName (TyClDecl n) where
  hsGetNames (FamDecl (FamilyDecl {fdLName = name})) = hsGetNames name
  hsGetNames (SynDecl {tcdLName = name}) = hsGetNames name
  hsGetNames (DataDecl {tcdLName = name, tcdDataDefn = datadef}) = hsGetNames name ++ hsGetNames datadef
  hsGetNames (ClassDecl {tcdLName = name, tcdSigs = sigs}) = hsGetNames name ++ hsGetNames sigs

instance HsHasName n => HsHasName (HsDataDefn n) where
  hsGetNames (HsDataDefn {dd_cons = ctors}) = hsGetNames ctors

instance HsHasName n => HsHasName (ConDecl n) where
  hsGetNames (ConDecl {con_names = names, con_details = details}) = hsGetNames names ++ hsGetNames details

instance HsHasName n => HsHasName (HsConDeclDetails n) where
  hsGetNames (RecCon rec) = hsGetNames rec
  hsGetNames _ = []

instance HsHasName n => HsHasName (ConDeclField n) where
  hsGetNames (ConDeclField name _ _) = hsGetNames name

instance HsHasName n => HsHasName (Sig n) where
  hsGetNames (TypeSig n _ _) = hsGetNames n
  hsGetNames (PatSynSig n _ _ _ _) = hsGetNames n
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

instance HsHasName n => HsHasName (LHsTyVarBndrs n) where
  hsGetNames (HsQTvs kvs tvs) = hsGetNames kvs ++ hsGetNames tvs

instance HsHasName n => HsHasName (HsTyVarBndr n) where
  hsGetNames (UserTyVar n) = hsGetNames n
  hsGetNames (KindedTyVar n _) = hsGetNames n

instance HsHasName n => HsHasName (Stmt n b) where
  hsGetNames (LetStmt binds) = hsGetNames binds
  hsGetNames (BindStmt pat _ _ _) = hsGetNames pat
  hsGetNames (RecStmt {recS_rec_ids = ids}) = hsGetNames ids

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
  hsGetNames (NPlusKPat lname _ _ _) = hsGetNames lname
  hsGetNames (SigPatIn p _) = hsGetNames p
  hsGetNames (SigPatOut p _) = hsGetNames p
  hsGetNames _ = []
