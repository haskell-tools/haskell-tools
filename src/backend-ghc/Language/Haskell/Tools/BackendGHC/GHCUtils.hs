{-# LANGUAGE MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleInstances
           , ScopedTypeVariables
           , ViewPatterns
           , LambdaCase
           , RecordWildCards
           , FlexibleContexts
           #-}
-- | Utility functions defined on the GHC AST representation.
module Language.Haskell.Tools.BackendGHC.GHCUtils where

import Data.Generics.Uniplate.Data ()
import Data.List

import Bag (Bag, bagToList, unionManyBags)
import BasicTypes (SourceText(..))
import ConLike (ConLike(..))
import Data.Maybe (Maybe(..), listToMaybe)
import GHC
import Id (Id, mkVanillaGlobal)
import OccName (OccName)
import Outputable (Outputable(..), showSDocUnsafe)
import PatSyn (patSynSig)
import RdrName (RdrName, rdrNameOcc, nameRdrName)
import SrcLoc
import Type (TyThing(..), mkFunTys)

class OutputableBndrId name => GHCName name where
  rdrName :: name -> RdrName
  getFromNameUsing :: Applicative f => (Name -> Ghc (f Id)) -> Name -> Ghc (f name)
  getBindsAndSigs :: HsValBinds name -> ([LSig name], LHsBinds name)
  nameFromId :: Id -> name
  unpackPostRn :: RdrName -> PostRn name name -> name

  gunpackPostRn :: a -> (name -> a) -> PostRn name name -> a

instance GHCName RdrName where
  rdrName = id
  getFromNameUsing _ n = return $ pure (nameRdrName n)
  getBindsAndSigs (ValBindsIn binds sigs) = (sigs, binds)
  getBindsAndSigs _ = error "ValBindsOut: ValBindsOut in parsed source"
  nameFromId = nameRdrName . getName
  unpackPostRn rdr _ = rdr

  gunpackPostRn a _ _ = a

occName :: GHCName n => n -> OccName
occName = rdrNameOcc . rdrName

instance GHCName GHC.Name where
  rdrName = nameRdrName
  getFromNameUsing f n = fmap nameFromId <$> f n
  getBindsAndSigs (ValBindsOut bindGroups sigs) = (sigs, unionManyBags (map snd bindGroups))
  getBindsAndSigs _ = error "getBindsAndSigs: ValBindsIn in renamed source"
  nameFromId = getName
  unpackPostRn _ a = a

  gunpackPostRn _ f pr = f pr

getFieldOccName :: GHCName n => Located (FieldOcc n) -> Located n
getFieldOccName (L l (FieldOcc (L _ rdr) postRn)) = L l (unpackPostRn rdr postRn)

getFieldOccName' :: GHCName n => FieldOcc n -> n
getFieldOccName' (FieldOcc (L _ rdr) postRn) = unpackPostRn rdr postRn



-- | Loading ids for top-level ghc names
getTopLevelId :: GHC.Name -> Ghc (Maybe GHC.Id)
getTopLevelId name =
    lookupName name >>= \case
      Just (AnId id) -> return (Just id)
      Just (AConLike (RealDataCon dc)) -> return $ Just $ mkVanillaGlobal name (dataConUserType dc)
      Just (AConLike (PatSynCon ps)) -> return $ Just $ mkVanillaGlobal name (createPatSynType ps)
      Just (ATyCon tc) -> return $ Just $ mkVanillaGlobal name (tyConKind tc)
      _ -> return Nothing
  where createPatSynType patSyn = case patSynSig patSyn of (_, _, _, _, args, res) -> mkFunTys args res

hsGetNames' :: HsHasName a => a -> [GHC.Name]
hsGetNames' = map fst . hsGetNames Nothing

-- | Get names from the GHC AST
class HsHasName a where
  hsGetNames :: Maybe GHC.Name -> a -> [(GHC.Name, Maybe GHC.Name)]

instance HsHasName RdrName where
  hsGetNames _ _ = []

instance HsHasName Name where
  hsGetNames p n = [(n, p)]

instance HsHasName Id where
  hsGetNames p n = [(getName n, p)]

instance HsHasName e => HsHasName [e] where
  hsGetNames p es = concatMap (hsGetNames p) es

instance HsHasName e => HsHasName (Located e) where
  hsGetNames p (L _ e) = hsGetNames p e

instance HsHasName n => HsHasName (HsLocalBinds n) where
  hsGetNames p (HsValBinds bnds) = hsGetNames p bnds
  hsGetNames _ _ = []

instance (GHCName n, HsHasName n) => HsHasName (HsDecl n) where
  hsGetNames p (TyClD tycl) = hsGetNames p tycl
  hsGetNames p (ValD vald) = hsGetNames p vald
  hsGetNames p (ForD ford) = hsGetNames p ford
  hsGetNames p (InstD inst) = hsGetNames p inst
  hsGetNames _ _ = []

instance (GHCName n, HsHasName n) => HsHasName (InstDecl n) where
  hsGetNames p (ClsInstD clsInst) = hsGetNames p (cid_datafam_insts clsInst)
  hsGetNames p (DataFamInstD dataFamInst) = hsGetNames p dataFamInst
  hsGetNames _ _ = []

instance (GHCName n, HsHasName n) => HsHasName (DataFamInstDecl n) where
  hsGetNames p dfid = hsGetNames p (dfid_defn dfid)

instance (GHCName n, HsHasName n) => HsHasName (TyClGroup n) where
  hsGetNames p (TyClGroup tycls _ _) = hsGetNames p tycls

instance (GHCName n, HsHasName n) => HsHasName (TyClDecl n) where
  hsGetNames p (FamDecl fd) = hsGetNames p fd
  hsGetNames p (SynDecl {tcdLName = name}) = hsGetNames p name
  hsGetNames p (DataDecl {tcdLName = name, tcdDataDefn = datadef})
    = let n = hsGetNames p name in n ++ hsGetNames (listToMaybe (map fst n)) datadef
  hsGetNames p (ClassDecl {tcdLName = name, tcdSigs = sigs, tcdATs = typeAssocs})
    = let n = hsGetNames p name in n ++ hsGetNames (listToMaybe (map fst n)) sigs
                                     ++ hsGetNames (listToMaybe (map fst n)) typeAssocs

instance (GHCName n, HsHasName n) => HsHasName (FamilyDecl n) where
 hsGetNames p (FamilyDecl { fdLName = name }) = hsGetNames p name

instance (GHCName n, HsHasName n) => HsHasName (HsDataDefn n) where
  hsGetNames p (HsDataDefn {dd_cons = ctors}) = hsGetNames p ctors

instance (GHCName n, HsHasName n) => HsHasName (ConDecl n) where
  hsGetNames p (ConDeclGADT {con_names = names, con_type = (HsIB _ (L _ (HsFunTy (L _ (HsRecTy flds)) _)) _)})
    = hsGetNames p names ++ hsGetNames p flds
  hsGetNames p (ConDeclGADT {con_names = names, con_type = (HsIB _ (L _ (HsRecTy flds)) _)})
    = hsGetNames p names ++ hsGetNames p flds
  hsGetNames p (ConDeclGADT {con_names = names}) = hsGetNames p names
  hsGetNames p (ConDeclH98 {con_name = name, con_details = details})
    = hsGetNames p name ++ hsGetNames p details

instance (GHCName n, HsHasName n) => HsHasName (HsConDeclDetails n) where
  hsGetNames p (RecCon rec) = hsGetNames p rec
  hsGetNames _ _ = []

instance (GHCName n, HsHasName n) => HsHasName (ConDeclField n) where
  hsGetNames p (ConDeclField name _ _) = hsGetNames p name

instance (GHCName n, HsHasName n) => HsHasName (FieldOcc n) where
  hsGetNames p (FieldOcc _ pr) = gunpackPostRn [] (hsGetNames p :: n -> [(Name, Maybe Name)]) pr

instance (GHCName n, HsHasName n) => HsHasName (Sig n) where
  hsGetNames p (TypeSig n _) = hsGetNames p n
  hsGetNames p (ClassOpSig _ n _) = hsGetNames p n
  hsGetNames p (PatSynSig n _) = hsGetNames p n
  hsGetNames _ _ = []

instance HsHasName n => HsHasName (ForeignDecl n) where
  hsGetNames p (ForeignImport n _ _ _) = hsGetNames p n
  hsGetNames _ _ = []

instance HsHasName n => HsHasName (HsValBinds n) where
  hsGetNames p (ValBindsIn bnds _) = hsGetNames p bnds
  hsGetNames p (ValBindsOut bnds _) = hsGetNames p $ map snd bnds

instance HsHasName n => HsHasName (Bag n) where
  hsGetNames p = hsGetNames p . bagToList

instance HsHasName n => HsHasName (HsBind n) where
  hsGetNames p (FunBind {fun_id = lname}) = hsGetNames p lname
  hsGetNames p (PatBind {pat_lhs = pat}) = hsGetNames p pat
  hsGetNames p (VarBind {var_id = id}) = hsGetNames p id
  hsGetNames p (PatSynBind (PSB {psb_id = id})) = hsGetNames p id
  hsGetNames _ _ = error "hsGetNames: called on compiler-generated binding"

instance HsHasName n => HsHasName (ParStmtBlock l n) where
  hsGetNames p (ParStmtBlock _ binds _) = hsGetNames p binds

--instance HsHasName n => HsHasName (LHsTyVarBndrs n) where
--  hsGetNames (HsQTvs kvs tvs) = hsGetNames kvs ++ hsGetNames tvs

instance HsHasName n => HsHasName (HsTyVarBndr n) where
  hsGetNames p (UserTyVar n) = hsGetNames p n
  hsGetNames p (KindedTyVar n _) = hsGetNames p n

instance HsHasName n => HsHasName (Match n b) where
  hsGetNames p (Match _ pats _ _) = concatMap (hsGetNames p) pats

instance HsHasName n => HsHasName (Stmt n b) where
  hsGetNames p (LetStmt binds) = hsGetNames p binds
  hsGetNames p (BindStmt pat _ _ _ _) = hsGetNames p pat
  hsGetNames p (RecStmt {recS_rec_ids = ids}) = hsGetNames p ids
  hsGetNames _ _ = []

instance HsHasName n => HsHasName (Pat n) where
  hsGetNames x (VarPat id) = hsGetNames x id
  hsGetNames x (LazyPat p) = hsGetNames x p
  hsGetNames x (AsPat lname p) = hsGetNames x lname ++ hsGetNames x p
  hsGetNames x (ParPat p) = hsGetNames x p
  hsGetNames x (BangPat p) = hsGetNames x p
  hsGetNames x (ListPat pats _ _) = concatMap (hsGetNames x) pats
  hsGetNames x (TuplePat pats _ _) = concatMap (hsGetNames x) pats
  hsGetNames x (PArrPat pats _) = concatMap (hsGetNames x) pats
  hsGetNames x (ConPatIn _ details) = concatMap (hsGetNames x) (hsConPatArgs details)
  hsGetNames x (ConPatOut {pat_args = details}) = concatMap (hsGetNames x) (hsConPatArgs details)
  hsGetNames x (ViewPat _ p _) = hsGetNames x p
  hsGetNames x (NPlusKPat lname _ _ _ _ _) = hsGetNames x lname
  hsGetNames x (SigPatIn p _) = hsGetNames x p
  hsGetNames x (SigPatOut p _) = hsGetNames x p
  hsGetNames _ _ = []

instance (GHCName n, HsHasName n) => HsHasName (HsGroup n) where
  hsGetNames p g@(HsGroup vals _ clds _ _ _ foreigns _ _ _ _ _)
    = hsGetNames p vals ++ hsGetNames p clds ++ hsGetNames p (hsGroupInstDecls g) ++ hsGetNames p foreigns

-- | Get the original form of a name
rdrNameStr :: RdrName -> String
rdrNameStr name = showSDocUnsafe $ ppr name


class FromGHCName n where
  fromGHCName :: GHC.Name -> n

instance FromGHCName RdrName where
  fromGHCName = rdrName

instance FromGHCName GHC.Name where
  fromGHCName = id

-- | Tries to simplify the type that has HsAppsTy before renaming. Does not always provide the correct form.
-- Treats each operator as if they are of equivalent precedence and always left-associative.
cleanHsType :: forall n . (OutputableBndrId n) => HsType n -> HsType n
-- for some reason * is considered infix
cleanHsType (HsAppsTy apps) = unLoc $ guessType apps
  where guessType :: OutputableBndrId n => [LHsAppType n] -> LHsType n
        guessType (L l (HsAppInfix n) : rest) -- must be a prefix actually
          = guessType' (L l (HsTyVar NotPromoted n)) rest
        guessType (L _ (HsAppPrefix t) : rest) = guessType' t rest
        guessType [] = error $ "guessType: empty: " ++ showSDocUnsafe (ppr apps)

        guessType' :: LHsType n -> [LHsAppType n] -> LHsType n
        guessType' fun (L _ (HsAppPrefix t) : rest) = guessType' (hsAppTy fun t) rest
        guessType' fun (L l (HsAppInfix n) : rest)
          -- TODO: find a better check
          | showSDocUnsafe (ppr n) == "*" = guessType' (hsAppTy fun (L l (HsTyVar NotPromoted n))) rest
        guessType' left (L _ (HsAppInfix n) : right) = hsOpTy left n (guessType right)
        guessType' t [] = t

        hsAppTy :: LHsType n -> LHsType n -> LHsType n
        hsAppTy t1 t2 = L (getLoc t1 `combineSrcSpans` getLoc t2) $ HsAppTy t1 t2

        hsOpTy :: LHsType n -> Located n -> LHsType n -> LHsType n
        hsOpTy t1 n t2 = L (getLoc t1 `combineSrcSpans` getLoc t2) $ HsOpTy t1 n t2
cleanHsType t = t

mergeFixityDefs :: [Located (FixitySig n)] -> [Located (FixitySig n)]
mergeFixityDefs (s@(L l _) : rest)
  = let (same, different) = partition ((== l) . getLoc) rest
     in foldl mergeWith s (map unLoc same) : mergeFixityDefs different
  where mergeWith (L l (FixitySig names fixity)) (FixitySig otherNames _) = L l (FixitySig (names ++ otherNames) fixity)
mergeFixityDefs [] = []

getGroupRange :: HsGroup n -> SrcSpan
getGroupRange (HsGroup {..})
  = foldr combineSrcSpans noSrcSpan locs
  where locs = [getHsValRange hs_valds] ++ map getLoc hs_splcds ++ map getLoc (concatMap group_tyclds hs_tyclds) ++ map getLoc (concatMap group_roles hs_tyclds)
                 ++ map getLoc hs_derivds ++ map getLoc hs_fixds ++ map getLoc hs_defds
                 ++ map getLoc hs_fords ++ map getLoc hs_warnds ++ map getLoc hs_annds ++ map getLoc hs_ruleds ++ map getLoc hs_vects
                 ++ map getLoc hs_docs

getHsValRange :: HsValBinds n -> SrcSpan
getHsValRange (ValBindsIn vals sig) = foldr combineSrcSpans noSrcSpan $ map getLoc (bagToList vals) ++ map getLoc sig
getHsValRange (ValBindsOut vals sig) = foldr combineSrcSpans noSrcSpan $ concatMap (map getLoc . bagToList . snd) vals ++ map getLoc sig

fromSrcText :: SourceText -> String
fromSrcText (SourceText s) = s
fromSrcText NoSourceText = ""
