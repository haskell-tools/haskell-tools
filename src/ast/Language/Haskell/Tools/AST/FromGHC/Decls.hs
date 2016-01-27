{-# LANGUAGE LambdaCase 
           , ViewPatterns
           #-}
module Language.Haskell.Tools.AST.FromGHC.Decls where

import RdrName as GHC
import Class as GHC
import HsSyn as GHC
import SrcLoc as GHC
import HsDecls as GHC
import Name as GHC
import OccName as GHC
import ApiAnnotation as GHC
import FastString as GHC
import BasicTypes as GHC
import Bag as GHC
import ForeignCall as GHC

import Control.Monad.Reader
import Data.Maybe

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Kinds
import Language.Haskell.Tools.AST.FromGHC.Types
import Language.Haskell.Tools.AST.FromGHC.TH
import Language.Haskell.Tools.AST.FromGHC.Binds
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Base as AST
import qualified Language.Haskell.Tools.AST.Binds as AST
import qualified Language.Haskell.Tools.AST.Types as AST
import qualified Language.Haskell.Tools.AST.Decls as AST

trfDecls :: TransformName n => [LHsDecl n] -> Trf (AnnList AST.Decl (AnnotType n))
-- TODO: filter documentation comments
trfDecls decls = AnnList <$> mapM trfDecl decls

trfDecl :: TransformName n => Located (HsDecl n) -> Trf (Ann AST.Decl (AnnotType n))
trfDecl = trfLoc $ \case
  TyClD (FamDecl (FamilyDecl DataFamily name tyVars kindSig)) 
    -> AST.TypeFamilyDecl <$> (annCont $ AST.DataFamily <$> createDeclHead name tyVars <*> trfKindSig kindSig)
  TyClD (FamDecl (FamilyDecl OpenTypeFamily name tyVars kindSig)) 
    -> AST.TypeFamilyDecl <$> (annCont $ AST.TypeFamily <$> createDeclHead name tyVars <*> trfKindSig kindSig)
  TyClD (FamDecl (FamilyDecl (ClosedTypeFamily typeEqs) name tyVars kindSig)) 
    -> AST.ClosedTypeFamilyDecl <$> createDeclHead name tyVars <*> trfKindSig kindSig <*> trfTypeEqs typeEqs
  TyClD (SynDecl name vars rhs _) 
    -> AST.TypeDecl <$> createDeclHead name vars <*> trfType rhs
  TyClD (DataDecl name vars (HsDataDefn nd ctx ct kind cons derivs) _) 
    -> AST.DataDecl <$> trfDataKeyword nd
                    <*> trfCtx ctx
                    <*> createDeclHead name vars
                    <*> (AnnList <$> mapM trfConDecl cons)
                    <*> trfMaybe trfDerivings derivs
  TyClD (ClassDecl ctx name vars funDeps sigs defs typeFuns typeFunDefs docs _) 
    -> AST.ClassDecl <$> trfCtx ctx <*> createDeclHead name vars <*> trfFunDeps funDeps 
                     <*> createClassBody sigs defs typeFuns typeFunDefs
  InstD (ClsInstD (ClsInstDecl typ binds sigs typefam datafam overlap))
    -> AST.InstDecl <$> trfMaybe trfOverlap overlap <*> trfInstanceRule typ 
                    <*> trfInstBody binds sigs typefam datafam
  ValD bind -> AST.ValueBinding <$> (annCont $ trfBind' bind)
  SigD (ts @ (TypeSig {})) -> AST.TypeSigDecl <$> (annCont $ trfTypeSig ts)
  SigD (FixSig fs) -> AST.FixityDecl <$> (annCont $ trfFixitySig fs)
  -- TODO: pattern synonym type signature
  -- TODO: INLINE, SPECIALIZE, MINIMAL, VECTORISE pragmas, Warnings, Annotations, rewrite rules, role annotations
  DefD (DefaultDecl types) -> AST.DefaultDecl . AnnList <$> mapM trfType types
  ForD (ForeignImport name typ _ (CImport ccall safe _ _ _)) 
    -> AST.ForeignImport <$> trfCallConv ccall <*> trfSafety safe <*> trfName name <*> trfType typ
  ForD (ForeignExport name typ _ (CExport (L l (CExportStatic _ ccall)) _)) 
    -> AST.ForeignExport <$> annLoc (pure l) (trfCallConv' ccall) <*> trfName name <*> trfType typ
  SpliceD (SpliceDecl (unLoc -> spl) _) -> AST.SpliceDecl <$> (annCont $ trfSplice' spl)

trfConDecl :: TransformName n => Located (ConDecl n) -> Trf (Ann AST.ConDecl (AnnotType n))
trfConDecl = trfLoc $ \case 
  ConDecl { con_names = [name], con_details = PrefixCon args }
    -> AST.ConDecl <$> trfName name <*> (AnnList <$> mapM trfType args)
  ConDecl { con_names = [name], con_details = RecCon (unLoc -> flds) }
    -> AST.RecordDecl <$> trfName name <*> (AnnList <$> mapM trfFieldDecl flds)
  ConDecl { con_names = [name], con_details = InfixCon t1 t2 }
    -> AST.InfixConDecl <$> trfName name <*> trfType t1 <*> trfType t2

trfFieldDecl :: TransformName n => Located (ConDeclField n) -> Trf (Ann AST.FieldDecl (AnnotType n))
trfFieldDecl = trfLoc $ \(ConDeclField names typ _)
  -> AST.FieldDecl <$> (AnnList <$> mapM trfName names) <*> trfType typ

trfDerivings :: TransformName n => Located [LHsType n] -> Trf (Ann AST.Deriving (AnnotType n))
trfDerivings = trfLoc $ \case
  [typ@(unLoc -> HsTyVar cls)] -> AST.DerivingOne <$> trfInstanceRule typ
  derivs -> AST.Derivings . AnnList <$> mapM trfInstanceRule derivs
  
trfInstanceRule :: TransformName n => Located (HsType n) -> Trf (Ann AST.InstanceRule (AnnotType n))
trfInstanceRule = trfLoc $ \case
  (HsForAllTy Explicit _ bndrs ctx typ) 
    -> AST.InstanceRule <$> (annJust <$> annLoc (pure $ collectLocs (hsq_tvs bndrs)) (trfBindings (hsq_tvs bndrs))) 
                        <*> trfCtx ctx
                        <*> trfInstanceHead typ
  (HsForAllTy Implicit _ _ _ typ) -> AST.InstanceRule <$> pure annNothing 
                                                      <*> pure annNothing 
                                                      <*> trfInstanceHead typ
  HsParTy typ -> AST.InstanceParen <$> trfInstanceRule typ
  HsTyVar tv -> AST.InstanceRule <$> pure annNothing 
                                 <*> pure annNothing 
                                 <*> annLoc (asks contRange) 
                                            (AST.InstanceHeadCon <$> annCont (trfName' tv))
                                 
trfInstanceHead :: TransformName n => Located (HsType n) -> Trf (Ann AST.InstanceHead (AnnotType n))
trfInstanceHead = trfLoc $ \case
  HsTyVar tv -> AST.InstanceHeadCon <$> annCont (trfName' tv)
  HsAppTy t1 t2 -> AST.InstanceHeadApp <$> trfInstanceHead t1 <*> trfType t2
  HsParTy typ -> AST.InstanceHeadParen <$> trfInstanceHead typ
  HsOpTy t1 (_,op) t2 
    -> AST.InstanceHeadApp <$> (annLoc (pure $ combineSrcSpans (getLoc t1) (getLoc op))
                                       (AST.InstanceHeadInfix <$> trfType t1 <*> trfName op)) 
                           <*> trfType t2
 
trfTypeEqs :: TransformName n => [Located (TyFamInstEqn n)] -> Trf (AnnList AST.TypeEqn (AnnotType n))
trfTypeEqs = fmap AnnList . mapM trfTypeEq

trfTypeEq :: TransformName n => Located (TyFamInstEqn n) -> Trf (Ann AST.TypeEqn (AnnotType n))
trfTypeEq = trfLoc $ \(TyFamEqn name pats rhs) 
  -> AST.TypeEqn <$> combineTypes name pats <*> trfType rhs
  where combineTypes :: TransformName n => Located n -> HsTyPats n -> Trf (Ann AST.Type (AnnotType n))
        combineTypes name pats 
          = foldl (\t p -> do typ <- t
                              annLoc (pure $ combineSrcSpans (extractRange $ _annotation typ) (getLoc p)) 
                                     (AST.TyApp <$> pure typ <*> trfType p)) 
                  (annLoc (pure $ getLoc name) (AST.TyVar <$> annCont (trfName' (unLoc name)))) 
                  (hswb_cts pats)
                 
trfFunDeps :: TransformName n => [Located (FunDep (Located n))] -> Trf (AnnMaybe AST.FunDeps (AnnotType n))
trfFunDeps [] = pure annNothing
trfFunDeps _ = pure undefined
  
createDeclHead :: TransformName n => Located n -> LHsTyVarBndrs n -> Trf (Ann AST.DeclHead (AnnotType n))
createDeclHead name vars
  = foldl (\t p -> do typ <- t
                      annLoc (pure $ combineSrcSpans (extractRange $ _annotation typ) (getLoc p)) 
                             (AST.DHApp typ <$> trfTyVar p)) 
          (annLoc (pure $ getLoc name) (AST.DeclHead <$> annCont (trfName' (unLoc name)))) 
          (hsq_tvs vars)
      
         
createClassBody :: TransformName n => [LSig n] -> LHsBinds n -> [LFamilyDecl n] 
                               -> [LTyFamDefltEqn n] -> Trf (AnnMaybe AST.ClassBody (AnnotType n))
createClassBody sigs binds typeFams typeFamDefs 
  = do isThereWhere <- isGoodSrcSpan <$> (tokenLoc AnnWhere)
       if isThereWhere 
         then annJust <$> annLoc (combinedLoc <$> tokenLoc AnnWhere) 
                                 (AST.ClassBody . AnnList . orderDefs . concat
                                     <$> sequenceA [getSigs, getBinds, getFams, getFamDefs])
         else pure annNothing
  where combinedLoc wh = foldl combineSrcSpans wh allLocs
        allLocs = map getLoc sigs ++ map getLoc (bagToList binds) ++ map getLoc typeFams ++ map getLoc typeFamDefs
        getSigs = mapM trfClassElemSig sigs
        getBinds = mapM (copyAnnot AST.ClsDef . trfBind) (bagToList binds)
        getFams = mapM (copyAnnot AST.ClsTypeFam . trfTypeFam) typeFams
        getFamDefs = mapM trfTypeFamDef typeFamDefs
       
trfClassElemSig :: TransformName n => Located (Sig n) -> Trf (Ann AST.ClassElement (AnnotType n))
trfClassElemSig = trfLoc $ \case
  TypeSig [name] typ _ -> AST.ClsSig <$> (annCont $ AST.TypeSignature <$> trfName name <*> trfType typ)
  GenericSig [name] typ -> AST.ClsDefSig <$> trfName name <*> trfType typ
         
trfTypeFam :: TransformName n => Located (FamilyDecl n) -> Trf (Ann AST.TypeFamily (AnnotType n))
trfTypeFam = trfLoc $ \case
  FamilyDecl DataFamily name tyVars kindSig
    -> AST.DataFamily <$> createDeclHead name tyVars <*> trfKindSig kindSig
  FamilyDecl OpenTypeFamily name tyVars kindSig
    -> AST.TypeFamily <$> createDeclHead name tyVars <*> trfKindSig kindSig
          
trfTypeFamDef :: TransformName n => Located (TyFamDefltEqn n) -> Trf (Ann AST.ClassElement (AnnotType n))
trfTypeFamDef = trfLoc $ \(TyFamEqn con pats rhs) 
  -> AST.ClsTypeDef <$> createDeclHead con pats <*> trfType rhs
          
trfInstBody :: TransformName n => LHsBinds n -> [LSig n] -> [LTyFamInstDecl n] -> [LDataFamInstDecl n] -> Trf (AnnMaybe AST.InstBody (AnnotType n))
trfInstBody binds sigs fams dats = do
    wh <- tokenLoc AnnWhere
    if isGoodSrcSpan wh then
      annJust <$> annLoc (combinedLoc <$> tokenLoc AnnWhere) 
                         (AST.InstBody . AnnList . orderDefs . concat
                             <$> sequenceA [getSigs, getBinds, getFams, getDats])
    else return annNothing
  where combinedLoc wh = foldl combineSrcSpans wh allLocs
        allLocs = map getLoc sigs ++ map getLoc (bagToList binds) ++ map getLoc fams ++ map getLoc dats
        getSigs = mapM trfClassInstSig sigs
        getBinds = mapM (copyAnnot AST.InstBodyNormalDecl . trfBind) (bagToList binds)
        getFams = mapM trfInstTypeFam fams
        getDats = mapM trfInstDataFam dats
          
trfClassInstSig :: TransformName n => Located (Sig n) -> Trf (Ann AST.InstBodyDecl (AnnotType n))
trfClassInstSig = trfLoc $ \case
  TypeSig [name] typ _ -> AST.InstBodyTypeSig <$> (annCont $ AST.TypeSignature <$> trfName name <*> trfType typ)
          
trfInstTypeFam :: TransformName n => Located (TyFamInstDecl n) -> Trf (Ann AST.InstBodyDecl (AnnotType n))
trfInstTypeFam (unLoc -> TyFamInstDecl eqn _) = copyAnnot AST.InstBodyTypeDecl (trfTypeEq eqn)

trfInstDataFam :: TransformName n => Located (DataFamInstDecl n) -> Trf (Ann AST.InstBodyDecl (AnnotType n))
trfInstDataFam = trfLoc $ \case 
  (DataFamInstDecl tc (hswb_cts -> pats) (HsDataDefn dn ctx _ _ cons derivs) _) 
    -> AST.InstBodyDataDecl <$> trfDataKeyword dn 
         <*> annLoc (pure $ collectLocs pats `combineSrcSpans` getLoc tc `combineSrcSpans` getLoc ctx)
                    (AST.InstanceRule annNothing <$> trfCtx ctx 
                                                 <*> foldr (\t r -> annLoc (combineSrcSpans (getLoc t) . extractRange . _annotation <$> r) 
                                                                           (AST.InstanceHeadApp <$> r <*> (trfType t))) 
                                                           (copyAnnot AST.InstanceHeadCon (trfName tc)) pats)
         <*> (AnnList <$> mapM trfConDecl cons)
         <*> trfMaybe trfDerivings derivs
          
