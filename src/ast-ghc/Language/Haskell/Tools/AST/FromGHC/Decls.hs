{-# LANGUAGE LambdaCase 
           , ViewPatterns
           , ScopedTypeVariables
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
import Outputable as GHC

import Control.Monad.Reader
import Control.Reference
import Data.Maybe

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Kinds
import Language.Haskell.Tools.AST.FromGHC.Types
import Language.Haskell.Tools.AST.FromGHC.Patterns
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.TH
import Language.Haskell.Tools.AST.FromGHC.Binds
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST (Ann(..), AnnMaybe(..), AnnList(..), RangeWithName, getRange)
import qualified Language.Haskell.Tools.AST as AST

trfDecls :: TransformName n r => [LHsDecl n] -> Trf (AnnList AST.Decl r)
-- TODO: filter documentation comments
trfDecls decls = makeIndentedList atTheEnd (mapM trfDecl decls)

trfDeclsGroup :: forall n r . TransformName n r => HsGroup n -> Trf (AnnList AST.Decl r)
trfDeclsGroup (HsGroup vals splices tycls insts derivs fixities defaults foreigns warns anns rules vects docs) 
  = makeIndentedList atTheEnd (fmap (orderDefs . concat) $ sequence $
      [ trfBindOrSig vals
      , concat <$> mapM (mapM (trfDecl . (fmap TyClD)) . group_tyclds) tycls
      , mapM (trfDecl . (fmap SpliceD)) splices
      , mapM (trfDecl . (fmap InstD)) insts
      , mapM (trfDecl . (fmap DerivD)) derivs
      , mapM (trfDecl . (fmap (SigD . FixSig))) fixities
      , mapM (trfDecl . (fmap DefD)) defaults
      , mapM (trfDecl . (fmap ForD)) foreigns
      , mapM (trfDecl . (fmap WarningD)) warns
      , mapM (trfDecl . (fmap AnnD)) anns
      , mapM (trfDecl . (fmap RuleD)) rules
      , mapM (trfDecl . (fmap VectD)) vects
      -- , mapM (trfDecl . (fmap DocD)) docs
      ])
  where trfBindOrSig :: HsValBinds n -> Trf [Ann AST.Decl r]
        trfBindOrSig (getBindsAndSigs -> (sigs, binds))
          = (++) <$> mapM (trfLoc trfVal) (bagToList binds)
                 <*> mapM (trfLoc trfSig) sigs
           
           
trfDecl :: TransformName n r => Located (HsDecl n) -> Trf (Ann AST.Decl r)
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
    -> let ctxLoc = case nd of DataType -> after AnnData
                               NewType -> after AnnNewtype
           consLoc = case unLoc ctx of [] -> ctxLoc
                                       _  -> after AnnDarrow
        in AST.DataDecl <$> trfDataKeyword nd
                        <*> trfCtx ctxLoc ctx
                        <*> createDeclHead name vars
                        <*> makeList " | " consLoc (mapM trfConDecl cons)
                        <*> trfMaybe "" "" trfDerivings derivs
  TyClD (ClassDecl ctx name vars funDeps sigs defs typeFuns typeFunDefs docs _) 
    -> AST.ClassDecl <$> trfCtx (after AnnClass) ctx 
                     <*> createDeclHead name vars 
                     <*> trfFunDeps funDeps 
                     <*> createClassBody sigs defs typeFuns typeFunDefs
  InstD (ClsInstD (ClsInstDecl typ binds sigs typefam datafam overlap))
    -> AST.InstDecl <$> trfMaybeDefault " " "" trfOverlap (after AnnInstance) overlap 
                    <*> trfInstanceRule typ 
                    <*> trfInstBody binds sigs typefam datafam
  InstD (DataFamInstD (DataFamInstDecl con pats (HsDataDefn nd _ _ _ cons derivs) _))
    -> AST.DataInstDecl <$> trfDataKeyword nd
                        <*> between AnnInstance AnnEqual (makeInstanceRuleTyVars con pats)
                        <*> makeList " | " (after AnnEqual) (mapM trfConDecl cons)
                        <*> trfMaybe "" "" trfDerivings derivs
  InstD (TyFamInstD (TyFamInstDecl (L l (TyFamEqn con pats rhs)) _))
    -> AST.TypeInstDecl <$> between AnnInstance AnnEqual (makeInstanceRuleTyVars con pats) <*> trfType rhs
  ValD bind -> trfVal bind
  SigD sig -> trfSig sig
  DerivD (DerivDecl t overlap) -> AST.DerivDecl <$> trfMaybe "" "" trfOverlap overlap <*> trfInstanceRule t
  -- TODO: INLINE, SPECIALIZE, MINIMAL, VECTORISE pragmas, Warnings, Annotations, rewrite rules, role annotations
  RuleD (HsRules _ rules) -> AST.PragmaDecl <$> annCont (AST.RulePragma <$> makeIndentedList (before AnnClose) (mapM trfRewriteRule rules))
  RoleAnnotD (RoleAnnotDecl name roles) -> AST.RoleDecl <$> trfName name <*> makeList " " atTheEnd (mapM trfRole roles)
  DefD (DefaultDecl types) -> AST.DefaultDecl . nonemptyAnnList <$> mapM trfType types
  ForD (ForeignImport name typ _ (CImport ccall safe _ _ _)) 
    -> AST.ForeignImport <$> trfCallConv ccall <*> trfSafety safe <*> trfName name <*> trfType typ
  ForD (ForeignExport name typ _ (CExport (L l (CExportStatic _ ccall)) _)) 
    -> AST.ForeignExport <$> annLoc (pure l) (trfCallConv' ccall) <*> trfName name <*> trfType typ
  SpliceD (SpliceDecl (unLoc -> spl) _) -> AST.SpliceDecl <$> (annCont $ trfSplice' spl)

trfVal :: TransformName n r => HsBindLR n n -> Trf (AST.Decl r)
trfVal (PatSynBind psb) = AST.PatternSynonymDecl <$> annCont (trfPatternSynonym psb)
trfVal bind = AST.ValueBinding <$> (annCont $ trfBind' bind)

trfSig :: TransformName n r => Sig n -> Trf (AST.Decl r)
trfSig (ts @ (TypeSig {})) = AST.TypeSigDecl <$> (annCont $ trfTypeSig' ts)
trfSig (FixSig fs) = AST.FixityDecl <$> (annCont $ trfFixitySig fs)
trfSig (PatSynSig id (expl, args) ctx1 ctx2 typ) 
  = AST.PatTypeSigDecl <$> annCont (AST.PatternTypeSignature <$> trfName id <*> addForall expl (hsq_tvs args) (addCtx ctx1 (addCtx ctx2 (trfType typ))))
  where addForall Implicit _ t = t
        addForall _ args t = annFrom AnnForall (AST.TyForall <$> trfBindings args <*> nothing "" " => " (after AnnDot) <*> t)
        addCtx (L _ []) t = t
        addCtx ctx t = annLoc (pure $ combineSrcSpans (getLoc ctx) (getLoc typ)) 
                              (AST.TyCtx <$> (fromJust . (^. AST.annMaybe) <$> trfCtx atTheStart ctx) <*> t)

trfConDecl :: TransformName n r => Located (ConDecl n) -> Trf (Ann AST.ConDecl r)
trfConDecl = trfLoc trfConDecl'

trfConDecl' :: TransformName n r => ConDecl n -> Trf (AST.ConDecl r)
trfConDecl' (ConDecl { con_names = [name], con_details = PrefixCon args })
  = AST.ConDecl <$> trfName name <*> makeList " " atTheEnd (mapM trfType args)
trfConDecl' (ConDecl { con_names = [name], con_details = RecCon (unLoc -> flds) })
  = AST.RecordDecl <$> trfName name <*> (between AnnOpenC AnnCloseC $ trfAnnList ", " trfFieldDecl' flds)
trfConDecl' (ConDecl { con_names = [name], con_details = InfixCon t1 t2 })
  = AST.InfixConDecl <$> trfName name <*> trfType t1 <*> trfType t2

trfFieldDecl :: TransformName n r => Located (ConDeclField n) -> Trf (Ann AST.FieldDecl r)
trfFieldDecl = trfLoc trfFieldDecl'

trfFieldDecl' :: TransformName n r => ConDeclField n -> Trf (AST.FieldDecl r)
trfFieldDecl' (ConDeclField names typ _) = AST.FieldDecl <$> (nonemptyAnnList <$> mapM trfName names) <*> trfType typ

trfDerivings :: TransformName n r => Located [LHsType n] -> Trf (Ann AST.Deriving r)
trfDerivings = trfLoc $ \case
  [typ@(unLoc -> HsTyVar cls)] -> AST.DerivingOne <$> trfInstanceHead typ
  derivs -> AST.Derivings <$> trfAnnList ", " trfInstanceHead' derivs
  
trfInstanceRule :: TransformName n r => Located (HsType n) -> Trf (Ann AST.InstanceRule r)
trfInstanceRule = trfLoc $ \case
    (HsForAllTy Explicit _ bndrs ctx typ) 
      -> AST.InstanceRule <$> (makeJust <$> annLoc (pure $ collectLocs (hsq_tvs bndrs)) (trfBindings (hsq_tvs bndrs))) 
                          <*> trfCtx (after AnnDot) ctx
                          <*> trfInstanceHead typ
    (HsForAllTy Implicit _ _ ctx typ) -> AST.InstanceRule <$> nothing "" " . " atTheStart 
                                                          <*> trfCtx atTheStart ctx
                                                          <*> trfInstanceHead typ
    HsParTy typ -> AST.InstanceParen <$> trfInstanceRule typ
    HsTyVar tv -> instanceHead $ annCont (AST.InstanceHeadCon <$> trfNameSp' tv)
    HsAppTy t1 t2 -> instanceHead $ annCont (AST.InstanceHeadApp <$> trfInstanceHead t1 <*> trfType t2)
  where instanceHead hd = AST.InstanceRule <$> (nothing "" " . " atTheStart) <*> (nothing " " "" atTheStart) <*> hd
                            
makeInstanceRuleTyVars :: TransformName n r => Located n -> HsWithBndrs n [LHsType n] -> Trf (Ann AST.InstanceRule r)
makeInstanceRuleTyVars n vars = annCont
  $ AST.InstanceRule <$> nothing "" " . " atTheStart
                     <*> nothing " " "" atTheStart
                     <*> foldl (\c t -> annLoc (pure $ combineSrcSpans (getLoc n) (getLoc t)) $ AST.InstanceHeadApp <$> c <*> (trfType t))
                               (copyAnnot AST.InstanceHeadCon (trfName n))
                               (hswb_cts vars)

trfInstanceHead :: TransformName n r => Located (HsType n) -> Trf (Ann AST.InstanceHead r)
trfInstanceHead = trfLoc trfInstanceHead'

trfInstanceHead' :: TransformName n r => HsType n -> Trf (AST.InstanceHead r)
trfInstanceHead' (HsForAllTy Implicit Nothing (HsQTvs [] []) (unLoc -> []) (unLoc -> t)) = trfInstanceHead' t
trfInstanceHead' (HsTyVar tv) = AST.InstanceHeadCon <$> trfNameSp' tv
trfInstanceHead' (HsAppTy t1 t2) = AST.InstanceHeadApp <$> trfInstanceHead t1 <*> trfType t2
trfInstanceHead' (HsParTy typ) = AST.InstanceHeadParen <$> trfInstanceHead typ
trfInstanceHead' (HsOpTy t1 (_,op) t2) 
  = AST.InstanceHeadApp <$> (annLoc (pure $ combineSrcSpans (getLoc t1) (getLoc op))
                                    (AST.InstanceHeadInfix <$> trfType t1 <*> trfName op)) 
                        <*> trfType t2
 
trfTypeEqs :: TransformName n r => [Located (TyFamInstEqn n)] -> Trf (AnnList AST.TypeEqn r)
trfTypeEqs eqs = makeList "\n" (after AnnWhere) (mapM trfTypeEq eqs)

trfTypeEq :: TransformName n r => Located (TyFamInstEqn n) -> Trf (Ann AST.TypeEqn r)
trfTypeEq = trfLoc $ \(TyFamEqn name pats rhs) 
  -> AST.TypeEqn <$> combineTypes name pats <*> trfType rhs
  where combineTypes :: TransformName n r => Located n -> HsTyPats n -> Trf (Ann AST.Type r)
        combineTypes name pats 
          = foldl (\t p -> do typ <- t
                              annLoc (pure $ combineSrcSpans (getRange $ _annotation typ) (getLoc p)) 
                                     (AST.TyApp <$> pure typ <*> trfType p)) 
                  (annLoc (pure $ getLoc name) (AST.TyVar <$> trfNameSp' (unLoc name))) 
                  (hswb_cts pats)
                 
trfFunDeps :: TransformName n r => [Located (FunDep (Located n))] -> Trf (AnnMaybe AST.FunDeps r)
trfFunDeps [] = nothing "|" "" $ before AnnWhere
trfFunDeps _ = error "trfFunDeps"
  
createDeclHead :: TransformName n r => Located n -> LHsTyVarBndrs n -> Trf (Ann AST.DeclHead r)
createDeclHead name vars
  = foldl (\t p -> do typ <- t
                      annLoc (pure $ combineSrcSpans (getRange $ _annotation typ) (getLoc p)) 
                             (AST.DHApp typ <$> trfTyVar p)) 
          (annLoc (pure $ getLoc name) (AST.DeclHead <$> trfNameSp' (unLoc name))) 
          (hsq_tvs vars)
      
         
createClassBody :: TransformName n r => [LSig n] -> LHsBinds n -> [LFamilyDecl n] 
                               -> [LTyFamDefltEqn n] -> Trf (AnnMaybe AST.ClassBody r)
createClassBody sigs binds typeFams typeFamDefs 
  = do isThereWhere <- isGoodSrcSpan <$> (tokenLoc AnnWhere)
       if isThereWhere 
         then makeJust <$> annLoc (combinedLoc <$> tokenLoc AnnWhere) 
                                  (AST.ClassBody <$> makeList "" (after AnnWhere) 
                                                                 (orderDefs . concat <$> sequenceA allDefs))
         else nothing " where " "" atTheEnd
  where combinedLoc wh = foldl combineSrcSpans wh allLocs
        allLocs = map getLoc sigs ++ map getLoc (bagToList binds) ++ map getLoc typeFams ++ map getLoc typeFamDefs
        allDefs = [getSigs, getBinds, getFams, getFamDefs]
        getSigs = mapM trfClassElemSig sigs
        getBinds = mapM (copyAnnot AST.ClsDef . trfBind) (bagToList binds)
        getFams = mapM (copyAnnot AST.ClsTypeFam . trfTypeFam) typeFams
        getFamDefs = mapM trfTypeFamDef typeFamDefs
       
trfClassElemSig :: TransformName n r => Located (Sig n) -> Trf (Ann AST.ClassElement r)
trfClassElemSig = trfLoc $ \case
  TypeSig names typ _ -> AST.ClsSig <$> (annCont $ AST.TypeSignature <$> makeNonemptyList ", " (mapM trfName names) <*> trfType typ)
  GenericSig [name] typ -> AST.ClsDefSig <$> trfName name <*> trfType typ
         
trfTypeFam :: TransformName n r => Located (FamilyDecl n) -> Trf (Ann AST.TypeFamily r)
trfTypeFam = trfLoc $ \case
  FamilyDecl DataFamily name tyVars kindSig
    -> AST.DataFamily <$> createDeclHead name tyVars <*> trfKindSig kindSig
  FamilyDecl OpenTypeFamily name tyVars kindSig
    -> AST.TypeFamily <$> createDeclHead name tyVars <*> trfKindSig kindSig
          
trfTypeFamDef :: TransformName n r => Located (TyFamDefltEqn n) -> Trf (Ann AST.ClassElement r)
trfTypeFamDef = trfLoc $ \(TyFamEqn con pats rhs) 
  -> AST.ClsTypeDef <$> createDeclHead con pats <*> trfType rhs
          
trfInstBody :: TransformName n r => LHsBinds n -> [LSig n] -> [LTyFamInstDecl n] -> [LDataFamInstDecl n] -> Trf (AnnMaybe AST.InstBody r)
trfInstBody binds sigs fams dats = do
    wh <- tokenLoc AnnWhere
    if isGoodSrcSpan wh then
      makeJust <$> annLoc (combinedLoc <$> tokenLoc AnnWhere) 
                          (AST.InstBody <$> (makeList "" (after AnnWhere) 
                                                         (orderDefs . concat <$> sequenceA allDefs)))
    else nothing " where " "" atTheEnd
  where combinedLoc wh = foldl combineSrcSpans wh allLocs
        allLocs = map getLoc sigs ++ map getLoc (bagToList binds) ++ map getLoc fams ++ map getLoc dats
        allDefs = [getSigs, getBinds, getFams, getDats]
        getSigs = mapM trfClassInstSig sigs
        getBinds = mapM (copyAnnot AST.InstBodyNormalDecl . trfBind) (bagToList binds)
        getFams = mapM trfInstTypeFam fams
        getDats = mapM trfInstDataFam dats
          
trfClassInstSig :: TransformName n r => Located (Sig n) -> Trf (Ann AST.InstBodyDecl r)
trfClassInstSig = trfLoc $ \case
  TypeSig names typ _ -> AST.InstBodyTypeSig <$> (annCont $ AST.TypeSignature <$> makeNonemptyList ", " (mapM trfName names) <*> trfType typ)
          
trfInstTypeFam :: TransformName n r => Located (TyFamInstDecl n) -> Trf (Ann AST.InstBodyDecl r)
trfInstTypeFam (unLoc -> TyFamInstDecl eqn _) = copyAnnot AST.InstBodyTypeDecl (trfTypeEq eqn)

trfInstDataFam :: TransformName n r => Located (DataFamInstDecl n) -> Trf (Ann AST.InstBodyDecl r)
trfInstDataFam = trfLoc $ \case 
  (DataFamInstDecl tc (hswb_cts -> pats) (HsDataDefn dn ctx _ _ cons derivs) _) 
    -> AST.InstBodyDataDecl 
         <$> trfDataKeyword dn 
         <*> annLoc (pure $ collectLocs pats `combineSrcSpans` getLoc tc `combineSrcSpans` getLoc ctx)
                    (AST.InstanceRule <$> nothing "" " . " atTheStart
                                      <*> trfCtx atTheStart ctx 
                                      <*> foldr (\t r -> annLoc (combineSrcSpans (getLoc t) . getRange . _annotation <$> r) 
                                                                (AST.InstanceHeadApp <$> r <*> (trfType t))) 
                                                (copyAnnot AST.InstanceHeadCon (trfName tc)) pats)
         <*> trfAnnList "" trfConDecl' cons
         <*> trfMaybe " deriving " "" trfDerivings derivs
          
trfPatternSynonym :: forall n r . TransformName n r => PatSynBind n n -> Trf (AST.PatternSynonym r)
trfPatternSynonym (PSB id _ (PrefixPatSyn args) def dir)
  = let sep = case dir of ImplicitBidirectional -> AnnEqual
                          _ -> AnnLarrow
        rhsLoc = combineSrcSpans (getLoc def) <$> tokenLoc sep
     in AST.PatternSynonym <$> trfName id
                           <*> makeList " " (before sep) (mapM trfName args) 
                           <*> annLoc rhsLoc (trfPatSynRhs dir def)
  where trfPatSynRhs :: TransformName n r => HsPatSynDir n -> Located (Pat n) -> Trf (AST.PatSynRhs r)
        trfPatSynRhs ImplicitBidirectional pat = AST.BidirectionalPatSyn <$> trfPattern pat <*> nothing " where " "" atTheEnd
        trfPatSynRhs (ExplicitBidirectional mg) pat = AST.BidirectionalPatSyn <$> trfPattern pat <*> (makeJust <$> trfPatSynWhere mg)
        trfPatSynRhs Unidirectional pat = AST.OneDirectionalPatSyn <$> trfPattern pat
        trfPatSynWhere :: TransformName n r => MatchGroup n (LHsExpr n) -> Trf (Ann AST.PatSynWhere r)
        trfPatSynWhere (MG { mg_alts = alts }) = annLoc (pure $ collectLocs alts) (AST.PatSynWhere <$> makeIndentedList (after AnnWhere) (mapM (trfMatch (unLoc id)) alts))
