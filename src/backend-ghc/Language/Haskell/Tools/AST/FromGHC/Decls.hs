{-# LANGUAGE LambdaCase
           , ViewPatterns
           , ScopedTypeVariables
           #-}
-- | Functions that convert the declarations of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.AST.FromGHC.Decls where

import ApiAnnotation as GHC (AnnKeywordId(..))
import Bag as GHC (bagToList)
import BasicTypes as GHC
import BooleanFormula as GHC (BooleanFormula(..))
import Class as GHC (FunDep)
import ForeignCall as GHC (Safety(..), CExportSpec(..), CCallConv(..))
import qualified GHC
import HsSyn as GHC
import Name as GHC (Name, occNameString, nameOccName, isSymOcc)
import Outputable as GHC (Outputable(..), showSDocUnsafe)
import RdrName as GHC (RdrName, rdrNameOcc)
import SrcLoc as GHC
import TyCon as GHC (Role(..))

import Control.Monad.Reader
import Control.Reference ((.-), (!~), biplateRef)
import Data.Generics.Uniplate.Data ()
import Data.List
import Data.Maybe (Maybe(..), fromMaybe)

import Language.Haskell.Tools.AST.FromGHC.Binds
import Language.Haskell.Tools.AST.FromGHC.Exprs (trfExpr)
import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Language.Haskell.Tools.AST.FromGHC.Kinds
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Names
import Language.Haskell.Tools.AST.FromGHC.Patterns (trfPattern)
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.TH (trfSplice)
import Language.Haskell.Tools.AST.FromGHC.Types
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST (Ann, AnnMaybeG, AnnListG, getRange, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.SemaInfoTypes as AST (nameInfo)

trfDecls :: TransformName n r => [LHsDecl n] -> Trf (AnnListG AST.UDecl (Dom r) RangeStage)
-- TODO: filter documentation comments
trfDecls decls = addToCurrentScope decls $ makeIndentedListNewlineBefore atTheEnd (mapM trfDecl decls)

trfDeclsGroup :: forall n r . TransformName n r => HsGroup n -> Trf (AnnListG AST.UDecl (Dom r) RangeStage)
trfDeclsGroup (HsGroup vals splices tycls insts derivs fixities defaults foreigns warns anns rules vects _)
  = do spls <- getDeclSplices
       let (sigs, bagToList -> binds) = getBindsAndSigs vals
           alldecls :: [Located (HsDecl n)]
           alldecls = (map (fmap SpliceD) splices)
                        ++ (map (fmap ValD) binds)
                        ++ (map (fmap SigD) sigs)
                        ++ (map (fmap TyClD) (concat $ map group_tyclds tycls))
                        ++ (map (fmap InstD) insts)
                        ++ (map (fmap DerivD) derivs)
                        ++ (map (fmap (SigD . FixSig)) (mergeFixityDefs fixities))
                        ++ (map (fmap DefD) defaults)
                        ++ (map (fmap ForD) foreigns)
                        ++ (map (fmap WarningD) warns)
                        ++ (map (fmap AnnD) anns)
                        ++ (map (fmap RuleD) rules)
                        ++ (map (fmap VectD) vects)
       let actualDefinitions = removeContained $ orderElems $ replaceSpliceDecls spls alldecls
       addToCurrentScope actualDefinitions
         $ makeIndentedListNewlineBefore atTheEnd
            (orderDefs <$> ((++) <$> getDeclsToInsert <*> (mapM trfDecl actualDefinitions)))
  where
    replaceSpliceDecls :: [Located (HsSplice n)] -> [Located (HsDecl n)] -> [Located (HsDecl n)]
    replaceSpliceDecls splices decls = foldl mergeSplice decls splices

    orderElems :: [Located a] -> [Located a]
    orderElems = sortOn (srcSpanStart . getLoc)

    removeContained :: [Located (HsDecl n)] -> [Located (HsDecl n)]
    removeContained (fst:snd:rest) | RealSrcSpan fstLoc <- getLoc fst
                                   , RealSrcSpan sndLoc <- getLoc snd
                                   , fstLoc `containsSpan` sndLoc
      = removeContained (fst:rest)
    removeContained (fst:rest) = fst : removeContained rest
    removeContained [] = []

    mergeSplice :: [Located (HsDecl n)] -> Located (HsSplice n) -> [Located (HsDecl n)]
    mergeSplice decls spl@(L spLoc@(RealSrcSpan rss) _)
      = L spLoc (SpliceD (SpliceDecl spl ExplicitSplice)) : filter (\(L (RealSrcSpan rdsp) _) -> not (rss `containsSpan` rdsp)) decls
    mergeSplice _ (L (UnhelpfulSpan {}) _) = error "mergeSplice: no real span"

    getDeclsToInsert :: Trf [Ann AST.UDecl (Dom r) RangeStage]
    getDeclsToInsert = do decls <- asks declsToInsert
                          allLocals <- asks localsInScope
                          case allLocals of locals:_ -> liftGhc $ mapM (loadIdsForDecls locals) decls
                                            [] -> error "getDeclsToInsert: empty scope"
       where loadIdsForDecls :: [GHC.Name] -> Ann AST.UDecl (Dom RdrName) RangeStage -> GHC.Ghc (Ann AST.UDecl (Dom r) RangeStage)
             loadIdsForDecls locals = AST.semaTraverse $
                AST.SemaTrf (AST.nameInfo !~ findName) pure (traverse findName) pure pure pure
               where findName rdr = pure $ fromGHCName $ fromMaybe (error $ "Data definition name not found: " ++ showSDocUnsafe (ppr rdr)
                                                                              ++ ", locals: " ++ (concat $ intersperse ", " $ map (showSDocUnsafe . ppr) locals))
                                                       $ find ((occNameString (rdrNameOcc rdr) ==) . occNameString . nameOccName) locals

trfDecl :: TransformName n r => Located (HsDecl n) -> Trf (Ann AST.UDecl (Dom r) RangeStage)
trfDecl = trfLocNoSema $ \case
  TyClD (FamDecl (FamilyDecl (ClosedTypeFamily typeEqs) name tyVars kindSig inj))
    -> AST.UClosedTypeFamilyDecl <$> focusAfter AnnType (createDeclHead name tyVars)
                                <*> trfFamilyResultSig kindSig inj
                                <*> trfTypeEqs typeEqs
  TyClD (FamDecl fd) -> AST.UTypeFamilyDecl <$> annContNoSema (trfTypeFam' fd)
  TyClD (SynDecl name vars rhs _)
    -> AST.UTypeDecl <$> between AnnType AnnEqual (createDeclHead name vars) <*> trfType rhs
  TyClD (DataDecl name vars (HsDataDefn nd ctx _ kind cons derivs) _ _)
    -> do let ctxTok = case nd of DataType -> AnnData
                                  NewType -> AnnNewtype
              consLoc = focusBeforeIfPresent AnnDeriving atTheEnd
          whereLoc <- tokenLoc AnnWhere
          if isGoodSrcSpan whereLoc then trfGADT nd name vars ctx kind cons derivs ctxTok consLoc
                                    else trfDataDef nd name vars ctx cons derivs ctxTok consLoc
  TyClD (ClassDecl ctx name vars funDeps sigs defs typeFuns typeFunDefs _ _)
    -> AST.UClassDecl <$> trfCtx (after AnnClass) ctx
                     <*> betweenIfPresent AnnClass AnnWhere (createDeclHead name vars)
                     <*> trfFunDeps funDeps
                     <*> createClassBody sigs defs typeFuns typeFunDefs
  InstD (ClsInstD (ClsInstDecl typ binds sigs typefam datafam overlap))
    -> AST.UInstDecl <$> trfMaybeDefault " " "" trfOverlap (after AnnInstance) overlap
                    <*> trfInstanceRule (hsib_body typ)
                    <*> trfInstBody binds sigs typefam datafam
  InstD (DataFamInstD (DataFamInstDecl con pats (HsDataDefn nd _ _ _ cons derivs) _))
    -> AST.UDataInstDecl <$> trfDataKeyword nd
                        <*> (focusAfter AnnInstance . focusBeforeIfPresent AnnEqual . focusBeforeIfPresent AnnDeriving)
                              (makeInstanceRuleTyVars con pats)
                                                       -- the location is needed when there is no = sign
                        <*> makeListBefore " = " " | " (pure $ srcSpanStart $ foldLocs $ getLoc con : map getLoc (hsib_body pats)) (mapM trfConDecl cons)
                        <*> trfMaybe "" "" trfDerivings derivs
  InstD (TyFamInstD (TyFamInstDecl (L _ (TyFamEqn con pats rhs)) _))
    -> AST.UTypeInstDecl <$> between AnnInstance AnnEqual (makeInstanceRuleTyVars con pats) <*> trfType rhs
  ValD bind -> trfVal bind
  SigD sig -> trfSig sig
  DerivD (DerivDecl t overlap) -> AST.UDerivDecl <$> trfMaybeDefault " " "" trfOverlap (after AnnInstance) overlap <*> trfInstanceRule (hsib_body t)
  RuleD (HsRules _ rules) -> AST.UPragmaDecl <$> annContNoSema (AST.URulePragma <$> makeIndentedList (before AnnClose) (mapM trfRewriteRule rules))
  RoleAnnotD (RoleAnnotDecl name roles) -> AST.URoleDecl <$> trfQualifiedName False name <*> makeList " " atTheEnd (mapM trfRole roles)
  DefD (DefaultDecl types) -> AST.UDefaultDecl <$> makeList "," (after AnnOpenP) (mapM trfType types)
  ForD (ForeignImport name (hsib_body -> typ) _ (CImport ccall safe _ _ _))
    -> AST.UForeignImport <$> trfCallConv ccall <*> trfSafety (getLoc ccall) safe <*> define (trfName name) <*> trfType typ
  ForD (ForeignExport name (hsib_body -> typ) _ (CExport (L l (CExportStatic _ _ ccall)) _))
    -> AST.UForeignExport <$> annLocNoSema (pure l) (trfCallConv' ccall) <*> trfName name <*> trfType typ
  SpliceD (SpliceDecl (unLoc -> spl) _) -> AST.USpliceDecl <$> trfSplice spl
  WarningD (Warnings src [])
    -> AST.UPragmaDecl <$> annContNoSema (AST.UDeprPragma <$> (makeList " " (after AnnOpen) (pure []))
                                                          <*> makeList ", " (before AnnClose) (pure []))
  WarningD (Warnings src [L _ (Warning names (DeprecatedTxt _ stringLits))])
    -> AST.UPragmaDecl <$> annContNoSema (AST.UDeprPragma <$> (makeList " " (after AnnOpen) $ mapM trfName names)
                                                          <*> makeList ", " (before AnnClose) (mapM (\(L l (StringLiteral _ fs)) -> trfFastString (L l fs)) stringLits))
  WarningD (Warnings src [L _ (Warning names (WarningTxt _ stringLits))])
    -> AST.UPragmaDecl <$> annContNoSema (AST.UWarningPragma <$> (makeNonemptyList " " $ mapM trfName names)
                                                             <*> makeList ", " (before AnnClose) (mapM (\(L l (StringLiteral _ fs)) -> trfFastString (L l fs)) stringLits))
  AnnD (HsAnnotation stxt subject expr)
    -> AST.UPragmaDecl <$> annContNoSema (AST.UAnnPragma <$> trfAnnotationSubject stxt subject (srcSpanStart $ getLoc expr) <*> trfExpr expr)
  d -> unhandledElement "declaration" d

trfGADT :: TransformName n r => NewOrData -> Located n -> LHsQTyVars n -> Located (HsContext n)
                                 -> Maybe (Located (HsKind n)) -> [Located (ConDecl n)]
                                 -> Maybe (Located [LHsSigType n]) -> AnnKeywordId -> Trf SrcLoc -> Trf (AST.UDecl (Dom r) RangeStage)
trfGADT nd name vars ctx kind cons derivs ctxTok consLoc
  = AST.UGDataDecl <$> trfDataKeyword nd
                   <*> trfCtx (after ctxTok) ctx
                   <*> betweenIfPresent ctxTok AnnEqual (createDeclHead name vars)
                   <*> focusBefore AnnWhere (trfKindSig kind)
                   <*> makeIndentedListBefore " where " consLoc (mapM trfGADTConDecl cons)
                   <*> trfMaybe "" "" trfDerivings derivs

trfDataDef :: TransformName n r => NewOrData -> Located n -> LHsQTyVars n -> Located (HsContext n)
                                     -> [Located (ConDecl n)] -> Maybe (Located [LHsSigType n])
                                     -> AnnKeywordId -> Trf SrcLoc -> Trf (AST.UDecl (Dom r) RangeStage)
trfDataDef nd name vars ctx cons derivs ctxTok consLoc
  = AST.UDataDecl <$> trfDataKeyword nd
                  <*> trfCtx (after ctxTok) ctx
                  <*> betweenIfPresent ctxTok AnnEqual (createDeclHead name vars)
                  <*> makeListBefore "=" " | " consLoc (mapM trfConDecl cons)
                  <*> trfMaybe "" "" trfDerivings derivs

trfVal :: TransformName n r => HsBindLR n n -> Trf (AST.UDecl (Dom r) RangeStage)
trfVal (PatSynBind psb) = AST.UPatternSynonymDecl <$> annContNoSema (trfPatternSynonym psb)
trfVal bind = AST.UValueBinding <$> (annContNoSema $ trfBind' bind)

trfSig :: TransformName n r => Sig n -> Trf (AST.UDecl (Dom r) RangeStage)
trfSig (ts @ (TypeSig {})) = AST.UTypeSigDecl <$> defineTypeVars (annContNoSema $ trfTypeSig' ts)
trfSig (FixSig fs) = AST.UFixityDecl <$> (annContNoSema $ trfFixitySig fs)
trfSig (PatSynSig id typ)
  = AST.UPatTypeSigDecl <$> annContNoSema (AST.UPatternTypeSignature <$> trfName id <*> trfType (hsib_body typ))
trfSig (InlineSig name prag)
  = AST.UPragmaDecl <$> annContNoSema (AST.UInlinePragmaDecl <$> trfInlinePragma name prag)
trfSig (SpecSig name (map hsib_body -> types) (inl_act -> phase))
  = AST.UPragmaDecl <$> annContNoSema (AST.USpecializeDecl <$> trfSpecializePragma name types phase)
trfSig s = unhandledElement "signature" s

trfSpecializePragma :: TransformName n r
                    => Located n -> [Located (HsType n)] -> Activation -> Trf (Ann AST.USpecializePragma (Dom r) RangeStage)
trfSpecializePragma name types phase
  = annContNoSema $ AST.USpecializePragma <$> trfPhase (pure $ srcSpanStart (getLoc name)) phase
                                          <*> trfName name
                                          <*> (orderAnnList <$> trfAnnList ", " trfType' types)

trfConDecl :: TransformName n r => Located (ConDecl n) -> Trf (Ann AST.UConDecl (Dom r) RangeStage)
trfConDecl = trfLocNoSema trfConDecl'

trfConDecl' :: TransformName n r => ConDecl n -> Trf (AST.UConDecl (Dom r) RangeStage)
trfConDecl' (ConDeclH98 { con_name = name, con_qvars = tyVars, con_cxt = ctx, con_details = PrefixCon args })
  = AST.UConDecl <$> trfConTyVars tyVars <*> trfConCtx ctx <*> define (trfName name) <*> makeList " " atTheEnd (mapM trfType args)
trfConDecl' (ConDeclH98 { con_name = name, con_qvars = tyVars, con_cxt = ctx, con_details = RecCon (unLoc -> flds) })
  = AST.URecordDecl <$> trfConTyVars tyVars <*> trfConCtx ctx <*> define (trfName name) <*> (between AnnOpenC AnnCloseC $ trfAnnList ", " trfFieldDecl' flds)
trfConDecl' (ConDeclH98 { con_name = name, con_qvars = tyVars, con_cxt = ctx, con_details = InfixCon t1 t2 })
  = AST.UInfixConDecl <$> trfConTyVars tyVars <*> trfConCtx ctx <*> trfType t1 <*> define (trfOperator name) <*> trfType t2
trfConDecl' (ConDeclGADT {}) = error "trfConDecl': GADT con received"

trfConTyVars :: TransformName n r => Maybe (LHsQTyVars n) -> Trf (AnnListG AST.UTyVar (Dom r) RangeStage)
trfConTyVars Nothing = makeListAfter "." " " atTheStart (return [])
trfConTyVars (Just vars) = trfBindings $ hsq_explicit vars

trfConCtx :: TransformName n r => Maybe (LHsContext n) -> Trf (AnnMaybeG AST.UContext (Dom r) RangeStage)
trfConCtx Nothing = nothing "" " => " atTheStart
trfConCtx (Just ctx) = trfCtx atTheStart ctx

trfGADTConDecl :: TransformName n r => Located (ConDecl n) -> Trf (Ann AST.UGadtConDecl (Dom r) RangeStage)
trfGADTConDecl = trfLocNoSema $ \(ConDeclGADT { con_names = names, con_type = hsib_body -> typ })
  -> let nameLoc = collectLocs names
         typLoc = getLoc typ
         (vars, ctx, t) = getTypeVarsAndCtx typ
      in AST.UGadtConDecl <$> define (trfAnnList ", " trfName' names)
                          <*> focusOn (mkSrcSpan (srcSpanEnd nameLoc) (srcSpanStart typLoc)) (trfBindings vars)
                          <*> updateFocus (return . updateEnd (\_ -> srcSpanStart typLoc)) (focusAfterIfPresent AnnDot (trfCtx atTheStart ctx))
                          <*> trfGadtConType t
  where getTypeVarsAndCtx :: LHsType n -> ([LHsTyVarBndr n], LHsContext n, LHsType n)
        getTypeVarsAndCtx (L _ (HsForAllTy [] typ)) = getTypeVarsAndCtx typ
        getTypeVarsAndCtx (L _ (HsForAllTy bndrs typ)) = let (_,ctx,t) = getTypeVarsAndCtx typ in (bndrs, ctx, t)
        getTypeVarsAndCtx (L _ (HsQualTy ctx typ)) = let (vars,_,t) = getTypeVarsAndCtx typ in (vars, ctx, t)
        getTypeVarsAndCtx t = ([], L noSrcSpan [], t)

trfGadtConType :: TransformName n r => Located (HsType n) -> Trf (Ann AST.UGadtConType (Dom r) RangeStage)
trfGadtConType = trfLocNoSema $ \case
  HsFunTy (cleanHsType . unLoc -> HsRecTy flds) resType
    -> AST.UGadtRecordType <$> between AnnOpenC AnnCloseC (trfAnnList ", " trfFieldDecl' flds)
                           <*> trfType resType
  typ -> AST.UGadtNormalType <$> annContNoSema (trfType' typ)

trfFieldDecl :: TransformName n r => Located (ConDeclField n) -> Trf (Ann AST.UFieldDecl (Dom r) RangeStage)
trfFieldDecl = trfLocNoSema trfFieldDecl'

trfFieldDecl' :: TransformName n r => ConDeclField n -> Trf (AST.UFieldDecl (Dom r) RangeStage)
trfFieldDecl' (ConDeclField names typ _) = AST.UFieldDecl <$> (define $ nonemptyAnnList <$> mapM (trfName . getFieldOccName) names) <*> trfType typ

trfDerivings :: TransformName n r => Located [LHsSigType n] -> Trf (Ann AST.UDeriving (Dom r) RangeStage)
trfDerivings = trfLocNoSema $ \case
  [hsib_body -> typ@(unLoc -> HsTyVar {})] -> AST.UDerivingOne <$> trfInstanceHead typ
  derivs -> AST.UDerivings <$> trfAnnList ", " trfInstanceHead' (map hsib_body derivs)

trfInstanceRule :: TransformName n r => Located (HsType n) -> Trf (Ann AST.UInstanceRule (Dom r) RangeStage)
trfInstanceRule = trfLocNoSema (trfInstanceRule' . cleanHsType)

trfInstanceRule' :: TransformName n r => HsType n -> Trf (AST.UInstanceRule (Dom r) RangeStage)
trfInstanceRule' (HsForAllTy bndrs (unLoc -> HsQualTy ctx typ))
  = AST.UInstanceRule <$> (makeJust <$> annLocNoSema (pure $ collectLocs bndrs) (trfBindings bndrs))
                      <*> trfCtx (after AnnDot) ctx
                      <*> trfInstanceHead typ
trfInstanceRule' (HsQualTy ctx typ) = AST.UInstanceRule <$> nothing "" " . " atTheStart
                                                        <*> trfCtx atTheStart ctx
                                                        <*> trfInstanceHead typ
trfInstanceRule' (HsParTy typ) = instanceHead $ annContNoSema (AST.UInstanceHeadParen <$> trfInstanceHead typ)
trfInstanceRule' (HsTyVar tv) = instanceHead $ annContNoSema (AST.UInstanceHeadCon <$> trfName tv)
trfInstanceRule' (HsAppTy t1 t2) = instanceHead $ annContNoSema (AST.UInstanceHeadApp <$> trfInstanceHead t1 <*> trfType t2)
trfInstanceRule' (HsOpTy t1 op t2) = instanceHead $ annContNoSema (AST.UInstanceHeadApp <$> annLocNoSema (pure $ getLoc t1 `combineSrcSpans` getLoc op) (AST.UInstanceHeadInfix <$> trfType t1 <*> trfOperator op) <*> trfType t2)
trfInstanceRule' t = unhandledElement "instance rule" t

instanceHead :: Trf (Ann AST.UInstanceHead (Dom r) RangeStage) -> Trf (AST.UInstanceRule (Dom r) RangeStage)
instanceHead hd = AST.UInstanceRule <$> (nothing "" " . " atTheStart) <*> (nothing " " "" atTheStart) <*> hd

makeInstanceRuleTyVars :: TransformName n r => Located n -> HsImplicitBndrs n [LHsType n] -> Trf (Ann AST.UInstanceRule (Dom r) RangeStage)
makeInstanceRuleTyVars n vars
  | isSymOcc (occName (unLoc n))
  , leftOp:rest <- hsib_body vars
  , srcSpanStart (getLoc n) > srcSpanEnd (getLoc leftOp)
  = annContNoSema
      $ AST.UInstanceRule <$> nothing "" " . " atTheStart
                          <*> nothing " " "" atTheStart
                          <*> foldl foldTypeArgs
                                    (annLocNoSema (pure $ combineSrcSpans (getLoc leftOp) (getLoc n))
                                      (AST.UInstanceHeadInfix <$> trfType leftOp <*> trfOperator n)) rest
  | otherwise
  = annContNoSema
      $ AST.UInstanceRule <$> nothing "" " . " atTheStart
                          <*> nothing " " "" atTheStart
                          <*> foldl foldTypeArgs (copyAnnot AST.UInstanceHeadCon (trfName n)) (hsib_body vars)
  where foldTypeArgs base typ = annLocNoSema (pure $ combineSrcSpans (getLoc n) (getLoc typ)) $ AST.UInstanceHeadApp <$> base <*> (trfType typ)


trfInstanceHead :: TransformName n r => Located (HsType n) -> Trf (Ann AST.UInstanceHead (Dom r) RangeStage)
trfInstanceHead = trfLocNoSema trfInstanceHead'

trfInstanceHead' :: TransformName n r => HsType n -> Trf (AST.UInstanceHead (Dom r) RangeStage)
trfInstanceHead' = trfInstanceHead'' . cleanHsType where
  trfInstanceHead'' (HsForAllTy [] (unLoc -> t)) = trfInstanceHead' t
  trfInstanceHead'' (HsTyVar tv) = AST.UInstanceHeadCon <$> trfName tv
  trfInstanceHead'' (HsAppTy t1 t2) = AST.UInstanceHeadApp <$> trfInstanceHead t1 <*> trfType t2
  trfInstanceHead'' (HsParTy typ) = AST.UInstanceHeadParen <$> trfInstanceHead typ
  trfInstanceHead'' (HsOpTy t1 op t2)
    = AST.UInstanceHeadApp <$> (annLocNoSema (pure $ combineSrcSpans (getLoc t1) (getLoc op))
                                             (AST.UInstanceHeadInfix <$> trfType t1 <*> trfOperator op))
                          <*> trfType t2
  trfInstanceHead'' t = unhandledElement "instance head" t

trfTypeEqs :: TransformName n r => Maybe [Located (TyFamInstEqn n)] -> Trf (AnnListG AST.UTypeEqn (Dom r) RangeStage)
trfTypeEqs eqs =
  do toks <- tokensAfter AnnWhere
     case toks of [] -> error "trfTypeEqs: no where found after closed type family"
                  loc:_ -> makeList "\n" (pure $ srcSpanStart loc) (mapM trfTypeEq (fromMaybe [] eqs))

trfTypeEq :: TransformName n r => Located (TyFamInstEqn n) -> Trf (Ann AST.UTypeEqn (Dom r) RangeStage)
trfTypeEq = trfLocNoSema $ \(TyFamEqn name pats rhs)
  -> AST.UTypeEqn <$> defineTypeVars (focusBefore AnnEqual (combineTypes name (hsib_body pats))) <*> trfType rhs
  where combineTypes :: TransformName n r => Located n -> [LHsType n] -> Trf (Ann AST.UType (Dom r) RangeStage)
        combineTypes name [lhs, rhs] | srcSpanStart (getLoc name) > srcSpanEnd (getLoc lhs)
          = annContNoSema $ AST.UTyInfix <$> trfType lhs <*> trfOperator name <*> trfType rhs
        combineTypes name pats = wrapTypes (annLocNoSema (pure $ getLoc name) (AST.UTyVar <$> trfName name)) pats

        wrapTypes :: TransformName n r => Trf (Ann AST.UType (Dom r) RangeStage) -> [LHsType n] -> Trf (Ann AST.UType (Dom r) RangeStage)
        wrapTypes base pats
          = foldl (\t p -> do typ <- t
                              annLocNoSema (pure $ combineSrcSpans (getRange typ) (getLoc p))
                                     (AST.UTyApp <$> pure typ <*> trfType p)) base pats

trfFunDeps :: TransformName n r => [Located (FunDep (Located n))] -> Trf (AnnMaybeG AST.UFunDeps (Dom r) RangeStage)
trfFunDeps [] = do whereToken <- tokenLoc AnnWhere
                   nothing "| " "" (if isGoodSrcSpan whereToken then pure $ srcSpanStart whereToken else atTheEnd)
trfFunDeps fundeps = makeJust <$> annLocNoSema (combineSrcSpans (collectLocs fundeps) <$> tokenLoc AnnVbar)
                                         (AST.UFunDeps <$> trfAnnList ", " trfFunDep' fundeps)

trfFunDep' :: TransformName n r => FunDep (Located n) -> Trf (AST.UFunDep (Dom r) RangeStage)
trfFunDep' (lhs, rhs) = AST.UFunDep <$> trfAnnList ", " trfName' lhs <*> trfAnnList ", " trfName' rhs

createDeclHead :: TransformName n r => Located n -> LHsQTyVars n -> Trf (Ann AST.UDeclHead (Dom r) RangeStage)
createDeclHead name (hsq_explicit -> lhs : rhs : rest)
  | srcSpanStart (getLoc name) > srcSpanEnd (getLoc lhs)
  -- infix declaration
  = wrapDeclHead rest
      $ annLocNoSema (addParenLocs $ getLoc lhs `combineSrcSpans` getLoc rhs)
                     (AST.UDHInfix <$> defineTypeVars (trfTyVar lhs) <*> define (trfOperator name) <*> defineTypeVars (trfTyVar rhs))
createDeclHead name vars = defineTypeVars $ wrapDeclHead (hsq_explicit vars) (define $ copyAnnot AST.UDeclHead (trfName name))

wrapDeclHead :: TransformName n r => [LHsTyVarBndr n] -> Trf (Ann AST.UDeclHead (Dom r) RangeStage) -> Trf (Ann AST.UDeclHead (Dom r) RangeStage)
wrapDeclHead vars base
  = foldl (\t p -> do typ <- t
                      annLocNoSema (addParenLocs $ combineSrcSpans (getRange typ) (getLoc p))
                             (AST.UDHApp typ <$> trfTyVar p)
          ) base vars

-- | Get the parentheses directly before and after (for parenthesized application)
addParenLocs :: SrcSpan -> Trf SrcSpan
addParenLocs sp
  = let possibleSpan = mkSrcSpan (updateCol (subtract 1) (srcSpanStart sp)) (updateCol (+1) (srcSpanEnd sp))
     in local (\s -> s { contRange = possibleSpan })
              (combineSrcSpans <$> (combineSrcSpans sp <$> tokenLoc AnnOpenP) <*> tokenLocBack AnnCloseP)


createClassBody :: TransformName n r => [LSig n] -> LHsBinds n -> [LFamilyDecl n]
                               -> [LTyFamDefltEqn n] -> Trf (AnnMaybeG AST.UClassBody (Dom r) RangeStage)
createClassBody sigs binds typeFams typeFamDefs
  = do isThereWhere <- isGoodSrcSpan <$> (tokenLoc AnnWhere)
       if isThereWhere
         then makeJust <$> annLocNoSema (combinedLoc <$> tokenLoc AnnWhere)
                                        (AST.UClassBody <$> makeList "" (after AnnWhere)
                                                                       (orderDefs . concat <$> sequenceA allDefs))
         else nothing " where " "" atTheEnd
  where combinedLoc wh = foldl combineSrcSpans wh allLocs
        allLocs = map getLoc sigs ++ map getLoc (bagToList binds) ++ map getLoc typeFams ++ map getLoc typeFamDefs
        allDefs = [getSigs, getBinds, getFams, getFamDefs]
        getSigs = mapM trfClassElemSig sigs
        getBinds = mapM (copyAnnot AST.UClsDef . trfBind) (bagToList binds)
        getFams = mapM (copyAnnot AST.UClsTypeFam . trfTypeFam) typeFams
        getFamDefs = mapM trfTypeFamDef typeFamDefs

trfClassElemSig :: TransformName n r => Located (Sig n) -> Trf (Ann AST.UClassElement (Dom r) RangeStage)
trfClassElemSig = trfLocNoSema $ \case
  TypeSig names typ -> AST.UClsSig <$> (annContNoSema $ AST.UTypeSignature <$> define (makeNonemptyList ", " (mapM trfName names))
                                  <*> trfType (hswc_body $ hsib_body typ))
  ClassOpSig True [name] typ -> AST.UClsDefSig <$> trfName name <*> trfType (hsib_body typ)
  ClassOpSig False names typ -> AST.UClsSig <$> (annContNoSema $ AST.UTypeSignature <$> define (makeNonemptyList ", " (mapM trfName names))
                                           <*> trfType (hsib_body typ))
  MinimalSig _ formula -> AST.UClsMinimal <$> trfMinimalFormula formula
  InlineSig name prag -> AST.UClsInline <$> trfInlinePragma name prag
  FixSig fixity -> AST.UClsFixity <$> annContNoSema (trfFixitySig fixity)
  s -> unhandledElement "signature in class" s

trfTypeFam :: TransformName n r => Located (FamilyDecl n) -> Trf (Ann AST.UTypeFamily (Dom r) RangeStage)
trfTypeFam = trfLocNoSema trfTypeFam'

trfTypeFam' :: TransformName n r => FamilyDecl n -> Trf (AST.UTypeFamily (Dom r) RangeStage)
trfTypeFam' (FamilyDecl DataFamily name tyVars kindSig _)
  = AST.UDataFamily <$> (case unLoc kindSig of KindSig _ -> between AnnData AnnDcolon; _ -> id) (createDeclHead name tyVars)
                   <*> trfFamilyKind kindSig
trfTypeFam' (FamilyDecl OpenTypeFamily name tyVars kindSig injectivity)
  = AST.UTypeFamily <$> (case unLoc kindSig of KindSig _ -> between AnnType AnnDcolon; _ -> id) (createDeclHead name tyVars)
                   <*> trfFamilyResultSig kindSig injectivity
trfTypeFam' (FamilyDecl (ClosedTypeFamily {}) _ _ _ _) = error "trfTypeFam': closed type family received"

trfTypeFamDef :: TransformName n r => Located (TyFamDefltEqn n) -> Trf (Ann AST.UClassElement (Dom r) RangeStage)
trfTypeFamDef = trfLocNoSema $ \(TyFamEqn con pats rhs)
  -> AST.UClsTypeDef <$> between AnnType AnnEqual (createDeclHead con pats) <*> trfType rhs

trfInstBody :: TransformName n r => LHsBinds n -> [LSig n] -> [LTyFamInstDecl n] -> [LDataFamInstDecl n] -> Trf (AnnMaybeG AST.UInstBody (Dom r) RangeStage)
trfInstBody binds sigs fams dats = do
    wh <- tokenLoc AnnWhere
    if isGoodSrcSpan wh then
      makeJust <$> annLocNoSema (combinedLoc <$> tokenLoc AnnWhere)
                                (AST.UInstBody <$> (makeList "" (after AnnWhere)
                                                      (orderDefs . concat <$> sequenceA allDefs)))
    else nothing " where " "" atTheEnd
  where combinedLoc wh = foldl combineSrcSpans wh allLocs
        allLocs = map getLoc sigs ++ map getLoc (bagToList binds) ++ map getLoc fams ++ map getLoc dats
        allDefs = [getSigs, getBinds, getFams, getDats]
        getSigs = mapM trfClassInstSig sigs
        getBinds = mapM (copyAnnot AST.UInstBodyNormalDecl . trfBind) (bagToList binds)
        getFams = mapM trfInstTypeFam fams
        getDats = mapM trfInstDataFam dats

trfClassInstSig :: TransformName n r => Located (Sig n) -> Trf (Ann AST.UInstBodyDecl (Dom r) RangeStage)
trfClassInstSig = trfLocNoSema $ \case
  TypeSig names typ -> AST.UInstBodyTypeSig <$> (annContNoSema $ AST.UTypeSignature <$> makeNonemptyList ", " (mapM trfName names)
                                           <*> trfType (hswc_body $ hsib_body typ))
  ClassOpSig _ names typ -> AST.UInstBodyTypeSig <$> (annContNoSema $ AST.UTypeSignature <$> define (makeNonemptyList ", " (mapM trfName names))
                                                <*> trfType (hsib_body typ))
  SpecInstSig _ typ -> AST.USpecializeInstance <$> trfType (hsib_body typ)
  SpecSig name (map hsib_body -> tys) (inl_act -> phase) -> AST.UInstanceSpecialize <$> trfSpecializePragma name tys phase
  InlineSig name prag -> AST.UInlineInstance <$> trfInlinePragma name prag
  s -> unhandledElement "class instance signature" s

trfInstTypeFam :: TransformName n r => Located (TyFamInstDecl n) -> Trf (Ann AST.UInstBodyDecl (Dom r) RangeStage)
trfInstTypeFam (unLoc -> TyFamInstDecl eqn _) = copyAnnot AST.UInstBodyTypeDecl (trfTypeEq eqn)

trfInstDataFam :: TransformName n r => Located (DataFamInstDecl n) -> Trf (Ann AST.UInstBodyDecl (Dom r) RangeStage)
trfInstDataFam = trfLocNoSema $ \case
  (DataFamInstDecl tc (hsib_body -> pats) (HsDataDefn dn ctx _ _ cons derivs) _)
    -> AST.UInstBodyDataDecl
         <$> trfDataKeyword dn
         <*> annLocNoSema (pure $ collectLocs pats `combineSrcSpans` getLoc tc `combineSrcSpans` getLoc ctx)
                          (AST.UInstanceRule <$> nothing "" " . " atTheStart
                                             <*> trfCtx atTheStart ctx
                                             <*> foldl (\r t -> annLocNoSema (combineSrcSpans (getLoc t) . getRange <$> r)
                                                                             (AST.UInstanceHeadApp <$> r <*> (trfType t)))
                                                       (copyAnnot AST.UInstanceHeadCon (trfName tc)) pats)
         <*> trfAnnList "" trfConDecl' cons
         <*> trfMaybe " deriving " "" trfDerivings derivs

trfPatternSynonym :: forall n r . TransformName n r => PatSynBind n n -> Trf (AST.UPatternSynonym (Dom r) RangeStage)
trfPatternSynonym (PSB id _ lhs def dir)
  = let sep = case dir of ImplicitBidirectional -> AnnEqual
                          _                     -> AnnLarrow
        rhsLoc = combineSrcSpans (getLoc def) <$> tokenLoc sep
        -- we use the selector name instead of the pattern variable name
        rewrites = case lhs of RecordPatSyn flds -> map (\r -> (unLoc (recordPatSynPatVar r), unLoc (recordPatSynSelectorId r))) flds
                               _                 -> []
        changedRhs = biplateRef .- (\n -> case lookup n rewrites of Just x -> x; Nothing -> n) $ def
     in AST.UPatternSynonym <$> trfPatSynLhs id lhs
                            <*> annLocNoSema rhsLoc (trfPatSynRhs dir changedRhs)

  where trfPatSynLhs :: Located n -> HsPatSynDetails (Located n) -> Trf (Ann AST.UPatSynLhs (Dom r) RangeStage)
        trfPatSynLhs id (PrefixPatSyn args)
          = annLocNoSema (pure $ foldLocs (getLoc id : map getLoc args)) $ AST.UNormalPatSyn <$> trfName id <*> trfAnnList " " trfName' args
        trfPatSynLhs op (InfixPatSyn lhs rhs)
          = annLocNoSema (pure $ getLoc lhs `combineSrcSpans` getLoc rhs) $ AST.UInfixPatSyn <$> trfName lhs <*> trfOperator op <*> trfName rhs
        trfPatSynLhs id (RecordPatSyn flds)
          = annLocNoSema (mkSrcSpan (srcSpanStart (getLoc id)) <$> before AnnEqual)
              $ AST.URecordPatSyn <$> trfName id <*> trfAnnList ", " trfName' (map recordPatSynSelectorId flds)

        trfPatSynRhs :: HsPatSynDir n -> Located (Pat n) -> Trf (AST.UPatSynRhs (Dom r) RangeStage)
        trfPatSynRhs ImplicitBidirectional pat = AST.UBidirectionalPatSyn <$> trfPattern pat <*> nothing " where " "" atTheEnd
        trfPatSynRhs (ExplicitBidirectional mg) pat = AST.UBidirectionalPatSyn <$> trfPattern pat <*> (makeJust <$> trfPatSynWhere mg)
        trfPatSynRhs Unidirectional pat = AST.UOneDirectionalPatSyn <$> trfPattern pat

        trfPatSynWhere :: MatchGroup n (LHsExpr n) -> Trf (Ann AST.UPatSynWhere (Dom r) RangeStage)
        trfPatSynWhere (MG { mg_alts = alts }) = annLocNoSema (pure $ getLoc alts) (AST.UPatSynWhere <$> makeIndentedList (after AnnWhere) (mapM (trfMatch (unLoc id)) (unLoc alts)))

trfFamilyKind :: TransformName n r => Located (FamilyResultSig n) -> Trf (AnnMaybeG AST.UKindConstraint (Dom r) RangeStage)
trfFamilyKind (unLoc -> fr) = case fr of
  NoSig -> nothing "" " " atTheEnd
  KindSig k -> trfKindSig (Just k)
  TyVarSig tv -> error "trfFamilyKind: TyVarSig not supported"

trfFamilyResultSig :: TransformName n r => Located (FamilyResultSig n) -> Maybe (LInjectivityAnn n) -> Trf (AnnMaybeG AST.UTypeFamilySpec (Dom r) RangeStage)
trfFamilyResultSig (L l fr) Nothing = case fr of
  NoSig -> nothing "" " " atTheEnd
  KindSig k -> makeJust <$> (annLocNoSema (pure l) $ AST.UTypeFamilyKind <$> trfKindSig' k)
  TyVarSig {} -> error "trfFamilyResultSig: TyVarSig not supported" {- makeJust <$> (annLocNoSema (combineSrcSpans (getLoc tv) <$> (tokenBefore (srcSpanStart (getLoc tv)) AnnDcolon))
                              (AST.UKindConstraint <$> trfKindVar tv)) -}
trfFamilyResultSig (L _ sig) (Just (L l (InjectivityAnn n deps)))
  = makeJust <$> (annLocNoSema (pure l) $ AST.UTypeFamilyInjectivity <$> (annContNoSema $ AST.UInjectivityAnn <$> tv <*> trfAnnList ", " trfName' deps))
    where tv = case sig of TyVarSig tv -> trfTyVar tv
                           _ -> annLocNoSema (pure $ getLoc n) (AST.UTyVarDecl <$> trfName n <*> nothing "" "" (pure $ srcSpanEnd (getLoc n)))

trfAnnotationSubject :: TransformName n r => SourceText -> AnnProvenance n -> SrcLoc -> Trf (Ann AST.UAnnotationSubject (Dom r) RangeStage)
trfAnnotationSubject stxt subject payloadEnd
  = do payloadStart <- advanceStr stxt <$> atTheStart
       case subject of ValueAnnProvenance name@(L l _) -> annLocNoSema (pure l) (AST.UNameAnnotation <$> trfName name)
                       TypeAnnProvenance name@(L l _) -> annLocNoSema (pure $ mkSrcSpan payloadStart (srcSpanEnd l))
                                                                      (AST.UTypeAnnotation <$> trfName name)
                       ModuleAnnProvenance -> annLocNoSema (pure $ mkSrcSpan payloadStart payloadEnd) (pure AST.UModuleAnnotation)

trfDataKeyword ::  NewOrData -> Trf (Ann AST.UDataOrNewtypeKeyword (Dom r) RangeStage)
trfDataKeyword NewType = annLocNoSema (tokenLoc AnnNewtype) (pure AST.UNewtypeKeyword)
trfDataKeyword DataType = annLocNoSema (tokenLoc AnnData) (pure AST.UDataKeyword)

trfCallConv :: Located CCallConv -> Trf (Ann AST.UCallConv (Dom r) RangeStage)
trfCallConv = trfLocNoSema trfCallConv'

trfCallConv' :: CCallConv -> Trf (AST.UCallConv (Dom r) RangeStage)
trfCallConv' CCallConv = pure AST.UCCall
trfCallConv' CApiConv = pure AST.UCApi
trfCallConv' StdCallConv = pure AST.UStdCall
trfCallConv' JavaScriptCallConv = pure AST.UJavaScript
trfCallConv' PrimCallConv = error "trfCallConv: PrimCallConv not supported"

trfSafety :: SrcSpan -> Located Safety -> Trf (AnnMaybeG AST.USafety (Dom r) RangeStage)
trfSafety ccLoc lsaf@(L l _) | isGoodSrcSpan l
  = makeJust <$> trfLocNoSema (pure . \case
      PlaySafe -> AST.USafe
      PlayInterruptible -> AST.UInterruptible
      PlayRisky -> AST.UUnsafe) lsaf
  | otherwise = nothing " " "" (pure $ srcSpanEnd ccLoc)

trfOverlap :: Located OverlapMode -> Trf (Ann AST.UOverlapPragma (Dom r) RangeStage)
trfOverlap = trfLocNoSema $ pure . \case
  NoOverlap _ -> AST.UDisableOverlap
  Overlappable _ -> AST.UOverlappable
  Overlapping _ -> AST.UOverlapping
  Overlaps _ -> AST.UOverlaps
  Incoherent _ -> AST.UIncoherentOverlap

trfRole :: Located (Maybe Role) -> Trf (Ann AST.URole (Dom r) RangeStage)
trfRole = trfLocNoSema $ \case Just Nominal -> pure AST.UNominal
                               Just Representational -> pure AST.URepresentational
                               Just GHC.Phantom -> pure AST.UPhantom
                               Nothing -> error "trfRole: no role"

trfRewriteRule :: TransformName n r => Located (RuleDecl n) -> Trf (Ann AST.URule (Dom r) RangeStage)
trfRewriteRule = trfLocNoSema $ \(HsRule (L nameLoc (_, ruleName)) act bndrs left _ right _) ->
  AST.URule <$> trfFastString (L nameLoc ruleName)
            <*> trfPhase (pure $ srcSpanEnd nameLoc) act
            <*> makeListAfter " " " " (pure $ srcSpanStart $ getLoc left) (mapM trfRuleBndr bndrs)
            <*> trfExpr left
            <*> trfExpr right

trfRuleBndr :: TransformName n r => Located (RuleBndr n) -> Trf (Ann AST.URuleVar (Dom r) RangeStage)
trfRuleBndr = trfLocNoSema $ \case (RuleBndr n) -> AST.URuleVar <$> trfName n
                                   (RuleBndrSig n k) -> AST.USigRuleVar <$> trfName n <*> trfType (hswc_body $ hsib_body k)

trfMinimalFormula :: TransformName n r => Located (BooleanFormula (Located n)) -> Trf (Ann AST.UMinimalFormula (Dom r) RangeStage)
trfMinimalFormula = trfLocNoSema trfMinimalFormula'

trfMinimalFormula' :: TransformName n r => BooleanFormula (Located n) -> Trf (AST.UMinimalFormula (Dom r) RangeStage)
trfMinimalFormula' (Var name) = AST.UMinimalName <$> trfName name
trfMinimalFormula' (And formulas) = AST.UMinimalAnd <$> trfAnnList " & " trfMinimalFormula' formulas
trfMinimalFormula' (Or formulas) = AST.UMinimalOr <$> trfAnnList " | " trfMinimalFormula' formulas
trfMinimalFormula' (Parens formula) = AST.UMinimalParen <$> trfMinimalFormula formula
