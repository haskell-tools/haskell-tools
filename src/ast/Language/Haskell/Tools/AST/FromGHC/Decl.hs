{-# LANGUAGE LambdaCase 
           , ViewPatterns
             #-}
module Language.Haskell.Tools.AST.FromGHC.Decl where

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

import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Base as AST
import qualified Language.Haskell.Tools.AST.Decl as AST
import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

trfDecls :: [LHsDecl RdrName] -> Trf (AnnList AST.Decl RI)
-- TODO: filter documentation comments
trfDecls decls = AnnList <$> mapM trfDecl decls

trfDecl :: Located (HsDecl RdrName) -> Trf (Ann AST.Decl RI)
trfDecl = trfLoc $ \case
  TyClD (FamDecl (FamilyDecl DataFamily name tyVars kindSig)) 
    -> AST.TypeFamilyDecl <$> (AST.DataFamily <$> createDeclHead name tyVars <*> trfKindSig kindSig)
  TyClD (FamDecl (FamilyDecl OpenTypeFamily name tyVars kindSig)) 
    -> AST.TypeFamilyDecl <$> (AST.TypeFamily <$> createDeclHead name tyVars <*> trfKindSig kindSig)
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
  ValD bind -> AST.ValueBinding <$> trfBind' bind
  SigD (ts @ (TypeSig {})) -> AST.TypeSigDecl <$> trfTypeSig ts
  SigD (FixSig fs) -> AST.FixityDecl <$> trfFixitySig fs
  -- TODO: pattern synonym type signature
  -- TODO: INLINE, SPECIALIZE, MINIMAL, VECTORISE pragmas, Warnings, Annotations, rewrite rules, role annotations
  DefD (DefaultDecl types) -> AST.DefaultDecl . AnnList <$> mapM trfType types
  ForD (ForeignImport name typ _ (CImport ccall safe _ _ _)) 
    -> AST.ForeignImport <$> trfCallConv ccall <*> trfSafety safe <*> trfName name <*> trfType typ
  ForD (ForeignExport name typ _ (CExport (L l (CExportStatic _ ccall)) _)) 
    -> AST.ForeignExport <$> annLoc (pure l) (trfCallConv' ccall) <*> trfName name <*> trfType typ
  SpliceD (SpliceDecl (unLoc -> spl) _) -> AST.SpliceDecl <$> trfSplice' spl

trfConDecl :: Located (ConDecl RdrName) -> Trf (Ann AST.ConDecl RI)
trfConDecl = trfLoc $ \case 
  ConDecl { con_names = [name], con_details = PrefixCon args }
    -> AST.ConDecl <$> trfName name <*> (AnnList <$> mapM trfType args)
  ConDecl { con_names = [name], con_details = RecCon (unLoc -> flds) }
    -> AST.RecordDecl <$> trfName name <*> (AnnList <$> mapM trfFieldDecl flds)
  ConDecl { con_names = [name], con_details = InfixCon t1 t2 }
    -> AST.InfixConDecl <$> trfName name <*> trfType t1 <*> trfType t2

trfFieldDecl :: Located (ConDeclField RdrName) -> Trf (Ann AST.FieldDecl RI)
trfFieldDecl = trfLoc $ \(ConDeclField names typ _)
  -> AST.FieldDecl <$> (AnnList <$> mapM trfName names) <*> trfType typ

trfDerivings :: Located [LHsType RdrName] -> Trf (Ann AST.Deriving RI)
trfDerivings = trfLoc $ \case
  [typ@(unLoc -> HsTyVar cls)] -> AST.DerivingOne <$> trfInstanceRule typ
  derivs -> AST.Derivings . AnnList <$> mapM trfInstanceRule derivs
  
trfInstanceRule :: Located (HsType RdrName) -> Trf (Ann AST.InstanceRule RI)
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
                                 <*> annLoc (asks contRange) (AST.InstanceHeadCon <$> trfName' tv)
                                 
trfInstanceHead :: Located (HsType RdrName) -> Trf (Ann AST.InstanceHead RI)
trfInstanceHead = trfLoc $ \case
  HsTyVar tv -> AST.InstanceHeadCon <$> trfName' tv
  HsAppTy t1 t2 -> AST.InstanceHeadApp <$> trfInstanceHead t1 <*> trfType t2
  HsParTy typ -> AST.InstanceHeadParen <$> trfInstanceHead typ
  HsOpTy t1 (_,op) t2 
    -> AST.InstanceHeadApp <$> (annLoc (pure $ combineSrcSpans (getLoc t1) (getLoc op))
                                       (AST.InstanceHeadInfix <$> trfType t1 <*> trfName op)) 
                           <*> trfType t2
  
trfBind :: Located (HsBind RdrName) -> Trf (Ann AST.ValueBind RI)
trfBind = trfLoc trfBind'
  
trfBind' :: HsBind RdrName -> Trf (AST.ValueBind RI)
trfBind' (FunBind { fun_id = id, fun_matches = MG { mg_alts = [L matchLoc (Match { m_pats = [], m_grhss = GRHSs [L rhsLoc (GRHS [] expr)] locals })]} }) = AST.SimpleBind <$> (takeAnnot AST.VarPat (trfName id)) <*> annLoc (combineSrcSpans (getLoc expr) <$> tokenLoc AnnEqual) (AST.UnguardedRhs <$> trfExpr expr) <*> trfWhereLocalBinds locals
trfBind' (FunBind id isInfix (MG matches _ _ _) _ _ _) = AST.FunBind . AnnList <$> mapM (trfMatch id) matches
trfBind' (PatBind pat (GRHSs rhs locals) _ _ _) = AST.SimpleBind <$> trfPattern pat <*> trfRhss rhs <*> trfWhereLocalBinds locals
trfBind' (AbsBinds typeVars vars exports _ _) = undefined
trfBind' (PatSynBind psb) = undefined
  
trfMatch :: Located RdrName -> Located (Match RdrName (LHsExpr RdrName)) -> Trf (Ann AST.Match RI)
trfMatch name = trfLoc $ \(Match funid pats typ (GRHSs rhss locBinds))
  -> AST.Match <$> trfName (maybe name fst funid) <*> (AnnList <$> mapM trfPattern pats) <*> trfMaybe trfType typ 
               <*> trfRhss rhss <*> trfWhereLocalBinds locBinds
  
trfRhss :: [Located (GRHS RdrName (LHsExpr RdrName))] -> Trf (Ann AST.Rhs RI)
trfRhss [unLoc -> GRHS [] body] = annLoc (combineSrcSpans (getLoc body) <$> tokenLoc AnnEqual) 
                                         (AST.UnguardedRhs <$> trfExpr body)
trfRhss rhss = annLoc (pure $ collectLocs rhss) 
                      (AST.GuardedRhss . AnnList <$> mapM trfGuardedRhs rhss)
  
trfGuardedRhs :: Located (GRHS RdrName (LHsExpr RdrName)) -> Trf (Ann AST.GuardedRhs RI)
trfGuardedRhs = trfLoc $ \(GRHS guards body) 
  -> AST.GuardedRhs . AnnList <$> mapM trfRhsGuard guards <*> trfExpr body
  
trfRhsGuard :: Located (Stmt RdrName (LHsExpr RdrName)) -> Trf (Ann AST.RhsGuard RI)
trfRhsGuard = trfLoc $ \case
  BindStmt pat body _ _ -> AST.GuardBind <$> trfPattern pat <*> trfExpr body
  BodyStmt body _ _ _ -> AST.GuardCheck <$> trfExpr body
  LetStmt binds -> AST.GuardLet <$> trfLocalBinds binds
  
trfWhereLocalBinds :: HsLocalBinds RdrName -> Trf (AnnMaybe AST.LocalBinds RI)
trfWhereLocalBinds EmptyLocalBinds = pure annNothing  
trfWhereLocalBinds binds@(HsValBinds (ValBindsIn vals sigs)) 
  = annJust <$> annLoc (collectAnnots . _fromAnnList <$> localBinds) (AST.LocalBinds <$> localBinds)
      where localBinds = trfLocalBinds binds


trfLocalBinds :: HsLocalBinds RdrName -> Trf (AnnList AST.LocalBind RI)
trfLocalBinds (HsValBinds (ValBindsIn binds sigs)) 
  = AnnList . orderDefs <$> ((++) <$> mapM (takeAnnot AST.LocalValBind . trfBind) (bagToList binds) 
                                  <*> mapM trfLocalSig sigs)
                                 
trfLocalSig :: Located (Sig RdrName) -> Trf (Ann AST.LocalBind RI)
trfLocalSig = trfLoc $ \case
  ts@(TypeSig {}) -> AST.LocalSignature <$> trfTypeSig ts
  (FixSig fs) -> AST.LocalFixity <$> trfFixitySig fs
  
trfTypeSig :: Sig RdrName -> Trf (AST.TypeSignature RI)
trfTypeSig (TypeSig [name] typ _) = AST.TypeSignature <$> trfName name <*> trfType typ
  
trfFixitySig :: FixitySig RdrName -> Trf (AST.FixitySignature RI)
trfFixitySig (FixitySig names (Fixity prec dir)) 
  = AST.FixitySignature <$> transformDir dir
                        <*> annLoc (tokenLoc AnnVal) (pure $ AST.Precedence prec) 
                        <*> (AnnList <$> mapM trfName names)
  where transformDir InfixL = directionChar (pure AST.AssocLeft)
        transformDir InfixR = directionChar (pure AST.AssocRight)
        transformDir InfixN = annLoc (srcLocSpan . srcSpanEnd <$> tokenLoc AnnInfix) (pure AST.AssocNone)
        
        directionChar = annLoc ((\l -> mkSrcSpan (moveBackOneCol l) l) . srcSpanEnd <$> tokenLoc AnnInfix)
        moveBackOneCol (RealSrcLoc rl) = mkSrcLoc (srcLocFile rl) (srcLocLine rl) (srcLocCol rl - 1)
        moveBackOneCol (UnhelpfulLoc fs) = UnhelpfulLoc fs
        
trfPattern :: Located (Pat RdrName) -> Trf (Ann AST.Pattern RI)
trfPattern = trfLoc $ \case
  WildPat _ -> pure AST.WildPat
  VarPat name -> AST.VarPat <$> trfName' name
  LazyPat pat -> AST.IrrPat <$> trfPattern pat
  AsPat name pat -> AST.AsPat <$> trfName name <*> trfPattern pat
  ParPat pat -> AST.ParenPat <$> trfPattern pat
  BangPat pat -> AST.BangPat <$> trfPattern pat
  ListPat pats _ _ -> AST.ListPat . AnnList <$> mapM trfPattern pats
  TuplePat pats Boxed _ -> AST.TuplePat . AnnList <$> mapM trfPattern pats
  PArrPat pats _ -> AST.ParArrPat . AnnList <$> mapM trfPattern pats
  ConPatIn name _ -> AST.VarPat <$> trfName' (unLoc name)
  ViewPat expr pat _ -> AST.ViewPat <$> trfExpr expr <*> trfPattern pat
  SplicePat splice -> AST.SplicePat <$> trfSplice' splice
  QuasiQuotePat qq -> AST.QuasiQuotePat <$> trfQuasiQuotation' qq
  LitPat lit -> AST.LitPat <$> trfLiteral' lit
  NPat (ol_val . unLoc -> lit) _ _ -> AST.LitPat <$> trfOverloadedLit lit
  SigPatIn pat (hswb_cts -> typ) -> AST.TypeSigPat <$> trfPattern pat <*> trfType typ
  -- NPat, NPlusKPat, CoPat?
  
trfExpr :: Located (HsExpr RdrName) -> Trf (Ann AST.Expr RI)
trfExpr = trfLoc trfExpr'

trfExpr' :: HsExpr RdrName -> Trf (AST.Expr RI)
trfExpr' (HsVar name) = AST.Var <$> trfName' name
trfExpr' (HsIPVar (HsIPName ip)) = AST.Var . AST.nameFromList . fst <$> trfNameStr (unpackFS ip)
trfExpr' (HsOverLit (ol_val -> val)) = AST.Lit <$> trfOverloadedLit val
trfExpr' (HsLit val) = AST.Lit <$> trfLiteral' val
trfExpr' (HsLam (mg_alts -> [unLoc -> Match _ pats _ (GRHSs [unLoc -> GRHS [] expr] EmptyLocalBinds)]))
  = AST.Lambda <$> (AnnList <$> mapM trfPattern pats) <*> trfExpr expr
trfExpr' (HsLamCase _ (mg_alts -> matches)) = AST.LamCase . AnnList <$> mapM trfAlt matches
trfExpr' (HsApp e1 e2) = AST.App <$> trfExpr e1 <*> trfExpr e2
trfExpr' (OpApp e1 (L opLoc (HsVar op)) _ e2) 
  = AST.InfixApp <$> trfExpr e1 <*> annLoc (pure opLoc) (trfName' op) <*> trfExpr e2
trfExpr' (NegApp e _) = AST.PrefixApp <$> (annLoc (mkSrcSpan <$> (srcSpanStart <$> asks contRange) 
                                                             <*> (pure $ srcSpanStart (getLoc e))) 
                                                  (AST.nameFromList . fst <$> trfNameStr "-")) 
                                      <*> trfExpr e
trfExpr' (HsPar expr) = AST.Paren <$> trfExpr expr
trfExpr' (SectionL expr (L l (HsVar op))) = AST.LeftSection <$> trfExpr expr <*> annLoc (pure l) (trfName' op)
trfExpr' (SectionR (L l (HsVar op)) expr) = AST.RightSection <$> annLoc (pure l) (trfName' op) <*> trfExpr expr
trfExpr' (ExplicitTuple tupArgs box) | all tupArgPresent tupArgs 
  = wrap . AnnList <$> mapM (trfExpr . (\(Present e) -> e) . unLoc) tupArgs 
  where wrap = if box == Boxed then AST.Tuple else AST.UnboxedTuple
trfExpr' (ExplicitTuple tupArgs box)
  = wrap . AnnList <$> mapM (trfLoc $ (\case (Present e) -> AST.Present <$> trfExpr' (unLoc e)
                                             (Missing _) -> pure AST.Missing
                                       )) tupArgs 
  where wrap = if box == Boxed then AST.TupleSection else AST.UnboxedTupleSection
trfExpr' (HsCase expr (mg_alts -> cases)) = AST.Case <$> trfExpr expr <*> (AnnList <$> mapM trfAlt cases)
trfExpr' (HsIf _ expr thenE elseE) = AST.If <$> trfExpr expr <*> trfExpr thenE <*> trfExpr elseE
trfExpr' (HsMultiIf _ parts) = AST.MultiIf . AnnList <$> mapM trfGuardedRhs parts
trfExpr' (HsLet binds expr) = AST.Let <$> trfLocalBinds binds <*> trfExpr expr
trfExpr' (HsDo DoExpr stmts _) = AST.Do <$> (annLoc (tokenLoc AnnDo) (pure AST.DoKeyword)) 
                                        <*> (AnnList <$> mapM trfDoStmt stmts)
trfExpr' (HsDo MDoExpr stmts _) = AST.Do <$> (annLoc (tokenLoc AnnMdo) (pure AST.MDoKeyword)) 
                                         <*> (AnnList <$> mapM trfDoStmt stmts)
trfExpr' (HsDo ListComp stmts _)
  = AST.ListComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (HsDo MonadComp stmts _)
  = AST.ListComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (HsDo PArrComp stmts _)
  = AST.ParArrayComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (ExplicitList _ _ exprs) = AST.List . AnnList <$> mapM trfExpr exprs
trfExpr' (ExplicitPArr _ exprs) = AST.ParArray . AnnList <$> mapM trfExpr exprs
trfExpr' (RecordCon name _ fields) = AST.RecCon <$> trfName name <*> trfFieldUpdates fields
trfExpr' (RecordUpd expr fields _ _ _) = AST.RecUpdate <$> trfExpr expr <*> trfFieldUpdates fields
trfExpr' (ExprWithTySig expr typ _) = AST.TypeSig <$> trfExpr expr <*> trfType typ
trfExpr' (ArithSeq _ _ (From from)) = AST.Enum <$> trfExpr from <*> pure annNothing <*> pure annNothing
trfExpr' (ArithSeq _ _ (FromThen from step)) 
  = AST.Enum <$> trfExpr from <*> (annJust <$> trfExpr step) <*> pure annNothing
trfExpr' (ArithSeq _ _ (FromTo from to)) 
  = AST.Enum <$> trfExpr from <*> pure annNothing <*> (annJust <$> trfExpr to)
trfExpr' (ArithSeq _ _ (FromThenTo from step to)) 
  = AST.Enum <$> trfExpr from <*> (annJust <$> trfExpr step) <*> (annJust <$> trfExpr to)
trfExpr' (PArrSeq _ (FromTo from to)) 
  = AST.ParArrayEnum <$> trfExpr from <*> pure annNothing <*> trfExpr to
trfExpr' (PArrSeq _ (FromThenTo from step to)) 
  = AST.ParArrayEnum <$> trfExpr from <*> (annJust <$> trfExpr step) <*> trfExpr to
-- TODO: SCC, CORE, GENERATED annotations
trfExpr' (HsBracket brack) = AST.BracketExpr <$> trfBracket' brack
trfExpr' (HsSpliceE _ splice) = AST.Splice <$> trfSplice' splice
trfExpr' (HsQuasiQuoteE qq) = AST.QuasiQuoteExpr <$> trfQuasiQuotation' qq
-- TODO: arrows
-- TODO: static
  
trfAlt :: Located (Match RdrName (LHsExpr RdrName)) -> Trf (Ann AST.Alt RI)
trfAlt = trfLoc $ \(Match _ [pat] typ (GRHSs rhss locBinds))
  -> AST.Alt <$> trfPattern pat <*> trfRhss rhss <*> trfWhereLocalBinds locBinds
  
trfDoStmt :: Located (Stmt RdrName (LHsExpr RdrName)) -> Trf (Ann AST.Stmt RI)
trfDoStmt = trfLoc $ \case
  BindStmt pat expr _ _ -> AST.BindStmt <$> trfPattern pat <*> trfExpr expr
  BodyStmt expr _ _ _ -> AST.ExprStmt <$> trfExpr' (unLoc expr)
  LetStmt binds -> AST.LetStmt <$> trfLocalBinds binds
  RecStmt { recS_stmts = stmts } -> AST.RecStmt . AnnList <$> mapM trfDoStmt stmts

trfListCompStmts :: [Located (Stmt RdrName (LHsExpr RdrName))] -> Trf (AnnList AST.ListCompBody RI)
trfListCompStmts [unLoc -> ParStmt blocks _ _, unLoc -> (LastStmt {})]
  = AnnList <$> mapM (fmap ((\lcb -> Ann (collectAnnots $ _fromAnnList (AST._compStmts lcb)) lcb) 
                               . AST.ListCompBody . AnnList . concat) 
                       . mapM trfListCompStmt . (\(ParStmtBlock stmts _ _) -> stmts)) blocks
trfListCompStmts others 
  = AnnList . (:[]) <$> annLoc (collectAnnots <$> stmts) (AST.ListCompBody . AnnList <$> stmts) 
  where stmts = concat <$> mapM trfListCompStmt others

trfListCompStmt :: Located (Stmt RdrName (LHsExpr RdrName)) -> Trf [Ann AST.CompStmt RI]
trfListCompStmt (L l trst@(TransStmt { trS_stmts = stmts })) 
  = (++) <$> (concat <$> local (\s -> s { contRange = mkSrcSpan (srcSpanStart (contRange s)) (srcSpanEnd (getLoc (last stmts))) }) (mapM trfListCompStmt stmts)) 
         <*> ((:[]) <$> extractActualStmt trst)
-- last statement is extracted
trfListCompStmt (unLoc -> LastStmt _ _) = pure []
trfListCompStmt other = (:[]) <$> takeAnnot AST.CompStmt (trfDoStmt other)
  
extractActualStmt :: Stmt RdrName (LHsExpr RdrName) -> Trf (Ann AST.CompStmt RI)
extractActualStmt = \case
  TransStmt { trS_form = ThenForm, trS_using = using, trS_by = by } 
    -> addAnnotation by using (AST.ThenStmt <$> trfExpr using <*> trfMaybe trfExpr by)
  TransStmt { trS_form = GroupForm, trS_using = using, trS_by = by } 
    -> addAnnotation by using (AST.GroupStmt <$> (annJust <$> trfExpr using) <*> trfMaybe trfExpr by)
  where addAnnotation by using
          = annLoc (combineSrcSpans (getLoc using) . combineSrcSpans (maybe noSrcSpan getLoc by)
                      <$> tokenLocBack AnnThen)
  
getLastStmt :: [Located (Stmt RdrName (LHsExpr RdrName))] -> Located (HsExpr RdrName)
getLastStmt (L _ (LastStmt body _) : rest) = body
getLastStmt (_ : rest) = getLastStmt rest
  
trfFieldUpdates :: HsRecordBinds RdrName -> Trf (AnnList AST.FieldUpdate RI)
trfFieldUpdates (HsRecFields fields dotdot) 
  = AnnList 
      <$> ((++) <$> mapM trfFieldUpdate fields 
                <*> (if isJust dotdot then (:[]) <$> annLoc (tokenLoc AnnDotdot) (pure AST.FieldWildcard) 
                                      else pure []) )
  
trfFieldUpdate :: Located (HsRecField RdrName (LHsExpr RdrName)) -> Trf (Ann AST.FieldUpdate RI)
trfFieldUpdate = trfLoc $ \case
  HsRecField id _ True -> AST.FieldPun <$> trfName' (unLoc id)
  HsRecField id val False -> AST.NormalFieldUpdate <$> trfName id <*> trfExpr val
  
trfKindSig :: Maybe (LHsKind RdrName) -> Trf (AnnMaybe AST.KindConstraint RI)
trfKindSig = trfMaybe (\k -> annLoc (combineSrcSpans (getLoc k) <$> (tokenLoc AnnDcolon)) 
                                    (fmap AST.KindConstraint $ trfLoc trfKind' k))

trfKind :: Located (HsKind RdrName) -> Trf (Ann AST.Kind RI)
trfKind = trfLoc trfKind'

trfKind' :: HsKind RdrName -> Trf (AST.Kind RI)
trfKind' (HsTyVar (Exact n)) 
  | isWiredInName n && occNameString (nameOccName n) == "*"
  = pure AST.KindStar
  | isWiredInName n && occNameString (nameOccName n) == "#"
  = pure AST.KindUnbox
trfKind' (HsParTy kind) = AST.KindParen <$> trfKind kind
trfKind' (HsFunTy k1 k2) = AST.KindFn <$> trfKind k1 <*> trfKind k2
trfKind' (HsAppTy k1 k2) = AST.KindApp <$> trfKind k1 <*> trfKind k2
trfKind' (HsTyVar kv) = AST.KindVar <$> trfName' kv
trfKind' (HsExplicitTupleTy _ kinds) = AST.KindTuple . AnnList <$> mapM trfKind kinds
trfKind' (HsExplicitListTy _ kinds) = AST.KindList . AnnList <$> mapM trfKind kinds
  
trfTypeEqs :: [Located (TyFamInstEqn RdrName)] -> Trf (AnnList AST.TypeEqn RI)
trfTypeEqs = fmap AnnList . mapM trfTypeEq

trfTypeEq :: Located (TyFamInstEqn RdrName) -> Trf (Ann AST.TypeEqn RI)
trfTypeEq = trfLoc $ \(TyFamEqn name pats rhs) 
  -> AST.TypeEqn <$> combineTypes name pats <*> trfType rhs
  where combineTypes :: Located RdrName -> HsTyPats RdrName -> Trf (Ann AST.Type RI)
        combineTypes name pats 
          = foldl (\t p -> do typ <- t
                              annLoc (pure $ combineSrcSpans (_annotation typ) (getLoc p)) 
                                     (AST.TyApp <$> pure typ <*> trfType p)) 
                  (annLoc (pure $ getLoc name) (AST.TyCon <$> trfName' (unLoc name))) 
                  (hswb_cts pats)
                 
  
trfType :: Located (HsType RdrName) -> Trf (Ann AST.Type RI)
trfType = trfLoc trfType'

trfType' :: HsType RdrName -> Trf (AST.Type RI)
trfType' (HsForAllTy _ _ bndrs ctx typ) = AST.TyForall <$> trfBindings (hsq_tvs bndrs) 
                                                       <*> trfCtx ctx <*> trfType typ
trfType' (HsTyVar name) | isRdrTc name = AST.TyCon <$> trfName' name
trfType' (HsTyVar name) | isRdrTyVar name = AST.TyVar <$> trfName' name
trfType' (HsAppTy t1 t2) = AST.TyApp <$> trfType t1 <*> trfType t2
trfType' (HsFunTy t1 t2) = AST.TyFun <$> trfType t1 <*> trfType t2
trfType' (HsListTy typ) = AST.TyList <$> trfType typ
trfType' (HsPArrTy typ) = AST.TyParArray <$> trfType typ
-- HsBoxedOrConstraintTuple?
trfType' (HsTupleTy HsBoxedTuple typs) = AST.TyTuple . AnnList <$> mapM trfType typs
trfType' (HsTupleTy HsUnboxedTuple typs) = AST.TyUnbTuple . AnnList <$> mapM trfType typs
trfType' (HsOpTy t1 op t2) = AST.TyInfix <$> trfType t1 <*> trfName (snd op) <*> trfType t2
trfType' (HsParTy typ) = AST.TyParen <$> trfType typ
trfType' (HsKindSig typ kind) = AST.TyKinded <$> trfType typ <*> trfKind kind
trfType' (HsQuasiQuoteTy qq) = AST.TyQuasiQuote <$> trfQuasiQuotation' qq
trfType' (HsSpliceTy splice _) = AST.TySplice <$> trfSplice' splice
trfType' (HsBangTy _ typ) = AST.TyBang <$> trfType typ
-- HsRecTy
-- HsCoreTy
trfType' (HsTyLit (HsNumTy _ int)) = pure $ AST.TyNumLit int
trfType' (HsTyLit (HsStrTy _ str)) = pure $ AST.TyStrLit (unpackFS str)
trfType' (HsWrapTy _ typ) = trfType' typ
trfType' HsWildcardTy = pure AST.TyWildcard
-- not implemented as ghc 7.10.3
trfType' (HsNamedWildcardTy name) = AST.TyNamedWildcard <$> trfName' name


  
trfBindings :: [Located (HsTyVarBndr RdrName)] -> Trf (AnnList AST.TyVar RI)
trfBindings vars = AnnList <$> mapM trfTyVar vars
  
trfCtx :: Located (HsContext RdrName) -> Trf (AnnMaybe AST.Context RI)
trfCtx (L l []) = pure annNothing
trfCtx (L l [L _ (HsParTy t)]) 
  = annJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                       (AST.ContextMulti . AnnList . (:[]) <$> trfAssertion t)
trfCtx (L l [t]) 
  = annJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                       (AST.ContextOne . _element <$> trfAssertion t)
trfCtx (L l ctx) = annJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                                      (AST.ContextMulti . AnnList <$> mapM trfAssertion ctx) 
  
  
trfAssertion :: LHsType RdrName -> Trf (Ann AST.Assertion RI)
trfAssertion t = annLoc (pure $ getLoc t) $ case base of 
  L l (HsTyVar name) -> AST.ClassAssert <$> annLoc (pure l) (trfName' name) 
                                        <*> (AnnList <$> mapM trfType args)
  L l (HsOpTy left op right) -> AST.InfixAssert <$> trfType left <*> trfName (snd op) <*> trfType right
  where (args, base) = getArgs t
        getArgs :: LHsType RdrName -> ([LHsType RdrName], LHsType RdrName)
        getArgs (L l (HsAppTy ft at)) = case getArgs ft of (args, base) -> (args++[at], base)
        getArgs t = ([], t)
  
trfFunDeps :: [Located (FunDep (Located name))] -> Trf (AnnMaybe AST.FunDeps RI)
trfFunDeps [] = pure annNothing
trfFunDeps _ = pure undefined
  
createDeclHead :: Located RdrName -> LHsTyVarBndrs RdrName -> Trf (Ann AST.DeclHead RI)
createDeclHead name vars
  = foldl (\t p -> do typ <- t
                      annLoc (pure $ combineSrcSpans (_annotation typ) (getLoc p)) 
                             (AST.DHApp typ <$> trfTyVar p)) 
          (annLoc (pure $ getLoc name) (AST.DeclHead <$> trfName' (unLoc name))) 
          (hsq_tvs vars)
         
trfDataKeyword :: NewOrData -> Trf (Ann AST.DataOrNewtypeKeyword RI)
trfDataKeyword NewType = annLoc (tokenLoc AnnNewtype) (pure AST.NewtypeKeyword)
trfDataKeyword DataType = annLoc (tokenLoc AnnData) (pure AST.DataKeyword)
         
createClassBody :: [LSig RdrName] -> LHsBinds RdrName -> [LFamilyDecl RdrName] 
                               -> [LTyFamDefltEqn RdrName] -> Trf (AnnMaybe AST.ClassBody RI)
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
        getBinds = mapM (takeAnnot AST.ClsDef . trfBind) (bagToList binds)
        getFams = mapM (takeAnnot AST.ClsTypeFam . trfTypeFam) typeFams
        getFamDefs = mapM trfTypeFamDef typeFamDefs
       
trfClassElemSig :: Located (Sig RdrName) -> Trf (Ann AST.ClassElement RI)
trfClassElemSig = trfLoc $ \case
  TypeSig [name] typ _ -> AST.ClsSig <$> (AST.TypeSignature <$> trfName name <*> trfType typ)
  GenericSig [name] typ -> AST.ClsDefSig <$> trfName name <*> trfType typ
         
trfTyVar :: Located (HsTyVarBndr RdrName) -> Trf (Ann AST.TyVar RI)
trfTyVar var@(L l _) = trfLoc (\case
  UserTyVar name -> AST.TyVarDecl <$> annLoc (pure l) (trfName' name) <*> pure annNothing
  KindedTyVar name kind -> AST.TyVarDecl <$> trfName name <*> trfKindSig (Just kind)) var
          
trfTypeFam :: Located (FamilyDecl RdrName) -> Trf (Ann AST.TypeFamily RI)
trfTypeFam = trfLoc $ \case
  FamilyDecl DataFamily name tyVars kindSig
    -> AST.DataFamily <$> createDeclHead name tyVars <*> trfKindSig kindSig
  FamilyDecl OpenTypeFamily name tyVars kindSig
    -> AST.TypeFamily <$> createDeclHead name tyVars <*> trfKindSig kindSig
          
trfTypeFamDef :: Located (TyFamDefltEqn RdrName) -> Trf (Ann AST.ClassElement RI)
trfTypeFamDef = trfLoc $ \(TyFamEqn con pats rhs) 
  -> AST.ClsTypeDef <$> createDeclHead con pats <*> trfType rhs
          
trfOverlap :: Located OverlapMode -> Trf (Ann AST.OverlapPragma RI)
trfOverlap = trfLoc $ pure . \case
  NoOverlap _ -> AST.DisableOverlap
  Overlappable _ -> AST.Overlappable
  Overlapping _ -> AST.Overlapping
  Overlaps _ -> AST.Overlaps
  Incoherent _ -> AST.IncoherentOverlap
          
trfInstBody :: LHsBinds RdrName -> [LSig RdrName] -> [LTyFamInstDecl RdrName] -> [LDataFamInstDecl RdrName] -> Trf (AnnMaybe AST.InstBody RI)
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
        getBinds = mapM (takeAnnot AST.InstBodyNormalDecl . trfBind) (bagToList binds)
        getFams = mapM trfInstTypeFam fams
        getDats = mapM trfInstDataFam dats
          
trfClassInstSig :: Located (Sig RdrName) -> Trf (Ann AST.InstBodyDecl RI)
trfClassInstSig = trfLoc $ \case
  TypeSig [name] typ _ -> AST.InstBodyTypeSig <$> (AST.TypeSignature <$> trfName name <*> trfType typ)
          
trfInstTypeFam :: Located (TyFamInstDecl RdrName) -> Trf (Ann AST.InstBodyDecl RI)
trfInstTypeFam (unLoc -> TyFamInstDecl eqn _) = takeAnnot AST.InstBodyTypeDecl (trfTypeEq eqn)

trfInstDataFam :: Located (DataFamInstDecl RdrName) -> Trf (Ann AST.InstBodyDecl RI)
trfInstDataFam = trfLoc $ \case 
  (DataFamInstDecl tc (hswb_cts -> pats) (HsDataDefn dn ctx _ _ cons derivs) _) 
    -> AST.InstBodyDataDecl <$> trfDataKeyword dn 
         <*> annLoc (pure $ collectLocs pats `combineSrcSpans` getLoc tc `combineSrcSpans` getLoc ctx)
                    (AST.InstanceRule annNothing <$> trfCtx ctx 
                                                 <*> foldr (\t r -> annLoc (combineSrcSpans (getLoc t) . _annotation <$> r) 
                                                                           (AST.InstanceHeadApp <$> r <*> (trfType t))) 
                                                           (takeAnnot AST.InstanceHeadCon (trfName tc)) pats)
         <*> (AnnList <$> mapM trfConDecl cons)
         <*> trfMaybe trfDerivings derivs
          
          
trfCallConv :: Located CCallConv -> Trf (Ann AST.CallConv RI)
trfCallConv = undefined      
   
trfCallConv' :: CCallConv -> Trf (AST.CallConv RI)
trfCallConv' = undefined 

trfSafety :: Located Safety -> Trf (AnnMaybe AST.Safety RI)
trfSafety = undefined 
          
trfQuasiQuotation' :: HsQuasiQuote RdrName -> Trf (AST.QuasiQuote RI)
trfQuasiQuotation' = undefined

trfSplice' :: HsSplice RdrName -> Trf (AST.Splice RI)
trfSplice' = undefined

trfBracket' :: HsBracket RdrName -> Trf (AST.Bracket RI)
trfBracket' = undefined
  