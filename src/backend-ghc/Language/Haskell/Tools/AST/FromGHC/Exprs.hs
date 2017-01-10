{-# LANGUAGE LambdaCase
           , ViewPatterns
           , ScopedTypeVariables
           , TypeApplications
           , AllowAmbiguousTypes
           #-}
-- | Functions that convert the expression-related elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.AST.FromGHC.Exprs where

import Data.Maybe
import Data.List (partition, find)
import Data.Data (toConstr)
import Control.Monad.Reader

import GHC
import SrcLoc as GHC
import BasicTypes as GHC
import Outputable as GHC
import PrelNames as GHC

import Language.Haskell.Tools.AST.FromGHC.Names
import Language.Haskell.Tools.AST.FromGHC.Types
import Language.Haskell.Tools.AST.FromGHC.Literals
import Language.Haskell.Tools.AST.FromGHC.Patterns
import Language.Haskell.Tools.AST.FromGHC.Stmts
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.Binds
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.TH
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Language.Haskell.Tools.AST.SemaInfoTypes

import Language.Haskell.Tools.AST (Ann(..), AnnListG(..), Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfExpr :: forall n r . TransformName n r => Located (HsExpr n) -> Trf (Ann AST.UExpr (Dom r) RangeStage)
-- correction for empty cases
trfExpr (L l cs@(HsCase expr (unLoc . mg_alts -> []))) 
  = do let realSpan = combineSrcSpans l (getLoc expr)
       tokensAfter <- allTokensAfter (srcSpanEnd realSpan)
       let actualSpan = case take 3 tokensAfter of 
                          [(_, AnnOf), (_, AnnOpenC), (endSpan, AnnCloseC)] -> realSpan `combineSrcSpans` endSpan
                          ((endSpan, AnnOf) : _) -> realSpan `combineSrcSpans` endSpan
                          _ -> error "trfExpr: case without 'of' '{' or '}' token"
       annLoc createScopeInfo (pure actualSpan) (trfExpr' cs)
trfExpr e = do exprSpls <- asks exprSplices
               let RealSrcSpan loce = getLoc e
                   contSplice = find (\sp -> case getSpliceLoc sp of (RealSrcSpan spLoc) -> spLoc `containsSpan` loce; _ -> False) exprSpls
               case contSplice of Just sp -> case sp of HsQuasiQuote {} -> exprSpliceInserted sp (annCont createScopeInfo (AST.UQuasiQuoteExpr <$> annLocNoSema (pure $ getSpliceLoc sp) (trfQuasiQuotation' sp)))
                                                        _               -> exprSpliceInserted sp (annCont createScopeInfo (AST.USplice <$> annLocNoSema (pure $ getSpliceLoc sp) (trfSplice' sp)))
                                  Nothing -> trfLoc trfExpr' createScopeInfo e

createScopeInfo :: Trf ScopeInfo
createScopeInfo = do scope <- asks localsInScope
                     return (mkScopeInfo scope)

trfExpr' :: TransformName n r => HsExpr n -> Trf (AST.UExpr (Dom r) RangeStage)
trfExpr' (HsVar name) = AST.UVar <$> trfName name
trfExpr' (HsRecFld fld) = AST.UVar <$> (asks contRange >>= \l -> trfAmbiguousFieldName' l fld)
trfExpr' (HsIPVar ip) = AST.UVar <$> trfImplicitName ip
trfExpr' (HsOverLit (ol_val -> val)) = AST.ULit <$> annContNoSema (trfOverloadedLit val)
trfExpr' (HsLit val) = AST.ULit <$> annContNoSema (trfLiteral' val)
trfExpr' (HsLam (unLoc . mg_alts -> [unLoc -> Match _ pats _ (GRHSs [unLoc -> GRHS [] expr] (unLoc -> EmptyLocalBinds))]))
  = AST.ULambda <$> (trfAnnList " " trfPattern' pats) <*> addToScope pats (trfExpr expr)
trfExpr' (HsLamCase _ (unLoc . mg_alts -> matches)) = AST.ULamCase <$> trfAnnList " " trfAlt' matches
trfExpr' (HsApp e1 e2) = AST.UApp <$> trfExpr e1 <*> trfExpr e2
trfExpr' (OpApp e1 (unLoc -> HsVar op) _ e2) 
  = AST.UInfixApp <$> trfExpr e1 <*> trfOperator op <*> trfExpr e2
trfExpr' (NegApp e _) = AST.UPrefixApp <$> annLocNoSema loc (AST.UNormalOp <$> annLoc info loc (AST.nameFromList <$> trfNameStr "-"))
                                       <*> trfExpr e
  where loc = mkSrcSpan <$> atTheStart <*> (pure $ srcSpanStart (getLoc e))
        info = createNameInfo =<< (fromMaybe (error "minus operation is not found") <$> liftGhc negateOpName)
        negateOpName = getFromNameUsing (\n -> (\case Just (AnId id) -> Just id; _ -> Nothing) <$> lookupName n) negateName
trfExpr' (HsPar (unLoc -> SectionL expr (unLoc -> HsVar op))) = AST.ULeftSection <$> trfExpr expr <*> trfOperator op
trfExpr' (HsPar (unLoc -> SectionR (unLoc -> HsVar op) expr)) = AST.URightSection <$> trfOperator op <*> trfExpr expr
trfExpr' (HsPar expr) = AST.UParen <$> trfExpr expr
trfExpr' (ExplicitTuple tupArgs box) | all tupArgPresent tupArgs 
  = wrap <$> between AnnOpenP AnnCloseP (trfAnnList' ", " (trfExpr . (\(Present e) -> e) . unLoc) tupArgs)
  where wrap = if box == Boxed then AST.UTuple else AST.UUnboxedTuple
trfExpr' (ExplicitTuple tupArgs box)
  = wrap <$> between AnnOpenP AnnCloseP
               (do locs <- elemLocs
                   makeList ", " atTheEnd $ mapM trfTupSecElem (zip (map unLoc tupArgs) locs))
  where wrap = if box == Boxed then AST.UTupleSection else AST.UUnboxedTupSec
        trfTupSecElem :: forall n r . TransformName n r => (HsTupArg n, SrcSpan) -> Trf (Ann AST.UTupSecElem (Dom r) RangeStage)
        trfTupSecElem (Present e, l) 
          = annLocNoSema (pure l) (AST.Present <$> (annCont createScopeInfo (trfExpr' (unLoc e))))
        trfTupSecElem (Missing _, l) = annLocNoSema (pure l) (pure AST.Missing)
        
        elemLocs :: Trf [SrcSpan]
        elemLocs = do r <- asks contRange
                      commaLocs <- allTokenLoc AnnComma
                      return $ foldl breakUp [r] commaLocs
        breakUp :: [SrcSpan] -> SrcSpan -> [SrcSpan]
        breakUp cont sep = concatMap (breakUpOne sep) cont

        breakUpOne :: SrcSpan -> SrcSpan -> [SrcSpan]
        breakUpOne sep@(RealSrcSpan realSep) sp@(RealSrcSpan realSp) 
          | realSp `containsSpan` realSep = [mkSrcSpan (srcSpanStart sp) (srcSpanStart sep), mkSrcSpan (srcSpanEnd sep) (srcSpanEnd sp)]
        breakUpOne _ sp = [sp]

trfExpr' (HsCase expr (unLoc . mg_alts -> cases)) = AST.UCase <$> trfExpr expr <*> (makeIndentedList (focusBeforeIfPresent AnnCloseC atTheEnd) (mapM trfAlt cases))
trfExpr' (HsIf _ expr thenE elseE) = AST.UIf <$> trfExpr expr <*> trfExpr thenE <*> trfExpr elseE
trfExpr' (HsMultiIf _ parts) = AST.UMultiIf <$> trfAnnList "" trfGuardedCaseRhs' parts
trfExpr' (HsLet (unLoc -> binds) expr) = addToScope binds (AST.ULet <$> trfLocalBinds binds <*> trfExpr expr)
trfExpr' (HsDo DoExpr (unLoc -> stmts) _) = AST.UDo <$> annLocNoSema (tokenLoc AnnDo) (pure AST.UDoKeyword) 
                                                    <*> makeNonemptyIndentedList (trfScopedSequence trfDoStmt stmts)
trfExpr' (HsDo MDoExpr (unLoc -> [unLoc -> RecStmt { recS_stmts = stmts }, lastStmt]) _) 
  = AST.UDo <$> annLocNoSema (tokenLoc AnnMdo) (pure AST.UMDoKeyword)
            <*> addToScope stmts (makeNonemptyIndentedList (mapM trfDoStmt (stmts ++ [lastStmt])))
trfExpr' (HsDo MDoExpr (unLoc -> stmts) _) = AST.UDo <$> annLocNoSema (tokenLoc AnnMdo) (pure AST.UMDoKeyword)
                                                     <*> addToScope stmts (makeNonemptyIndentedList (mapM trfDoStmt stmts))
-- TODO: scoping
trfExpr' (HsDo ListComp (unLoc -> stmts) _)
  = AST.UListComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (HsDo MonadComp (unLoc -> stmts) _)
  = AST.UListComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (HsDo PArrComp (unLoc -> stmts) _)
  = AST.UParArrayComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (ExplicitList _ _ exprs) = AST.UList <$> trfAnnList' ", " trfExpr exprs
trfExpr' (ExplicitPArr _ exprs) = AST.UParArray <$> trfAnnList' ", " trfExpr exprs
trfExpr' (RecordCon name _ _ fields) = AST.URecCon <$> trfName name <*> trfFieldInits fields
trfExpr' (RecordUpd expr fields _ _ _ _) = AST.URecUpdate <$> trfExpr expr <*> trfAnnList ", " trfFieldUpdate fields
trfExpr' (ExprWithTySig expr typ) = AST.UTypeSig <$> trfExpr expr <*> trfType (hswc_body $ hsib_body typ)
trfExpr' (ArithSeq _ _ (From from)) = AST.UEnum <$> trfExpr from <*> nothing "," "" (before AnnDotdot)
                                                                <*> nothing "" "" (before AnnCloseS)
trfExpr' (ArithSeq _ _ (FromThen from step)) 
  = AST.UEnum <$> trfExpr from <*> (makeJust <$> trfExpr step) <*> nothing "" "" (before AnnCloseS) 
trfExpr' (ArithSeq _ _ (FromTo from to)) 
  = AST.UEnum <$> trfExpr from <*> nothing "," "" (before AnnDotdot)
                               <*> (makeJust <$> trfExpr to)
trfExpr' (ArithSeq _ _ (FromThenTo from step to)) 
  = AST.UEnum <$> trfExpr from <*> (makeJust <$> trfExpr step) <*> (makeJust <$> trfExpr to)
trfExpr' (PArrSeq _ (FromTo from to)) 
  = AST.UParArrayEnum <$> trfExpr from <*> nothing "," "" (before AnnDotdot) <*> trfExpr to
trfExpr' (PArrSeq _ (FromThenTo from step to)) 
  = AST.UParArrayEnum <$> trfExpr from <*> (makeJust <$> trfExpr step) <*> trfExpr to
-- TODO: SCC, CORE, GENERATED annotations
trfExpr' (HsBracket brack) = AST.UBracketExpr <$> annContNoSema (trfBracket' brack)
trfExpr' (HsSpliceE qq@(HsQuasiQuote {})) = AST.UQuasiQuoteExpr <$> annContNoSema (trfQuasiQuotation' qq)
trfExpr' (HsSpliceE splice) = AST.USplice <$> annContNoSema (trfSplice' splice)
trfExpr' (HsRnBracketOut br _) = AST.UBracketExpr <$> annContNoSema (trfBracket' br)
trfExpr' (HsProc pat cmdTop) = AST.UProc <$> trfPattern pat <*> trfCmdTop cmdTop
trfExpr' (HsStatic expr) = AST.UStaticPtr <$> trfExpr expr
trfExpr' (HsAppType expr typ) = AST.UExplTypeApp <$> trfExpr expr <*> trfType (hswc_body typ)
trfExpr' t = error ("Illegal expression: " ++ showSDocUnsafe (ppr t) ++ " (ctor: " ++ show (toConstr t) ++ ")")
  
trfFieldInits :: TransformName n r => HsRecFields n (LHsExpr n) -> Trf (AnnListG AST.UFieldUpdate (Dom r) RangeStage)
trfFieldInits (HsRecFields fields dotdot)
  = do cont <- asks contRange
       let (normalFlds, implicitFlds) = partition ((cont /=) . getLoc) fields
       makeList ", " (before AnnCloseC)
         $ ((++) <$> mapM trfFieldInit normalFlds
                  <*> (if isJust dotdot then (:[]) <$> annLocNoSema (tokenLoc AnnDotdot) 
                                                                    (AST.UFieldWildcard <$> (annCont (createImplicitFldInfo (unLoc . (\(HsVar n) -> n) . unLoc) (map unLoc implicitFlds)) (pure AST.FldWildcard))) 
                                        else pure []))
  
trfFieldInit :: TransformName n r => Located (HsRecField n (LHsExpr n)) -> Trf (Ann AST.UFieldUpdate (Dom r) RangeStage)
trfFieldInit = trfLocNoSema $ \case
  HsRecField id _ True -> AST.UFieldPun <$> trfName (getFieldOccName id)
  HsRecField id val False -> AST.UNormalFieldUpdate <$> trfName (getFieldOccName id) <*> trfExpr val
  
trfFieldUpdate :: TransformName n r => HsRecField' (AmbiguousFieldOcc n) (LHsExpr n) -> Trf (AST.UFieldUpdate (Dom r) RangeStage)
trfFieldUpdate (HsRecField id _ True) = AST.UFieldPun <$> trfAmbiguousFieldName id
trfFieldUpdate (HsRecField id val False) = AST.UNormalFieldUpdate <$> trfAmbiguousFieldName id <*> trfExpr val

trfAlt :: TransformName n r => Located (Match n (LHsExpr n)) -> Trf (Ann AST.UAlt (Dom r) RangeStage)
trfAlt = trfLocNoSema trfAlt'

trfAlt' :: TransformName n r => Match n (LHsExpr n) -> Trf (AST.UAlt (Dom r) RangeStage)
trfAlt' = gTrfAlt' trfExpr

gTrfAlt' :: TransformName n r => (Located (ge n) -> Trf (Ann ae (Dom r) RangeStage)) -> Match n (Located (ge n)) -> Trf (AST.UAlt' ae (Dom r) RangeStage)
gTrfAlt' te (Match _ [pat] _ (GRHSs rhss (unLoc -> locBinds)))
  = AST.UAlt <$> trfPattern pat <*> gTrfCaseRhss te rhss <*> trfWhereLocalBinds locBinds
gTrfAlt' _ _ = error "gTrfAlt': not exactly one alternative when transforming a case alternative"
  
trfCaseRhss :: TransformName n r => [Located (GRHS n (LHsExpr n))] -> Trf (Ann AST.UCaseRhs (Dom r) RangeStage)
trfCaseRhss = gTrfCaseRhss trfExpr

gTrfCaseRhss :: TransformName n r => (Located (ge n) -> Trf (Ann ae (Dom r) RangeStage)) -> [Located (GRHS n (Located (ge n)))] -> Trf (Ann (AST.UCaseRhs' ae) (Dom r) RangeStage)
gTrfCaseRhss te [unLoc -> GRHS [] body] = annLocNoSema (combineSrcSpans (getLoc body) <$> updateFocus (pure . updateEnd (const $ srcSpanStart $ getLoc body)) 
                                                                                                      (tokenLocBack AnnRarrow)) 
                                                 (AST.UUnguardedCaseRhs <$> te body)
gTrfCaseRhss te rhss = annLocNoSema (pure $ collectLocs rhss) 
                              (AST.UGuardedCaseRhss <$> trfAnnList ";" (gTrfGuardedCaseRhs' te) rhss)
  
trfGuardedCaseRhs :: TransformName n r => Located (GRHS n (LHsExpr n)) -> Trf (Ann AST.UGuardedCaseRhs (Dom r) RangeStage)
trfGuardedCaseRhs = trfLocNoSema trfGuardedCaseRhs' 

trfGuardedCaseRhs' :: TransformName n r => GRHS n (LHsExpr n) -> Trf (AST.UGuardedCaseRhs (Dom r) RangeStage)
trfGuardedCaseRhs' = gTrfGuardedCaseRhs' trfExpr

gTrfGuardedCaseRhs' :: TransformName n r => (Located (ge n) -> Trf (Ann ae (Dom r) RangeStage)) -> GRHS n (Located (ge n)) -> Trf (AST.UGuardedCaseRhs' ae (Dom r) RangeStage)
gTrfGuardedCaseRhs' te (GRHS guards body) = AST.UGuardedCaseRhs <$> trfAnnList " " trfRhsGuard' guards <*> te body

trfCmdTop :: TransformName n r => Located (HsCmdTop n) -> Trf (Ann AST.UCmd (Dom r) RangeStage)
trfCmdTop (L _ (HsCmdTop cmd _ _ _)) = trfCmd cmd

trfCmd :: TransformName n r => Located (HsCmd n) -> Trf (Ann AST.UCmd (Dom r) RangeStage)
trfCmd = trfLocNoSema trfCmd'

trfCmd' :: TransformName n r => HsCmd n -> Trf (AST.UCmd (Dom r) RangeStage)
trfCmd' (HsCmdArrApp left right _ typ dir) = AST.UArrowAppCmd <$> trfExpr left <*> op <*> trfExpr right 
  where op = case (typ, dir) of (HsFirstOrderApp, False) -> annLocNoSema (tokenLoc Annrarrowtail) (pure AST.URightAppl)
                                (HsFirstOrderApp, True) -> annLocNoSema (tokenLoc Annlarrowtail) (pure AST.ULeftAppl)
                                (HsHigherOrderApp, False) -> annLocNoSema (tokenLoc AnnRarrowtail) (pure AST.URightHighApp)
                                (HsHigherOrderApp, True) -> annLocNoSema (tokenLoc AnnLarrowtail) (pure AST.ULeftHighApp)
                                                                       -- FIXME: needs a before 
trfCmd' (HsCmdArrForm expr _ cmds) = AST.UArrowFormCmd <$> trfExpr expr <*> makeList " " (before AnnClose) (mapM trfCmdTop cmds)
trfCmd' (HsCmdApp cmd expr) = AST.UAppCmd <$> trfCmd cmd <*> trfExpr expr
trfCmd' (HsCmdLam (MG (unLoc -> [unLoc -> Match _ pats _ (GRHSs [unLoc -> GRHS [] body] _)]) _ _ _)) 
  = AST.ULambdaCmd <$> trfAnnList " " trfPattern' pats <*> trfCmd body
trfCmd' (HsCmdPar cmd) = AST.UParenCmd <$> trfCmd cmd
trfCmd' (HsCmdCase expr (MG (unLoc -> alts) _ _ _)) 
  = AST.UCaseCmd <$> trfExpr expr <*> makeNonemptyIndentedList (mapM (trfLocNoSema (gTrfAlt' trfCmd)) alts) 
trfCmd' (HsCmdIf _ pred thenExpr elseExpr) = AST.UIfCmd <$> trfExpr pred <*> trfCmd thenExpr <*> trfCmd elseExpr
trfCmd' (HsCmdLet (unLoc -> binds) cmd) = addToScope binds (AST.ULetCmd <$> trfLocalBinds binds <*> trfCmd cmd)
trfCmd' (HsCmdDo (unLoc -> stmts) _) = AST.UDoCmd <$> makeNonemptyIndentedList (mapM (trfLocNoSema (gTrfDoStmt' trfCmd)) stmts)
-- | TODO: implement
trfCmd' (HsCmdLam {}) = error "trfCmd': cmd lambda not supported yet"
trfCmd' (HsCmdWrap {}) = error "trfCmd': cmd wrap not supported yet"