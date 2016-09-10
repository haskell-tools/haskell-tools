{-# LANGUAGE LambdaCase
           , ViewPatterns
           , ScopedTypeVariables
           , TypeApplications
           , AllowAmbiguousTypes
           #-}
module Language.Haskell.Tools.AST.FromGHC.Exprs where

import Data.Maybe
import Data.List (partition, find)
import Data.Data (toConstr)
import Control.Monad.Reader
import Control.Reference

import GHC
import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import HsPat as GHC
import HsExpr as GHC
import HsBinds as GHC
import HsLit as GHC
import BasicTypes as GHC
import ApiAnnotation as GHC
import FastString as GHC
import Outputable as GHC
import PrelNames as GHC
import DataCon as GHC

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Types
import Language.Haskell.Tools.AST.FromGHC.Literals
import Language.Haskell.Tools.AST.FromGHC.Patterns
import Language.Haskell.Tools.AST.FromGHC.Stmts
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.Binds
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.TH
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.GHCUtils

import Language.Haskell.Tools.AST (Ann(..), AnnList(..), Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

import Debug.Trace

trfExpr :: forall n r . TransformName n r => Located (HsExpr n) -> Trf (Ann AST.Expr (Dom r) RangeStage)
-- correction for empty cases
trfExpr (L l cs@(HsCase expr (unLoc . mg_alts -> []))) 
  = do let realSpan = combineSrcSpans l (getLoc expr)
       tokensAfter <- allTokensAfter (srcSpanEnd realSpan)
       let actualSpan = case take 3 tokensAfter of 
                          [(_, AnnOf), (_, AnnOpenC), (endSpan, AnnCloseC)] -> realSpan `combineSrcSpans` endSpan
                          ((endSpan, AnnOf) : _) -> realSpan `combineSrcSpans` endSpan
       annLoc createScopeInfo (pure actualSpan) (trfExpr' cs)
trfExpr e = do exprSpls <- asks exprSplices
               let RealSrcSpan loce = getLoc e
                   contSplice = find (\sp -> case getSpliceLoc sp of (RealSrcSpan spLoc) -> spLoc `containsSpan` loce; _ -> False) exprSpls
               case contSplice of Just sp -> case sp of HsQuasiQuote {} -> exprSpliceInserted sp (annCont createScopeInfo (AST.QuasiQuoteExpr <$> annLocNoSema (pure $ getSpliceLoc sp) (trfQuasiQuotation' sp)))
                                                        _               -> exprSpliceInserted sp (annCont createScopeInfo (AST.Splice <$> annLocNoSema (pure $ getSpliceLoc sp) (trfSplice' sp)))
                                  Nothing -> trfLoc trfExpr' createScopeInfo e

createScopeInfo :: Trf AST.ScopeInfo
createScopeInfo = do scope <- asks localsInScope
                     return (AST.ScopeInfo scope)

trfExpr' :: TransformName n r => HsExpr n -> Trf (AST.Expr (Dom r) RangeStage)
trfExpr' (HsVar name) = AST.Var <$> trfName name
trfExpr' (HsRecFld fld) = AST.Var <$> (asks contRange >>= \l -> trfAmbiguousFieldName' l fld)
trfExpr' (HsIPVar ip) = AST.Var <$> trfImplicitName ip
trfExpr' (HsOverLit (ol_val -> val)) = AST.Lit <$> annContNoSema (trfOverloadedLit val)
trfExpr' (HsLit val) = AST.Lit <$> annContNoSema (trfLiteral' val)
trfExpr' (HsLam (unLoc . mg_alts -> [unLoc -> Match _ pats _ (GRHSs [unLoc -> GRHS [] expr] (unLoc -> EmptyLocalBinds))]))
  = AST.Lambda <$> (trfAnnList " " trfPattern' pats) <*> addToScope pats (trfExpr expr)
trfExpr' (HsLamCase _ (unLoc . mg_alts -> matches)) = AST.LamCase <$> trfAnnList " " trfAlt' matches
trfExpr' (HsApp e1 e2) = AST.App <$> trfExpr e1 <*> trfExpr e2
trfExpr' (OpApp e1 (unLoc -> HsVar op) _ e2) 
  = AST.InfixApp <$> trfExpr e1 <*> trfOperator op <*> trfExpr e2
trfExpr' (NegApp e _) = AST.PrefixApp <$> annLocNoSema loc (AST.NormalOp <$> annLoc info loc (AST.nameFromList <$> trfNameStr "-"))
                                      <*> trfExpr e
  where loc = mkSrcSpan <$> atTheStart <*> (pure $ srcSpanStart (getLoc e))
        info = createNameInfo =<< (fromMaybe (error "minus operation is not found") <$> liftGhc negateOpName)
        negateOpName = getFromNameUsing (\n -> (\case Just (AnId id) -> Just id; _ -> Nothing) <$> lookupName n) negateName
trfExpr' (HsPar (unLoc -> SectionL expr (unLoc -> HsVar op))) = AST.LeftSection <$> trfExpr expr <*> trfOperator op
trfExpr' (HsPar (unLoc -> SectionR (unLoc -> HsVar op) expr)) = AST.RightSection <$> trfOperator op <*> trfExpr expr
trfExpr' (HsPar expr) = AST.Paren <$> trfExpr expr
trfExpr' (ExplicitTuple tupArgs box) | all tupArgPresent tupArgs 
  = wrap <$> between AnnOpenP AnnCloseP (trfAnnList' ", " (trfExpr . (\(Present e) -> e) . unLoc) tupArgs)
  where wrap = if box == Boxed then AST.Tuple else AST.UnboxedTuple
trfExpr' (ExplicitTuple tupArgs box)
  = wrap <$> between AnnOpenP AnnCloseP
               (do locs <- elemLocs
                   makeList ", " atTheEnd $ mapM trfTupSecElem (zip (map unLoc tupArgs) locs))
  where wrap = if box == Boxed then AST.TupleSection else AST.UnboxedTupSec
        trfTupSecElem :: forall n r . TransformName n r => (HsTupArg n, SrcSpan) -> Trf (Ann AST.TupSecElem (Dom r) RangeStage)
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

trfExpr' (HsCase expr (unLoc . mg_alts -> cases)) = AST.Case <$> trfExpr expr <*> (makeIndentedList (focusBeforeIfPresent AnnCloseC atTheEnd) (mapM trfAlt cases))
trfExpr' (HsIf _ expr thenE elseE) = AST.If <$> trfExpr expr <*> trfExpr thenE <*> trfExpr elseE
trfExpr' (HsMultiIf _ parts) = AST.MultiIf <$> trfAnnList "" trfGuardedCaseRhs' parts
trfExpr' (HsLet (unLoc -> binds) expr) = addToScope binds (AST.Let <$> trfLocalBinds binds <*> trfExpr expr)
trfExpr' (HsDo DoExpr (unLoc -> stmts) _) = AST.Do <$> annLocNoSema (tokenLoc AnnDo) (pure AST.DoKeyword) 
                                                   <*> makeNonemptyIndentedList (trfScopedSequence trfDoStmt stmts)
trfExpr' (HsDo MDoExpr (unLoc -> [unLoc -> RecStmt { recS_stmts = stmts }, lastStmt]) _) 
  = AST.Do <$> annLocNoSema (tokenLoc AnnMdo) (pure AST.MDoKeyword)
           <*> addToScope stmts (makeNonemptyIndentedList (mapM trfDoStmt (stmts ++ [lastStmt])))
trfExpr' (HsDo MDoExpr (unLoc -> stmts) _) = AST.Do <$> annLocNoSema (tokenLoc AnnMdo) (pure AST.MDoKeyword)
                                                    <*> addToScope stmts (makeNonemptyIndentedList (mapM trfDoStmt stmts))
-- TODO: scoping
trfExpr' (HsDo ListComp (unLoc -> stmts) _)
  = AST.ListComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (HsDo MonadComp (unLoc -> stmts) _)
  = AST.ListComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (HsDo PArrComp (unLoc -> stmts) _)
  = AST.ParArrayComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (ExplicitList _ _ exprs) = AST.List <$> trfAnnList' ", " trfExpr exprs
trfExpr' (ExplicitPArr _ exprs) = AST.ParArray <$> trfAnnList' ", " trfExpr exprs
trfExpr' (RecordCon name _ _ fields) = AST.RecCon <$> trfName name <*> trfFieldInits fields
trfExpr' (RecordUpd expr fields _ _ _ _) = AST.RecUpdate <$> trfExpr expr <*> trfAnnList ", " trfFieldUpdate fields
trfExpr' (ExprWithTySig expr typ) = AST.TypeSig <$> trfExpr expr <*> trfType (hswc_body $ hsib_body typ)
trfExpr' (ArithSeq _ _ (From from)) = AST.Enum <$> trfExpr from <*> nothing "," "" (before AnnDotdot)
                                                                <*> nothing "" "" (before AnnCloseS)
trfExpr' (ArithSeq _ _ (FromThen from step)) 
  = AST.Enum <$> trfExpr from <*> (makeJust <$> trfExpr step) <*> nothing "" "" (before AnnCloseS) 
trfExpr' (ArithSeq _ _ (FromTo from to)) 
  = AST.Enum <$> trfExpr from <*> nothing "," "" (before AnnDotdot)
                              <*> (makeJust <$> trfExpr to)
trfExpr' (ArithSeq _ _ (FromThenTo from step to)) 
  = AST.Enum <$> trfExpr from <*> (makeJust <$> trfExpr step) <*> (makeJust <$> trfExpr to)
trfExpr' (PArrSeq _ (FromTo from to)) 
  = AST.ParArrayEnum <$> trfExpr from <*> nothing "," "" (before AnnDotdot) <*> trfExpr to
trfExpr' (PArrSeq _ (FromThenTo from step to)) 
  = AST.ParArrayEnum <$> trfExpr from <*> (makeJust <$> trfExpr step) <*> trfExpr to
-- TODO: SCC, CORE, GENERATED annotations
trfExpr' (HsBracket brack) = AST.BracketExpr <$> annContNoSema (trfBracket' brack)
trfExpr' (HsSpliceE qq@(HsQuasiQuote {})) = AST.QuasiQuoteExpr <$> annContNoSema (trfQuasiQuotation' qq)
trfExpr' (HsSpliceE splice) = AST.Splice <$> annContNoSema (trfSplice' splice)
trfExpr' (HsRnBracketOut br _) = AST.BracketExpr <$> annContNoSema (trfBracket' br)
trfExpr' (HsProc pat cmdTop) = AST.Proc <$> trfPattern pat <*> trfCmdTop cmdTop
trfExpr' (HsStatic expr) = AST.StaticPtr <$> trfExpr expr
trfExpr' (HsAppType expr typ) = AST.ExplTypeApp <$> trfExpr expr <*> trfType (hswc_body typ)
trfExpr' t = error ("Illegal expression: " ++ showSDocUnsafe (ppr t) ++ " (ctor: " ++ show (toConstr t) ++ ")")
  
trfFieldInits :: TransformName n r => HsRecFields n (LHsExpr n) -> Trf (AnnList AST.FieldUpdate (Dom r) RangeStage)
trfFieldInits (HsRecFields fields dotdot)
  = do cont <- asks contRange
       let (normalFlds, implicitFlds) = partition ((cont /=) . getLoc) fields
       makeList ", " (before AnnCloseC)
         $ ((++) <$> mapM trfFieldInit normalFlds
                  <*> (if isJust dotdot then (:[]) <$> annLocNoSema (tokenLoc AnnDotdot) 
                                                                    (AST.FieldWildcard <$> (annCont (createImplicitFldInfo (unLoc . (\(HsVar n) -> n) . unLoc) (map unLoc implicitFlds)) (pure AST.FldWildcard))) 
                                        else pure []))
  
trfFieldInit :: TransformName n r => Located (HsRecField n (LHsExpr n)) -> Trf (Ann AST.FieldUpdate (Dom r) RangeStage)
trfFieldInit = trfLocNoSema $ \case
  HsRecField id _ True -> AST.FieldPun <$> trfName (getFieldOccName id)
  HsRecField id val False -> AST.NormalFieldUpdate <$> trfName (getFieldOccName id) <*> trfExpr val
  
trfFieldUpdate :: TransformName n r => HsRecField' (AmbiguousFieldOcc n) (LHsExpr n) -> Trf (AST.FieldUpdate (Dom r) RangeStage)
trfFieldUpdate (HsRecField id _ True) = AST.FieldPun <$> trfAmbiguousFieldName id
trfFieldUpdate (HsRecField id val False) = AST.NormalFieldUpdate <$> trfAmbiguousFieldName id <*> trfExpr val

trfAlt :: TransformName n r => Located (Match n (LHsExpr n)) -> Trf (Ann AST.Alt (Dom r) RangeStage)
trfAlt = trfLocNoSema trfAlt'

trfAlt' :: TransformName n r => Match n (LHsExpr n) -> Trf (AST.Alt (Dom r) RangeStage)
trfAlt' = gTrfAlt' trfExpr

gTrfAlt' :: TransformName n r => (Located (ge n) -> Trf (Ann ae (Dom r) RangeStage)) -> Match n (Located (ge n)) -> Trf (AST.Alt' ae (Dom r) RangeStage)
gTrfAlt' te (Match _ [pat] typ (GRHSs rhss (unLoc -> locBinds)))
  = AST.Alt <$> trfPattern pat <*> gTrfCaseRhss te rhss <*> trfWhereLocalBinds locBinds
  
trfCaseRhss :: TransformName n r => [Located (GRHS n (LHsExpr n))] -> Trf (Ann AST.CaseRhs (Dom r) RangeStage)
trfCaseRhss = gTrfCaseRhss trfExpr

gTrfCaseRhss :: TransformName n r => (Located (ge n) -> Trf (Ann ae (Dom r) RangeStage)) -> [Located (GRHS n (Located (ge n)))] -> Trf (Ann (AST.CaseRhs' ae) (Dom r) RangeStage)
gTrfCaseRhss te [unLoc -> GRHS [] body] = annLocNoSema (combineSrcSpans (getLoc body) <$> updateFocus (pure . updateEnd (const $ srcSpanStart $ getLoc body)) 
                                                                                                      (tokenLocBack AnnRarrow)) 
                                                 (AST.UnguardedCaseRhs <$> te body)
gTrfCaseRhss te rhss = annLocNoSema (pure $ collectLocs rhss) 
                              (AST.GuardedCaseRhss <$> trfAnnList ";" (gTrfGuardedCaseRhs' te) rhss)
  
trfGuardedCaseRhs :: TransformName n r => Located (GRHS n (LHsExpr n)) -> Trf (Ann AST.GuardedCaseRhs (Dom r) RangeStage)
trfGuardedCaseRhs = trfLocNoSema trfGuardedCaseRhs' 

trfGuardedCaseRhs' :: TransformName n r => GRHS n (LHsExpr n) -> Trf (AST.GuardedCaseRhs (Dom r) RangeStage)
trfGuardedCaseRhs' = gTrfGuardedCaseRhs' trfExpr

gTrfGuardedCaseRhs' :: TransformName n r => (Located (ge n) -> Trf (Ann ae (Dom r) RangeStage)) -> GRHS n (Located (ge n)) -> Trf (AST.GuardedCaseRhs' ae (Dom r) RangeStage)
gTrfGuardedCaseRhs' te (GRHS guards body) = AST.GuardedCaseRhs <$> trfAnnList " " trfRhsGuard' guards <*> te body

trfCmdTop :: TransformName n r => Located (HsCmdTop n) -> Trf (Ann AST.Cmd (Dom r) RangeStage)
trfCmdTop (L _ (HsCmdTop cmd _ _ _)) = trfCmd cmd

trfCmd :: TransformName n r => Located (HsCmd n) -> Trf (Ann AST.Cmd (Dom r) RangeStage)
trfCmd = trfLocNoSema trfCmd'

trfCmd' :: TransformName n r => HsCmd n -> Trf (AST.Cmd (Dom r) RangeStage)
trfCmd' (HsCmdArrApp left right _ typ dir) = AST.ArrowAppCmd <$> trfExpr left <*> op <*> trfExpr right 
  where op = case (typ, dir) of (HsFirstOrderApp, False) -> annLocNoSema (tokenLoc Annrarrowtail) (pure AST.RightAppl)
                                (HsFirstOrderApp, True) -> annLocNoSema (tokenLoc Annlarrowtail) (pure AST.LeftAppl)
                                (HsHigherOrderApp, False) -> annLocNoSema (tokenLoc AnnRarrowtail) (pure AST.RightHighApp)
                                (HsHigherOrderApp, True) -> annLocNoSema (tokenLoc AnnLarrowtail) (pure AST.LeftHighApp)
                                                                       -- FIXME: needs a before 
trfCmd' (HsCmdArrForm expr _ cmds) = AST.ArrowFormCmd <$> trfExpr expr <*> makeList " " (before AnnClose) (mapM trfCmdTop cmds)
trfCmd' (HsCmdApp cmd expr) = AST.AppCmd <$> trfCmd cmd <*> trfExpr expr
trfCmd' (HsCmdLam (MG (unLoc -> [unLoc -> Match _ pats _ (GRHSs [unLoc -> GRHS [] body] _)]) _ _ _)) 
  = AST.LambdaCmd <$> trfAnnList " " trfPattern' pats <*> trfCmd body
trfCmd' (HsCmdPar cmd) = AST.ParenCmd <$> trfCmd cmd
trfCmd' (HsCmdCase expr (MG (unLoc -> alts) _ _ _)) 
  = AST.CaseCmd <$> trfExpr expr <*> makeNonemptyIndentedList (mapM (trfLocNoSema (gTrfAlt' trfCmd)) alts) 
trfCmd' (HsCmdIf _ pred thenExpr elseExpr) = AST.IfCmd <$> trfExpr pred <*> trfCmd thenExpr <*> trfCmd elseExpr
trfCmd' (HsCmdLet (unLoc -> binds) cmd) = addToScope binds (AST.LetCmd <$> trfLocalBinds binds <*> trfCmd cmd)
trfCmd' (HsCmdDo (unLoc -> stmts) _) = AST.DoCmd <$> makeNonemptyIndentedList (mapM (trfLocNoSema (gTrfDoStmt' trfCmd)) stmts)