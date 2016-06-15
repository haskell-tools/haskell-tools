{-# LANGUAGE LambdaCase
           , ViewPatterns
           , ScopedTypeVariables
           #-}
module Language.Haskell.Tools.AST.FromGHC.Exprs where

import Data.Maybe
import Data.Data (toConstr)
import Control.Monad.Reader
import Control.Reference

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

import Language.Haskell.Tools.AST (Ann(..), AnnList(..))
import qualified Language.Haskell.Tools.AST as AST

import Debug.Trace

trfExpr :: forall n r . TransformName n r => Located (HsExpr n) -> Trf (Ann AST.Expr r)
-- correction for empty cases (TODO: put of and {} inside)
trfExpr (L l cs@(HsCase expr (unLoc . mg_alts -> []))) 
  = addScopeInfo (SemanticsPhantom :: SemanticsPhantom n) =<< annLoc (pure $ combineSrcSpans l (getLoc expr)) (trfExpr' cs)
trfExpr e = addScopeInfo (SemanticsPhantom :: SemanticsPhantom n) =<< trfLoc trfExpr' e

addScopeInfo :: forall n r . TransformName n r => SemanticsPhantom n -> Ann AST.Expr r -> Trf (Ann AST.Expr r)
addScopeInfo _ expr = do scope <- asks localsInScope
                         return $ AST.annotation .- addSemanticInfo (AST.ScopeInfo scope :: AST.SemanticInfo n) $ expr

trfExpr' :: TransformName n r => HsExpr n -> Trf (AST.Expr r)
trfExpr' (HsVar name) = AST.Var <$> trfName name
trfExpr' (HsRecFld fld) = AST.Var <$> (asks contRange >>= \l -> trfAmbiguousFieldName' l fld)
trfExpr' (HsIPVar (HsIPName ip)) = AST.Var <$> annCont (AST.NormalName <$> annCont (AST.nameFromList <$> trfNameStr (unpackFS ip)))
trfExpr' (HsOverLit (ol_val -> val)) = AST.Lit <$> annCont (trfOverloadedLit val)
trfExpr' (HsLit val) = AST.Lit <$> annCont (trfLiteral' val)
trfExpr' (HsLam (unLoc . mg_alts -> [unLoc -> Match _ pats _ (GRHSs [unLoc -> GRHS [] expr] (unLoc -> EmptyLocalBinds))]))
  = AST.Lambda <$> (trfAnnList " " trfPattern' pats) <*> addToScope pats (trfExpr expr)
trfExpr' (HsLamCase _ (unLoc . mg_alts -> matches)) = AST.LamCase <$> trfAnnList " " trfAlt' matches
trfExpr' (HsApp e1 e2) = AST.App <$> trfExpr e1 <*> trfExpr e2
trfExpr' (OpApp e1 (unLoc -> HsVar op) _ e2) 
  = AST.InfixApp <$> trfExpr e1 <*> trfOperator op <*> trfExpr e2
trfExpr' (NegApp e _) = AST.PrefixApp <$> annLoc loc (AST.NormalOp <$> annLoc loc (AST.nameFromList <$> trfNameStr "-"))
                                      <*> trfExpr e
  where loc = mkSrcSpan <$> atTheStart <*> (pure $ srcSpanStart (getLoc e))
trfExpr' (HsPar expr) = AST.Paren <$> trfExpr expr
trfExpr' (SectionL expr (unLoc -> HsVar op)) = AST.LeftSection <$> trfExpr expr <*> trfOperator op
trfExpr' (SectionR (unLoc -> HsVar op) expr) = AST.RightSection <$> trfOperator op <*> trfExpr expr
trfExpr' (ExplicitTuple tupArgs box) | all tupArgPresent tupArgs 
  = wrap <$> between AnnOpenP AnnCloseP (trfAnnList' ", " (trfExpr . (\(Present e) -> e) . unLoc) tupArgs)
  where wrap = if box == Boxed then AST.Tuple else AST.UnboxedTuple
trfExpr' (ExplicitTuple tupArgs box)
  = wrap <$> between AnnOpenP AnnCloseP
               (do locs <- elemLocs
                   makeList ", " atTheEnd $ mapM trfTupSecElem (zip (map unLoc tupArgs) locs))
  where wrap = if box == Boxed then AST.TupleSection else AST.UnboxedTupSec
        trfTupSecElem :: forall n r . TransformName n r => (HsTupArg n, SrcSpan) -> Trf (Ann AST.TupSecElem r)
        trfTupSecElem (Present e, l) 
          = annLoc (pure l) (AST.Present <$> (addScopeInfo (SemanticsPhantom :: SemanticsPhantom n) =<< annCont (trfExpr' (unLoc e))))
        trfTupSecElem (Missing _, l) = annLoc (pure l) (pure AST.Missing)
        
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
trfExpr' (HsLet (unLoc -> binds) expr) = AST.Let <$> addToScope binds (trfLocalBinds binds) <*> addToScope binds (trfExpr expr)
trfExpr' (HsDo DoExpr (unLoc -> stmts) _) = AST.Do <$> annLoc (tokenLoc AnnDo) (pure AST.DoKeyword) 
                                                   <*> makeNonemptyIndentedList (trfScopedSequence trfDoStmt stmts)
trfExpr' (HsDo MDoExpr (unLoc -> [unLoc -> RecStmt { recS_stmts = stmts }, lastStmt]) _) 
  = AST.Do <$> annLoc (tokenLoc AnnMdo) (pure AST.MDoKeyword)
           <*> addToScope stmts (makeNonemptyIndentedList (mapM trfDoStmt (stmts ++ [lastStmt])))
trfExpr' (HsDo MDoExpr (unLoc -> stmts) _) = AST.Do <$> annLoc (tokenLoc AnnMdo) (pure AST.MDoKeyword)
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
trfExpr' (HsBracket brack) = AST.BracketExpr <$> annCont (trfBracket' brack)
trfExpr' (HsRnBracketOut brack _) = AST.BracketExpr <$> annCont (trfBracket' brack)
trfExpr' (HsTcBracketOut brack _) = AST.BracketExpr <$> annCont (trfBracket' brack)
trfExpr' (HsSpliceE qq@(HsQuasiQuote {})) = AST.QuasiQuoteExpr <$> annCont (trfQuasiQuotation' qq)
trfExpr' (HsSpliceE splice) = AST.Splice <$> annCont (trfSplice' splice)
trfExpr' (HsProc pat cmdTop) = AST.Proc <$> trfPattern pat <*> trfCmdTop cmdTop
trfExpr' (HsStatic expr) = AST.StaticPtr <$> trfExpr expr
trfExpr' (HsAppType expr typ) = AST.ExplTypeApp <$> trfExpr expr <*> trfType (hswc_body typ)
trfExpr' t = error ("Illegal expression: " ++ showSDocUnsafe (ppr t) ++ " (ctor: " ++ show (toConstr t) ++ ")")
  
trfFieldInits :: TransformName n r => HsRecFields n (LHsExpr n) -> Trf (AnnList AST.FieldUpdate r)
trfFieldInits (HsRecFields fields dotdot) 
  = do cont <- asks contRange
       makeList ", " (before AnnCloseC)
         $ ((++) <$> mapM trfFieldInit (filter ((cont /=) . getLoc) fields) 
                  <*> (if isJust dotdot then (:[]) <$> annLoc (tokenLoc AnnDotdot) 
                                                              (pure AST.FieldWildcard) 
                                        else pure []))
  
trfFieldInit :: TransformName n r => Located (HsRecField n (LHsExpr n)) -> Trf (Ann AST.FieldUpdate r)
trfFieldInit = trfLoc $ \case
  HsRecField id _ True -> AST.FieldPun <$> trfName (getFieldOccName id)
  HsRecField id val False -> AST.NormalFieldUpdate <$> trfName (getFieldOccName id) <*> trfExpr val
  
trfFieldUpdate :: TransformName n r => HsRecField' (AmbiguousFieldOcc n) (LHsExpr n) -> Trf (AST.FieldUpdate r)
trfFieldUpdate (HsRecField id _ True) = AST.FieldPun <$> trfAmbiguousFieldName id
trfFieldUpdate (HsRecField id val False) = AST.NormalFieldUpdate <$> trfAmbiguousFieldName id <*> trfExpr val

trfAlt :: TransformName n r => Located (Match n (LHsExpr n)) -> Trf (Ann AST.Alt r)
trfAlt = trfLoc trfAlt'

trfAlt' :: TransformName n r => Match n (LHsExpr n) -> Trf (AST.Alt r)
trfAlt' = gTrfAlt' trfExpr

gTrfAlt' :: TransformName n r => (Located (ge n) -> Trf (Ann ae r)) -> Match n (Located (ge n)) -> Trf (AST.Alt' ae r)
gTrfAlt' te (Match _ [pat] typ (GRHSs rhss (unLoc -> locBinds)))
  = AST.Alt <$> trfPattern pat <*> gTrfCaseRhss te rhss <*> trfWhereLocalBinds locBinds
  
trfCaseRhss :: TransformName n r => [Located (GRHS n (LHsExpr n))] -> Trf (Ann AST.CaseRhs r)
trfCaseRhss = gTrfCaseRhss trfExpr

gTrfCaseRhss :: TransformName n r => (Located (ge n) -> Trf (Ann ae r)) -> [Located (GRHS n (Located (ge n)))] -> Trf (Ann (AST.CaseRhs' ae) r)
gTrfCaseRhss te [unLoc -> GRHS [] body] = annLoc (combineSrcSpans (getLoc body) <$> tokenLocBack AnnRarrow) 
                                                 (AST.UnguardedCaseRhs <$> te body)
gTrfCaseRhss te rhss = annLoc (pure $ collectLocs rhss) 
                              (AST.GuardedCaseRhss <$> trfAnnList ";" (gTrfGuardedCaseRhs' te) rhss)
  
trfGuardedCaseRhs :: TransformName n r => Located (GRHS n (LHsExpr n)) -> Trf (Ann AST.GuardedCaseRhs r)
trfGuardedCaseRhs = trfLoc trfGuardedCaseRhs' 

trfGuardedCaseRhs' :: TransformName n r => GRHS n (LHsExpr n) -> Trf (AST.GuardedCaseRhs r)
trfGuardedCaseRhs' = gTrfGuardedCaseRhs' trfExpr

gTrfGuardedCaseRhs' :: TransformName n r => (Located (ge n) -> Trf (Ann ae r)) -> GRHS n (Located (ge n)) -> Trf (AST.GuardedCaseRhs' ae r)
gTrfGuardedCaseRhs' te (GRHS guards body) = AST.GuardedCaseRhs <$> trfAnnList " " trfRhsGuard' guards <*> te body

trfCmdTop :: TransformName n r => Located (HsCmdTop n) -> Trf (Ann AST.Cmd r)
trfCmdTop (L _ (HsCmdTop cmd _ _ _)) = trfCmd cmd

trfCmd :: TransformName n r => Located (HsCmd n) -> Trf (Ann AST.Cmd r)
trfCmd = trfLoc trfCmd'

trfCmd' :: TransformName n r => HsCmd n -> Trf (AST.Cmd r)
trfCmd' (HsCmdArrApp left right _ typ dir) = AST.ArrowAppCmd <$> trfExpr left <*> op <*> trfExpr right 
  where op = case (typ, dir) of (HsFirstOrderApp, False) -> annLoc (tokenLoc Annrarrowtail) (pure AST.RightAppl)
                                (HsFirstOrderApp, True) -> annLoc (tokenLoc Annlarrowtail) (pure AST.LeftAppl)
                                (HsHigherOrderApp, False) -> annLoc (tokenLoc AnnRarrowtail) (pure AST.RightHighApp)
                                (HsHigherOrderApp, True) -> annLoc (tokenLoc AnnLarrowtail) (pure AST.LeftHighApp)
                                                                       -- FIXME: needs a before 
trfCmd' (HsCmdArrForm expr _ cmds) = AST.ArrowFormCmd <$> trfExpr expr <*> makeList " " (before AnnClose) (mapM trfCmdTop cmds)
trfCmd' (HsCmdApp cmd expr) = AST.AppCmd <$> trfCmd cmd <*> trfExpr expr
trfCmd' (HsCmdLam (MG (unLoc -> [unLoc -> Match _ pats _ (GRHSs [unLoc -> GRHS [] body] _)]) _ _ _)) 
  = AST.LambdaCmd <$> trfAnnList " " trfPattern' pats <*> trfCmd body
trfCmd' (HsCmdPar cmd) = AST.ParenCmd <$> trfCmd cmd
trfCmd' (HsCmdCase expr (MG (unLoc -> alts) _ _ _)) 
  = AST.CaseCmd <$> trfExpr expr <*> makeNonemptyIndentedList (mapM (trfLoc (gTrfAlt' trfCmd)) alts) 
trfCmd' (HsCmdIf _ pred thenExpr elseExpr) = AST.IfCmd <$> trfExpr pred <*> trfCmd thenExpr <*> trfCmd elseExpr
trfCmd' (HsCmdLet (unLoc -> binds) cmd) = AST.LetCmd <$> trfLocalBinds binds <*> trfCmd cmd
trfCmd' (HsCmdDo (unLoc -> stmts) _) = AST.DoCmd <$> makeNonemptyIndentedList (mapM (trfLoc (gTrfDoStmt' trfCmd)) stmts)