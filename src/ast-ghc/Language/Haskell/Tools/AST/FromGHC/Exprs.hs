{-# LANGUAGE LambdaCase
           , ViewPatterns
           #-}
module Language.Haskell.Tools.AST.FromGHC.Exprs where

import Data.Maybe
import Control.Monad.Reader

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

trfExpr :: TransformName n r => Located (HsExpr n) -> Trf (Ann AST.Expr r)
-- correction for empty cases (TODO: put of and {} inside)
trfExpr (L l cs@(HsCase expr (mg_alts -> []))) = annLoc (pure $ combineSrcSpans l (getLoc expr)) (trfExpr' cs)
trfExpr e = trfLoc trfExpr' e

trfExpr' :: TransformName n r => HsExpr n -> Trf (AST.Expr r)
trfExpr' (HsVar name) = AST.Var <$> annCont (trfName' name)
trfExpr' (HsIPVar (HsIPName ip)) = AST.Var <$> annCont (AST.NormalName <$> annCont (AST.nameFromList <$> trfNameStr (unpackFS ip)))
trfExpr' (HsOverLit (ol_val -> val)) = AST.Lit <$> annCont (trfOverloadedLit val)
trfExpr' (HsLit val) = AST.Lit <$> annCont (trfLiteral' val)
trfExpr' (HsLam (mg_alts -> [unLoc -> Match _ pats _ (GRHSs [unLoc -> GRHS [] expr] EmptyLocalBinds)]))
  = AST.Lambda <$> (trfAnnList " " trfPattern' pats) <*> addToScope pats (trfExpr expr)
trfExpr' (HsLamCase _ (mg_alts -> matches)) = AST.LamCase <$> trfAnnList " " trfAlt' matches
trfExpr' (HsApp e1 e2) = AST.App <$> trfExpr e1 <*> trfExpr e2
trfExpr' (OpApp e1 (L opLoc (HsVar op)) _ e2) 
  = AST.InfixApp <$> trfExpr e1 <*> annLoc (pure opLoc) (trfOperator' op) <*> trfExpr e2
trfExpr' (NegApp e _) = AST.PrefixApp <$> annLoc loc (AST.NormalOp <$> annLoc loc (AST.nameFromList <$> trfNameStr "-"))
                                      <*> trfExpr e
  where loc = mkSrcSpan <$> atTheStart <*> (pure $ srcSpanStart (getLoc e))
trfExpr' (HsPar expr) = AST.Paren <$> trfExpr expr
trfExpr' (SectionL expr (L l (HsVar op))) = AST.LeftSection <$> trfExpr expr <*> annLoc (pure l) (trfOperator' op)
trfExpr' (SectionR (L l (HsVar op)) expr) = AST.RightSection <$> annLoc (pure l) (trfOperator' op) <*> trfExpr expr
trfExpr' (ExplicitTuple tupArgs box) | all tupArgPresent tupArgs 
  = wrap <$> between AnnOpenP AnnCloseP (trfAnnList ", " (trfExpr' . unLoc . (\(Present e) -> e)) tupArgs)
  where wrap = if box == Boxed then AST.Tuple else AST.UnboxedTuple
trfExpr' (ExplicitTuple tupArgs box)
  = wrap <$> between AnnOpenP AnnCloseP
               (do locs <- elemLocs
                   makeList ", " atTheEnd $ mapM trfTupSecElem (zip (map unLoc tupArgs) locs))
  where wrap = if box == Boxed then AST.TupleSection else AST.UnboxedTupSec
        trfTupSecElem :: TransformName n r => (HsTupArg n, SrcSpan) -> Trf (Ann AST.TupSecElem r)
        trfTupSecElem (Present e, l) = annLoc (pure l) (AST.Present <$> annCont (trfExpr' (unLoc e)))
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

trfExpr' (HsCase expr (mg_alts -> cases)) = AST.Case <$> trfExpr expr <*> (makeIndentedList (focusBeforeIfPresent AnnCloseC atTheEnd) (mapM trfAlt cases))
trfExpr' (HsIf _ expr thenE elseE) = AST.If <$> trfExpr expr <*> trfExpr thenE <*> trfExpr elseE
trfExpr' (HsMultiIf _ parts) = AST.MultiIf <$> trfAnnList "" trfGuardedCaseRhs' parts
trfExpr' (HsLet binds expr) = AST.Let <$> addToScope binds (trfLocalBinds binds) <*> addToScope binds (trfExpr expr)
trfExpr' (HsDo DoExpr stmts _) = AST.Do <$> annLoc (tokenLoc AnnDo) (pure AST.DoKeyword) 
                                        <*> makeNonemptyIndentedList (trfScopedSequence trfDoStmt stmts)
trfExpr' (HsDo MDoExpr [unLoc -> RecStmt { recS_stmts = stmts }, lastStmt] _) 
  = AST.Do <$> annLoc (tokenLoc AnnMdo) (pure AST.MDoKeyword)
           <*> addToScope stmts (makeNonemptyIndentedList (mapM trfDoStmt (stmts ++ [lastStmt])))
trfExpr' (HsDo MDoExpr stmts _) = AST.Do <$> annLoc (tokenLoc AnnMdo) (pure AST.MDoKeyword)
                                         <*> addToScope stmts (makeNonemptyIndentedList (mapM trfDoStmt stmts))
-- TODO: scoping
trfExpr' (HsDo ListComp stmts _)
  = AST.ListComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (HsDo MonadComp stmts _)
  = AST.ListComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (HsDo PArrComp stmts _)
  = AST.ParArrayComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (ExplicitList _ _ exprs) = AST.List <$> trfAnnList ", " trfExpr' exprs
trfExpr' (ExplicitPArr _ exprs) = AST.ParArray <$> trfAnnList ", " trfExpr' exprs
trfExpr' (RecordCon name _ fields) = AST.RecCon <$> trfName name <*> trfFieldUpdates fields
trfExpr' (RecordUpd expr fields _ _ _) = AST.RecUpdate <$> trfExpr expr <*> trfFieldUpdates fields
trfExpr' (ExprWithTySig expr typ _) = AST.TypeSig <$> trfExpr expr <*> trfType typ
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
trfExpr' (HsSpliceE _ splice) = AST.Splice <$> annCont (trfSplice' splice)
trfExpr' (HsQuasiQuoteE qq) = AST.QuasiQuoteExpr <$> annCont (trfQuasiQuotation' qq)
trfExpr' (HsProc pat cmdTop) = AST.Proc <$> trfPattern pat <*> trfCmdTop cmdTop
trfExpr' (HsStatic expr) = AST.StaticPtr <$> trfExpr expr
-- TODO: static

trfFieldUpdates :: TransformName n r => HsRecordBinds n -> Trf (AnnList AST.FieldUpdate r)
trfFieldUpdates (HsRecFields fields dotdot) 
  = do cont <- asks contRange
       makeList ", " (before AnnCloseC)
         $ ((++) <$> mapM trfFieldUpdate (filter ((cont /=) . getLoc) fields) 
                 <*> (if isJust dotdot then (:[]) <$> annLoc (tokenLoc AnnDotdot) 
                                                             (pure AST.FieldWildcard) 
                                       else pure []))
  
trfFieldUpdate :: TransformName n r => Located (HsRecField n (LHsExpr n)) -> Trf (Ann AST.FieldUpdate r)
trfFieldUpdate = trfLoc $ \case
  HsRecField id _ True -> AST.FieldPun <$> annCont (trfName' (unLoc id))
  HsRecField id val False -> AST.NormalFieldUpdate <$> trfName id <*> trfExpr val
  
trfAlt :: TransformName n r => Located (Match n (LHsExpr n)) -> Trf (Ann AST.Alt r)
trfAlt = trfLoc trfAlt'

trfAlt' :: TransformName n r => Match n (LHsExpr n) -> Trf (AST.Alt r)
trfAlt' = gTrfAlt' trfExpr'

gTrfAlt' :: TransformName n r => (ge n -> Trf (ae r)) -> Match n (Located (ge n)) -> Trf (AST.Alt' ae r)
gTrfAlt' te (Match _ [pat] typ (GRHSs rhss locBinds))
  = AST.Alt <$> trfPattern pat <*> gTrfCaseRhss te rhss <*> trfWhereLocalBinds locBinds
  
trfCaseRhss :: TransformName n r => [Located (GRHS n (LHsExpr n))] -> Trf (Ann AST.CaseRhs r)
trfCaseRhss = gTrfCaseRhss trfExpr'

gTrfCaseRhss :: TransformName n r => (ge n -> Trf (ae r)) -> [Located (GRHS n (Located (ge n)))] -> Trf (Ann (AST.CaseRhs' ae) r)
gTrfCaseRhss te [unLoc -> GRHS [] body] = annLoc (combineSrcSpans (getLoc body) <$> tokenLocBack AnnRarrow) 
                                                 (AST.UnguardedCaseRhs <$> trfLoc te body)
gTrfCaseRhss te rhss = annLoc (pure $ collectLocs rhss) 
                              (AST.GuardedCaseRhss <$> trfAnnList ";" (gTrfGuardedCaseRhs' te) rhss)
  
trfGuardedCaseRhs :: TransformName n r => Located (GRHS n (LHsExpr n)) -> Trf (Ann AST.GuardedCaseRhs r)
trfGuardedCaseRhs = trfLoc trfGuardedCaseRhs' 

trfGuardedCaseRhs' :: TransformName n r => GRHS n (LHsExpr n) -> Trf (AST.GuardedCaseRhs r)
trfGuardedCaseRhs' = gTrfGuardedCaseRhs' trfExpr'

gTrfGuardedCaseRhs' :: TransformName n r => (ge n -> Trf (ae r)) -> GRHS n (Located (ge n)) -> Trf (AST.GuardedCaseRhs' ae r)
gTrfGuardedCaseRhs' te (GRHS guards body) = AST.GuardedCaseRhs <$> trfAnnList " " trfRhsGuard' guards <*> trfLoc te body

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
trfCmd' (HsCmdLam (MG [unLoc -> Match _ pats _ (GRHSs [unLoc -> GRHS [] body] _)] _ _ _)) 
  = AST.LambdaCmd <$> trfAnnList " " trfPattern' pats <*> trfCmd body
trfCmd' (HsCmdPar cmd) = AST.ParenCmd <$> trfCmd cmd
trfCmd' (HsCmdCase expr (MG alts _ _ _)) = AST.CaseCmd <$> trfExpr expr <*> makeNonemptyIndentedList (mapM (trfLoc (gTrfAlt' trfCmd')) alts) 
trfCmd' (HsCmdIf _ pred thenExpr elseExpr) = AST.IfCmd <$> trfExpr pred <*> trfCmd thenExpr <*> trfCmd elseExpr
trfCmd' (HsCmdLet binds cmd) = AST.LetCmd <$> trfLocalBinds binds <*> trfCmd cmd
trfCmd' (HsCmdDo stmts _) = AST.DoCmd <$> makeNonemptyIndentedList (mapM (trfLoc (gTrfDoStmt' trfCmd')) stmts)