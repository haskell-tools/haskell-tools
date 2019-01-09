{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions that convert the expression-related elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.BackendGHC.Exprs where

import Control.Monad.Reader
import Data.Function (on)
import Data.List
import Data.Maybe (Maybe(..), isJust, fromMaybe, catMaybes)

import BasicTypes as GHC (Boxity(..), StringLiteral(..))
import FastString (unpackFS)
import GHC
import OccName as GHC (occNameString)
import PrelNames as GHC (negateName)
import SrcLoc as GHC

import Language.Haskell.Tools.AST.SemaInfoTypes
import {-# SOURCE #-} Language.Haskell.Tools.BackendGHC.Binds (trfRhsGuard', trfWhereLocalBinds, trfLocalBinds)
import Language.Haskell.Tools.BackendGHC.GHCUtils (GHCName(..), getFieldOccName)
import Language.Haskell.Tools.BackendGHC.Literals
import Language.Haskell.Tools.BackendGHC.Monad
import Language.Haskell.Tools.BackendGHC.Names
import Language.Haskell.Tools.BackendGHC.Patterns (trfPattern)
import Language.Haskell.Tools.BackendGHC.Stmts
import {-# SOURCE #-} Language.Haskell.Tools.BackendGHC.TH (trfBracket', trfSplice, trfQuasiQuotation')
import Language.Haskell.Tools.BackendGHC.Types (trfType)
import Language.Haskell.Tools.BackendGHC.Utils

import Language.Haskell.Tools.AST (Ann, AnnListG, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfExpr :: forall n r p . (TransformName n r, n ~ GhcPass p) => Located (HsExpr n) -> Trf (Ann AST.UExpr (Dom r) RangeStage)
-- correction for empty cases
trfExpr (L l cs@(HsCase _ expr (unLoc . mg_alts -> [])))
  = do let realSpan = combineSrcSpans l (getLoc expr)
       tokensAfter <- allTokensAfter (srcSpanEnd realSpan)
       let actualSpan = case take 3 tokensAfter of
                          [(_, AnnOf), (_, AnnOpenC), (endSpan, AnnCloseC)] -> realSpan `combineSrcSpans` endSpan
                          ((endSpan, AnnOf) : _) -> realSpan `combineSrcSpans` endSpan
                          _ -> convProblem "trfExpr: case without 'of' '{' or '}' token"
       annLoc createScopeInfo (pure actualSpan) (trfExpr' cs)
trfExpr e | RealSrcSpan loce <- getLoc e
  = do exprSpls <- asks exprSplices
       let contSplice = filter (\sp -> case getLoc sp of (RealSrcSpan spLoc) -> spLoc `containsSpan` loce; _ -> False) exprSpls
       case contSplice of
         [] -> trfLoc trfExpr' createScopeInfo e
         _ -> let lsp@(L l sp) = minimumBy (compareSpans `on` getLoc) contSplice
               in case sp of
                    (HsQuasiQuote {}) -> do
                      sp' <- rdrSplice sp
                      exprSpliceInserted lsp (annLoc createScopeInfo (pure l) (AST.UQuasiQuoteExpr <$> annLocNoSema (pure l) (trfQuasiQuotation' sp')))
                    _ -> do sp' <- rdrSplice sp
                            exprSpliceInserted lsp (annLoc createScopeInfo (pure l) (AST.USplice <$> trfSplice sp'))
  | otherwise = trfLoc trfExpr' createScopeInfo e

createScopeInfo :: Trf ScopeInfo
createScopeInfo = do scope <- asks localsInScope
                     return (mkScopeInfo scope)

trfExpr' :: forall n r p . (TransformName n r, n ~ GhcPass p) => HsExpr n -> Trf (AST.UExpr (Dom r) RangeStage)
trfExpr' (HsVar _ name) = AST.UVar <$> trfName @n name
trfExpr' (HsUnboundVar _ name) = AST.UVar <$> trfNameText (occNameString $ unboundVarOcc name)
trfExpr' (HsRecFld _ fld) = AST.UVar <$> (asks contRange >>= \l -> trfAmbiguousFieldName' l fld)
trfExpr' (HsIPVar _ ip) = AST.UVar <$> trfImplicitName ip
trfExpr' (HsOverLit _ (ol_val -> val)) = AST.ULit <$> annCont (asks contRange >>= pure . PreLiteralInfo) (trfOverloadedLit val)
trfExpr' (HsLit _ val) = AST.ULit <$> annCont (pure $ RealLiteralInfo (monoLiteralType val)) (trfLiteral' val)
trfExpr' (HsLam _ (unLoc . mg_alts -> [unLoc -> Match _ _ pats (GRHSs _ [unLoc -> GRHS _ [] expr] (unLoc -> EmptyLocalBinds _))]))
  = AST.ULambda <$> (makeNonemptyList " " $ mapM trfPattern pats) <*> addToScope pats (trfExpr expr)
trfExpr' (HsLamCase _ (unLoc . mg_alts -> matches)) = AST.ULamCase <$> addToScope matches (trfAnnList " " trfAlt' matches)
trfExpr' (HsApp _ e1 e2) = AST.UApp <$> trfExpr e1 <*> trfExpr e2
trfExpr' (OpApp _ e1 (unLoc -> HsVar _ op) e2)
  = AST.UInfixApp <$> trfExpr e1 <*> trfOperator @n op <*> trfExpr e2
trfExpr' (OpApp _ e1 (L nameLoc (HsRecFld _ fld)) e2)
  = AST.UInfixApp <$> trfExpr e1 <*> trfAmbiguousOperator' nameLoc fld <*> trfExpr e2
trfExpr' (OpApp _ _ (L _ op) _) = unhandledElement "OpApp expression" op
trfExpr' (NegApp _ e _) = AST.UPrefixApp <$> annLocNoSema loc (AST.UNormalOp <$> annLoc info loc (AST.nameFromList <$> trfOperatorStr False "-"))
                                       <*> trfExpr e
  where loc = mkSrcSpan <$> atTheStart <*> (pure $ srcSpanStart (getLoc e))
        info = createNameInfo =<< (fromMaybe (convProblem "minus operation is not found") <$> liftGhc negateOpName)
        negateOpName = getFromNameUsing @r (\n -> (\case Just (AnId id) -> Just id; _ -> Nothing) <$> lookupName n) negateName
trfExpr' (HsPar _ (unLoc -> SectionL _ expr (unLoc -> HsVar _ op))) = AST.ULeftSection <$> trfExpr expr <*> trfOperator @n op
trfExpr' (HsPar _ (unLoc -> SectionL _ expr (L nameLoc (HsRecFld _ op))))
  = AST.ULeftSection <$> trfExpr expr <*> trfAmbiguousOperator' nameLoc op
trfExpr' (HsPar _ (unLoc -> SectionR _ (unLoc -> HsVar _ op) expr)) = AST.URightSection <$> trfOperator @n op <*> trfExpr expr
trfExpr' (HsPar _ (unLoc -> SectionR _ (L nameLoc (HsRecFld _ op)) expr))
  = AST.URightSection <$> trfAmbiguousOperator' nameLoc op <*> trfExpr expr
trfExpr' (HsPar _ expr) = AST.UParen <$> trfExpr expr
trfExpr' (ExplicitTuple _ tupArgs box) | all tupArgPresent tupArgs
  = wrap <$> between (if box == Boxed then AnnOpenP else AnnOpen) (if box == Boxed then AnnCloseP else AnnClose)
               (trfAnnList' ", " (trfExpr . (\(Present _ e) -> e) . unLoc) tupArgs)
  where wrap = if box == Boxed then AST.UTuple else AST.UUnboxedTuple
trfExpr' (ExplicitTuple _ tupArgs box)
  = wrap <$> between (if box == Boxed then AnnOpenP else AnnOpen) (if box == Boxed then AnnCloseP else AnnClose)
               (do locs <- elemLocs
                   makeList ", " atTheEnd $ mapM trfTupSecElem (zip (map unLoc tupArgs) locs))
  where wrap = if box == Boxed then AST.UTupleSection else AST.UUnboxedTupSec
        trfTupSecElem :: forall n r . (TransformName n r, n ~ GhcPass p) => (HsTupArg n, SrcSpan) -> Trf (Ann AST.UTupSecElem (Dom r) RangeStage)
        trfTupSecElem (Present _ e, l)
          = annLocNoSema (pure l) (AST.Present <$> (annCont createScopeInfo (trfExpr' (unLoc e))))
        trfTupSecElem (Missing _, l) = annLocNoSema (pure l) (pure AST.Missing)

        existingArgs :: [SrcSpan]
        existingArgs = catMaybes $ map (\case Present _ p -> Just (getLoc p); _ -> Nothing) $ map unLoc tupArgs

        elemLocs :: Trf [SrcSpan]
        elemLocs = do r <- asks contRange
                      commaLocs <- allTokenLoc AnnComma
                      return $ foldl breakUp [r] (filter freeComma commaLocs)
          where freeComma (RealSrcSpan s) = not $ any (\case RealSrcSpan e -> e `containsSpan` s; _ -> False) existingArgs
                freeComma _ = False

        breakUp :: [SrcSpan] -> SrcSpan -> [SrcSpan]
        breakUp cont sep = concatMap (breakUpOne sep) cont

        breakUpOne :: SrcSpan -> SrcSpan -> [SrcSpan]
        breakUpOne sep@(RealSrcSpan realSep) sp@(RealSrcSpan realSp)
          | realSp `containsSpan` realSep = [mkSrcSpan (srcSpanStart sp) (srcSpanStart sep), mkSrcSpan (srcSpanEnd sep) (srcSpanEnd sp)]
        breakUpOne _ sp = [sp]

trfExpr' (HsCase _ expr (unLoc . mg_alts -> cases)) = AST.UCase <$> trfExpr expr <*> (addToScope cases $ makeIndentedList (focusBeforeIfPresent AnnCloseC atTheEnd) (mapM trfAlt cases))
trfExpr' (HsIf _ _ expr thenE elseE) = AST.UIf <$> trfExpr expr <*> trfExpr thenE <*> trfExpr elseE
trfExpr' (HsMultiIf _ parts) = AST.UMultiIf <$> trfAnnList "" trfGuardedCaseRhs' parts
trfExpr' (HsLet _ (unLoc -> binds) expr) = addToScope binds (AST.ULet <$> trfLocalBinds AnnLet binds <*> trfExpr expr)
trfExpr' (HsDo _ DoExpr (unLoc -> stmts)) = AST.UDo <$> annLocNoSema (tokenLoc AnnDo) (pure AST.UDoKeyword)
                                                    <*> makeNonemptyIndentedList (trfScopedSequence trfDoStmt stmts)
trfExpr' (HsDo _ MDoExpr (unLoc -> [unLoc -> RecStmt { recS_stmts = stmts }, lastStmt]))
  = AST.UDo <$> annLocNoSema (tokenLoc AnnMdo) (pure AST.UMDoKeyword)
            <*> addToScope stmts (makeNonemptyIndentedList (mapM trfDoStmt (stmts ++ [lastStmt])))
trfExpr' (HsDo _ MDoExpr (unLoc -> stmts)) = AST.UDo <$> annLocNoSema (tokenLoc AnnMdo) (pure AST.UMDoKeyword)
                                                     <*> addToScope stmts (makeNonemptyIndentedList (mapM trfDoStmt stmts))
trfExpr' (HsDo _ ListComp (unLoc -> stmts))
  = AST.UListComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (HsDo _ MonadComp (unLoc -> stmts))
  = AST.UListComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (HsDo _ (ParStmtCtxt _) (unLoc -> stmts))
  = AST.UParArrayComp <$> trfExpr (getLastStmt stmts) <*> trfListCompStmts stmts
trfExpr' (ExplicitList _ _ exprs) = AST.UList <$> trfAnnList' ", " trfExpr exprs
-- trfExpr' (ExplicitPArr _ exprs) = AST.UParArray <$> trfAnnList' ", " trfExpr exprs
trfExpr' (RecordCon _ name fields) = AST.URecCon <$> trfName @n name <*> trfFieldInits fields
trfExpr' (RecordUpd _ expr fields) = AST.URecUpdate <$> trfExpr expr <*> trfAnnList ", " trfFieldUpdate fields
trfExpr' (ExprWithTySig typ expr) = AST.UTypeSig <$> trfExpr expr <*> trfType (hsib_body $ hswc_body typ)
trfExpr' (ArithSeq _ _ (From from)) = AST.UEnum <$> trfExpr from <*> nothing "," "" (before AnnDotdot)
                                                                <*> nothing "" "" (before AnnCloseS)
trfExpr' (ArithSeq _ _ (FromThen from step))
  = AST.UEnum <$> trfExpr from <*> (makeJust <$> trfExpr step) <*> nothing "" "" (before AnnCloseS)
trfExpr' (ArithSeq _ _ (FromTo from to))
  = AST.UEnum <$> trfExpr from <*> nothing "," "" (before AnnDotdot)
                               <*> (makeJust <$> trfExpr to)
trfExpr' (ArithSeq _ _ (FromThenTo from step to))
  = AST.UEnum <$> trfExpr from <*> (makeJust <$> trfExpr step) <*> (makeJust <$> trfExpr to)
-- trfExpr' (PArrSeq _ (FromTo from to))
--   = AST.UParArrayEnum <$> trfExpr from <*> nothing "," "" (before AnnDotdot) <*> trfExpr to
-- trfExpr' (PArrSeq _ (FromThenTo from step to))
--   = AST.UParArrayEnum <$> trfExpr from <*> (makeJust <$> trfExpr step) <*> trfExpr to
trfExpr' (HsBracket _ brack) = AST.UBracketExpr <$> annContNoSema (trfBracket' brack)
trfExpr' (HsSpliceE _ qq@(HsQuasiQuote {})) = AST.UQuasiQuoteExpr <$> annContNoSema (trfQuasiQuotation' qq)
trfExpr' (HsSpliceE _ splice) = AST.USplice <$> trfSplice splice
trfExpr' (HsRnBracketOut _ br _) = AST.UBracketExpr <$> annContNoSema (trfBracket' br)
trfExpr' (HsProc _ pat cmdTop) = AST.UProc <$> trfPattern pat <*> trfCmdTop cmdTop
trfExpr' (HsStatic _ expr) = AST.UStaticPtr <$> trfExpr expr
trfExpr' (HsAppType typ expr) = AST.UExplTypeApp <$> trfExpr expr <*> trfType (hswc_body typ)
trfExpr' (HsSCC _ _ lit expr) = AST.UExprPragma <$> pragma <*> trfExpr expr
  where pragma = do pragLoc <- tokensLoc [AnnOpen, AnnClose]
                    focusOn pragLoc $ annContNoSema (AST.USccPragma <$> annLocNoSema (mappend <$> tokenLoc AnnValStr <*> tokenLocBack AnnVal) (trfText' lit))
trfExpr' (HsCoreAnn _ _ lit expr) = AST.UExprPragma <$> pragma <*> trfExpr expr
  where pragma = do pragLoc <- tokensLoc [AnnOpen, AnnClose]
                    focusOn pragLoc $ annContNoSema (AST.UCorePragma <$> annLocNoSema (mappend <$> tokenLoc AnnValStr <*> tokenLocBack AnnVal) (trfText' lit))
trfExpr' (HsTickPragma _ _ source _ expr) = AST.UExprPragma <$> pragma <*> trfExpr expr
  where pragma = do pragLoc <- tokensLoc [AnnOpen, AnnClose]
                    focusOn pragLoc $ annContNoSema (AST.UGeneratedPragma <$> (trfSourceRange source))
trfExpr' (ExplicitSum _ tag arity expr)
  = do sepsBefore <- focusBeforeLoc (srcSpanStart (getLoc expr)) (eachTokenLoc (AnnOpen : replicate (tag - 1) AnnVbar))
       sepsAfter <- focusAfterLoc (srcSpanEnd (getLoc expr)) (eachTokenLoc (replicate (arity - tag) AnnVbar))
       let locsBefore = map srcSpanEnd $ init sepsBefore
           locsAfter = map srcSpanEnd sepsAfter
       AST.UUnboxedSum <$> makeList " | " (after AnnOpen) (mapM makePlaceholder locsBefore)
                       <*> trfExpr expr
                       <*> makeList " | " (before AnnClose) (mapM makePlaceholder locsAfter)
  where makePlaceholder l = annLocNoSema (pure (srcLocSpan l)) (pure AST.UUnboxedSumPlaceHolder)
trfExpr' (EWildPat _) = return AST.UHole
trfExpr' t = unhandledElement "expression" t

trfFieldInits :: (TransformName n r, n ~ GhcPass p) => HsRecFields n (LHsExpr n) -> Trf (AnnListG AST.UFieldUpdate (Dom r) RangeStage)
trfFieldInits (HsRecFields fields dotdot)
  = do cont <- asks contRange
       let (normalFlds, implicitFlds) = partition ((cont /=) . getLoc) fields
       makeList ", " (before AnnCloseC)
         $ ((++) <$> mapM trfFieldInit normalFlds
                  <*> (if isJust dotdot then (:[]) <$> annLocNoSema (tokenLoc AnnDotdot)
                                                                    (AST.UFieldWildcard <$> (annCont (createImplicitFldInfo (unLoc . (\(HsVar _ n) -> n) . unLoc) (map unLoc implicitFlds)) (pure AST.FldWildcard)))
                                        else pure []))

trfFieldInit :: forall n r p . (TransformName n r, n ~ GhcPass p) => Located (HsRecField n (LHsExpr n)) -> Trf (Ann AST.UFieldUpdate (Dom r) RangeStage)
trfFieldInit = trfLocNoSema $ \case
  HsRecField id _ True -> AST.UFieldPun <$> trfName @n (getFieldOccName id)
  HsRecField id val False -> AST.UNormalFieldUpdate <$> trfName @n (getFieldOccName id) <*> trfExpr val

trfFieldUpdate :: (TransformName n r, n ~ GhcPass p) => HsRecField' (AmbiguousFieldOcc n) (LHsExpr n) -> Trf (AST.UFieldUpdate (Dom r) RangeStage)
trfFieldUpdate (HsRecField id _ True) = AST.UFieldPun <$> trfAmbiguousFieldName id
trfFieldUpdate (HsRecField id val False) = AST.UNormalFieldUpdate <$> trfAmbiguousFieldName id <*> trfExpr val

trfAlt :: (TransformName n r, n ~ GhcPass p) => Located (Match n (LHsExpr n)) -> Trf (Ann AST.UAlt (Dom r) RangeStage)
trfAlt = trfLocNoSema trfAlt'

trfAlt' :: (TransformName n r, n ~ GhcPass p) => Match n (LHsExpr n) -> Trf (AST.UAlt (Dom r) RangeStage)
trfAlt' = gTrfAlt' trfExpr

gTrfAlt' :: (TransformName n r, n ~ GhcPass p) => (Located (ge n) -> Trf (Ann ae (Dom r) RangeStage)) -> Match n (Located (ge n)) -> Trf (AST.UAlt' ae (Dom r) RangeStage)
gTrfAlt' te (Match _ _ [pat] (GRHSs _ rhss (unLoc -> locBinds)))
  = AST.UAlt <$> trfPattern pat <*> gTrfCaseRhss te rhss <*> trfWhereLocalBinds (collectLocs rhss) locBinds
gTrfAlt' _ _ = convertionProblem "gTrfAlt': not exactly one alternative when transforming a case alternative"

trfCaseRhss :: (TransformName n r, n ~ GhcPass p) => [Located (GRHS n (LHsExpr n))] -> Trf (Ann AST.UCaseRhs (Dom r) RangeStage)
trfCaseRhss = gTrfCaseRhss trfExpr

gTrfCaseRhss :: (TransformName n r, n ~ GhcPass p) => (Located (ge n) -> Trf (Ann ae (Dom r) RangeStage)) -> [Located (GRHS n (Located (ge n)))] -> Trf (Ann (AST.UCaseRhs' ae) (Dom r) RangeStage)
gTrfCaseRhss te [unLoc -> GRHS _ [] body] = annLocNoSema (combineSrcSpans (getLoc body) <$> updateFocus (pure . updateEnd (const $ srcSpanStart $ getLoc body))
                                                                                                        (tokenLocBack AnnRarrow))
                                                 (AST.UUnguardedCaseRhs <$> te body)
gTrfCaseRhss te rhss = annLocNoSema (pure $ collectLocs rhss)
                              (AST.UGuardedCaseRhss <$> trfAnnList ";" (gTrfGuardedCaseRhs' te) rhss)

trfGuardedCaseRhs :: (TransformName n r, n ~ GhcPass p) => Located (GRHS n (LHsExpr n)) -> Trf (Ann AST.UGuardedCaseRhs (Dom r) RangeStage)
trfGuardedCaseRhs = trfLocNoSema trfGuardedCaseRhs'

trfGuardedCaseRhs' :: (TransformName n r, n ~ GhcPass p) => GRHS n (LHsExpr n) -> Trf (AST.UGuardedCaseRhs (Dom r) RangeStage)
trfGuardedCaseRhs' = gTrfGuardedCaseRhs' trfExpr

gTrfGuardedCaseRhs' :: (TransformName n r, n ~ GhcPass p) => (Located (ge n) -> Trf (Ann ae (Dom r) RangeStage)) -> GRHS n (Located (ge n)) -> Trf (AST.UGuardedCaseRhs' ae (Dom r) RangeStage)
gTrfGuardedCaseRhs' te (GRHS _ guards body) = AST.UGuardedCaseRhs <$> trfAnnList " " trfRhsGuard' guards <*> te body

trfCmdTop :: (TransformName n r, n ~ GhcPass p) => Located (HsCmdTop n) -> Trf (Ann AST.UCmd (Dom r) RangeStage)
trfCmdTop (L _ (HsCmdTop _ cmd)) = trfCmd cmd

trfCmd :: (TransformName n r, n ~ GhcPass p) => Located (HsCmd n) -> Trf (Ann AST.UCmd (Dom r) RangeStage)
trfCmd = trfLocNoSema trfCmd'

trfCmd' :: (TransformName n r, n ~ GhcPass p) => HsCmd n -> Trf (AST.UCmd (Dom r) RangeStage)
trfCmd' (HsCmdArrApp _ left right typ dir) = AST.UArrowAppCmd <$> trfExpr left <*> op <*> trfExpr right
  where op = case (typ, dir) of (HsFirstOrderApp, False) -> annLocNoSema (tokenLoc Annrarrowtail) (pure AST.URightAppl)
                                (HsFirstOrderApp, True) -> annLocNoSema (tokenLoc Annlarrowtail) (pure AST.ULeftAppl)
                                (HsHigherOrderApp, False) -> annLocNoSema (tokenLoc AnnRarrowtail) (pure AST.URightHighApp)
                                (HsHigherOrderApp, True) -> annLocNoSema (tokenLoc AnnLarrowtail) (pure AST.ULeftHighApp)
                                                                       -- FIXME: needs a before
trfCmd' (HsCmdArrForm _ expr _ _ cmds) = AST.UArrowFormCmd <$> trfExpr expr <*> makeList " " (before AnnClose) (mapM trfCmdTop cmds)
trfCmd' (HsCmdApp _ cmd expr) = AST.UAppCmd <$> trfCmd cmd <*> trfExpr expr
trfCmd' (HsCmdLam _ (MG _ (unLoc -> [unLoc -> Match _ _ pats (GRHSs _ [unLoc -> GRHS _ [] body] _)]) _))
  = AST.ULambdaCmd <$> (makeNonemptyList " " $ mapM trfPattern pats) <*> trfCmd body
trfCmd' (HsCmdPar _ cmd) = AST.UParenCmd <$> trfCmd cmd
trfCmd' (HsCmdCase _ expr (MG _ (unLoc -> alts) _))
  = AST.UCaseCmd <$> trfExpr expr <*> makeNonemptyIndentedList (mapM (trfLocNoSema (gTrfAlt' trfCmd)) alts)
trfCmd' (HsCmdIf _ _ pred thenExpr elseExpr) = AST.UIfCmd <$> trfExpr pred <*> trfCmd thenExpr <*> trfCmd elseExpr
trfCmd' (HsCmdLet _ (unLoc -> binds) cmd) = addToScope binds (AST.ULetCmd <$> trfLocalBinds AnnLet binds <*> trfCmd cmd)
trfCmd' (HsCmdDo _ (unLoc -> stmts)) = AST.UDoCmd <$> makeNonemptyIndentedList (mapM (trfLocNoSema (gTrfDoStmt' trfCmd)) stmts)
-- | TODO: implement
trfCmd' (HsCmdLam {}) = convertionProblem "trfCmd': cmd lambda not supported yet"
trfCmd' (HsCmdWrap {}) = convertionProblem "trfCmd': cmd wrap not supported yet"

trfText' :: StringLiteral -> Trf (AST.UStringNode (Dom r) RangeStage)
trfText' = pure . AST.UStringNode . unpackFS . sl_fs

trfSourceRange :: (StringLiteral, (Int, Int), (Int, Int)) -> Trf (Ann AST.USourceRange (Dom r) RangeStage)
trfSourceRange (fileName, (startRow, startCol), (endRow, endCol))
  = do fnLoc <- tokenLoc AnnValStr
       tokens <- allTokenLoc AnnVal
       case tokens of
         [srLoc, scLoc, erLoc, ecLoc] -> do
           annLocNoSema (pure (fnLoc `combineSrcSpans` ecLoc))
             (AST.USourceRange <$> annLocNoSema (pure fnLoc) (trfText' fileName)
                               <*> annLocNoSema (pure srLoc) (pure $ AST.Number $ fromIntegral startRow)
                               <*> annLocNoSema (pure scLoc) (pure $ AST.Number $ fromIntegral startCol)
                               <*> annLocNoSema (pure erLoc) (pure $ AST.Number $ fromIntegral endRow)
                               <*> annLocNoSema (pure ecLoc) (pure $ AST.Number $ fromIntegral endCol))
         _ -> convertionProblem "trfSourceRange: tokens not found"
