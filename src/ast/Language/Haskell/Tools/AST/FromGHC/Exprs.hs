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
import Language.Haskell.Tools.AST.FromGHC.TH
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Base as AST
import qualified Language.Haskell.Tools.AST.Exprs as AST

trfExpr :: TransformName n => Located (HsExpr n) -> Trf (Ann AST.Expr (AnnotType n))
trfExpr = trfLoc trfExpr'

trfExpr' :: TransformName n => HsExpr n -> Trf (AST.Expr (AnnotType n))
trfExpr' (HsVar name) = AST.Var <$> annCont (trfName' name)
trfExpr' (HsIPVar (HsIPName ip)) = AST.Var <$> annCont (AST.nameFromList . fst <$> trfNameStr (unpackFS ip))
trfExpr' (HsOverLit (ol_val -> val)) = AST.Lit <$> annCont (trfOverloadedLit val)
trfExpr' (HsLit val) = AST.Lit <$> annCont (trfLiteral' val)
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
  = wrap . AnnList <$> mapM (trfLoc $ (\case (Present e) -> AST.Present <$> annCont (trfExpr' (unLoc e))
                                             (Missing _) -> pure AST.Missing
                                       )) tupArgs 
  where wrap = if box == Boxed then AST.TupleSection else AST.UnboxedTupSec
trfExpr' (HsCase expr (mg_alts -> cases)) = AST.Case <$> trfExpr expr <*> (AnnList <$> mapM trfAlt cases)
trfExpr' (HsIf _ expr thenE elseE) = AST.If <$> trfExpr expr <*> trfExpr thenE <*> trfExpr elseE
trfExpr' (HsMultiIf _ parts) = AST.MultiIf . AnnList <$> mapM trfGuardedCaseRhs parts
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
trfExpr' (HsBracket brack) = AST.BracketExpr <$> annCont (trfBracket' brack)
trfExpr' (HsSpliceE _ splice) = AST.Splice <$> annCont (trfSplice' splice)
trfExpr' (HsQuasiQuoteE qq) = AST.QuasiQuoteExpr <$> annCont (trfQuasiQuotation' qq)
-- TODO: arrows
-- TODO: static

trfFieldUpdates :: TransformName n => HsRecordBinds n -> Trf (AnnList AST.FieldUpdate (AnnotType n))
trfFieldUpdates (HsRecFields fields dotdot) 
  = AnnList 
      <$> ((++) <$> mapM trfFieldUpdate fields 
                <*> (if isJust dotdot then (:[]) <$> annLoc (tokenLoc AnnDotdot) (pure AST.FieldWildcard) 
                                      else pure []) )
  
trfFieldUpdate :: TransformName n => Located (HsRecField n (LHsExpr n)) -> Trf (Ann AST.FieldUpdate (AnnotType n))
trfFieldUpdate = trfLoc $ \case
  HsRecField id _ True -> AST.FieldPun <$> annCont (trfName' (unLoc id))
  HsRecField id val False -> AST.NormalFieldUpdate <$> trfName id <*> trfExpr val
  
trfAlt :: TransformName n => Located (Match n (LHsExpr n)) -> Trf (Ann AST.Alt (AnnotType n))
trfAlt = trfLoc $ \(Match _ [pat] typ (GRHSs rhss locBinds))
  -> AST.Alt <$> trfPattern pat <*> trfCaseRhss rhss <*> trfWhereLocalBinds locBinds

trfCaseRhss :: TransformName n => [Located (GRHS n (LHsExpr n))] -> Trf (Ann AST.CaseRhs (AnnotType n))
trfCaseRhss [unLoc -> GRHS [] body] = annLoc (combineSrcSpans (getLoc body) <$> tokenLoc AnnEqual) 
                                             (AST.UnguardedCaseRhs <$> trfExpr body)
trfCaseRhss rhss = annLoc (pure $ collectLocs rhss) 
                          (AST.GuardedCaseRhss . AnnList <$> mapM trfGuardedCaseRhs rhss)
  
trfGuardedCaseRhs :: TransformName n => Located (GRHS n (LHsExpr n)) -> Trf (Ann AST.GuardedCaseRhs (AnnotType n))
trfGuardedCaseRhs = trfLoc $ \(GRHS guards body) 
  -> AST.GuardedCaseRhs . AnnList <$> mapM trfRhsGuard guards <*> trfExpr body
            