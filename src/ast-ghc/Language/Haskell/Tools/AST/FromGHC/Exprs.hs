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

import Language.Haskell.Tools.AST (Ann(..), AnnList(..))
import qualified Language.Haskell.Tools.AST as AST

trfExpr :: TransformName n r => Located (HsExpr n) -> Trf (Ann AST.Expr r)
trfExpr = trfLoc trfExpr'

trfExpr' :: TransformName n r => HsExpr n -> Trf (AST.Expr r)
trfExpr' (HsVar name) = AST.Var <$> trfNameSp' name
trfExpr' (HsIPVar (HsIPName ip)) = AST.Var <$> annCont (AST.nameFromList . fst <$> trfNameStr (unpackFS ip))
trfExpr' (HsOverLit (ol_val -> val)) = AST.Lit <$> annCont (trfOverloadedLit val)
trfExpr' (HsLit val) = AST.Lit <$> annCont (trfLiteral' val)
trfExpr' (HsLam (mg_alts -> [unLoc -> Match _ pats _ (GRHSs [unLoc -> GRHS [] expr] EmptyLocalBinds)]))
  = AST.Lambda <$> (trfAnnList " " trfPattern' pats) <*> trfExpr expr
trfExpr' (HsLamCase _ (mg_alts -> matches)) = AST.LamCase <$> trfAnnList " " trfAlt' matches
trfExpr' (HsApp e1 e2) = AST.App <$> trfExpr e1 <*> trfExpr e2
trfExpr' (OpApp e1 (L opLoc (HsVar op)) _ e2) 
  = AST.InfixApp <$> trfExpr e1 <*> trfNameSp op opLoc <*> trfExpr e2
trfExpr' (NegApp e _) = AST.PrefixApp <$> (annLoc (mkSrcSpan <$> (srcSpanStart <$> asks contRange) 
                                                             <*> (pure $ srcSpanStart (getLoc e))) 
                                                  (AST.nameFromList . fst <$> trfNameStr "-")) 
                                      <*> trfExpr e
trfExpr' (HsPar expr) = AST.Paren <$> trfExpr expr
trfExpr' (SectionL expr (L l (HsVar op))) = AST.LeftSection <$> trfExpr expr <*> trfNameSp op l
trfExpr' (SectionR (L l (HsVar op)) expr) = AST.RightSection <$> trfNameSp op l <*> trfExpr expr
trfExpr' (ExplicitTuple tupArgs box) | all tupArgPresent tupArgs 
  = wrap <$> between AnnOpenP AnnCloseP (trfAnnList ", " (trfExpr' . unLoc . (\(Present e) -> e)) tupArgs)
  where wrap = if box == Boxed then AST.Tuple else AST.UnboxedTuple
trfExpr' (ExplicitTuple tupArgs box)
  = wrap <$> between AnnOpenP AnnCloseP
               (trfAnnList ", " (\case (Present e) -> AST.Present <$> annCont (trfExpr' (unLoc e))
                                       (Missing _) -> pure AST.Missing
                                ) tupArgs)
  where wrap = if box == Boxed then AST.TupleSection else AST.UnboxedTupSec
trfExpr' (HsCase expr (mg_alts -> cases)) = AST.Case <$> trfExpr expr <*> (trfAnnList "; " trfAlt' cases)
trfExpr' (HsIf _ expr thenE elseE) = AST.If <$> trfExpr expr <*> trfExpr thenE <*> trfExpr elseE
trfExpr' (HsMultiIf _ parts) = AST.MultiIf <$> trfAnnList "" trfGuardedCaseRhs' parts
trfExpr' (HsLet binds expr) = AST.Let <$> trfLocalBinds binds <*> trfExpr expr
trfExpr' (HsDo DoExpr stmts _) = AST.Do <$> annLoc (tokenLoc AnnDo) (pure AST.DoKeyword) 
                                        <*> trfAnnList "; " trfDoStmt' stmts
trfExpr' (HsDo MDoExpr stmts _) = AST.Do <$> annLoc (tokenLoc AnnMdo) (pure AST.MDoKeyword)
                                         <*> trfAnnList "; " trfDoStmt' stmts
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
trfExpr' (HsSpliceE _ splice) = AST.Splice <$> annCont (trfSplice' splice)
trfExpr' (HsQuasiQuoteE qq) = AST.QuasiQuoteExpr <$> annCont (trfQuasiQuotation' qq)
-- TODO: arrows
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
  HsRecField id _ True -> AST.FieldPun <$> trfNameSp' (unLoc id)
  HsRecField id val False -> AST.NormalFieldUpdate <$> trfName id <*> trfExpr val
  
trfAlt :: TransformName n r => Located (Match n (LHsExpr n)) -> Trf (Ann AST.Alt r)
trfAlt = trfLoc trfAlt'

trfAlt' :: TransformName n r => Match n (LHsExpr n) -> Trf (AST.Alt r)
trfAlt' (Match _ [pat] typ (GRHSs rhss locBinds))
  = AST.Alt <$> trfPattern pat <*> trfCaseRhss rhss <*> trfWhereLocalBinds locBinds
  
trfCaseRhss :: TransformName n r => [Located (GRHS n (LHsExpr n))] -> Trf (Ann AST.CaseRhs r)
trfCaseRhss [unLoc -> GRHS [] body] = annLoc (combineSrcSpans (getLoc body) <$> tokenLoc AnnEqual) 
                                             (AST.UnguardedCaseRhs <$> trfExpr body)
trfCaseRhss rhss = annLoc (pure $ collectLocs rhss) 
                          (AST.GuardedCaseRhss <$> trfAnnList ";" trfGuardedCaseRhs' rhss)
  
trfGuardedCaseRhs :: TransformName n r => Located (GRHS n (LHsExpr n)) -> Trf (Ann AST.GuardedCaseRhs r)
trfGuardedCaseRhs = trfLoc trfGuardedCaseRhs' 

trfGuardedCaseRhs' :: TransformName n r => GRHS n (LHsExpr n) -> Trf (AST.GuardedCaseRhs r)
trfGuardedCaseRhs' (GRHS guards body) = AST.GuardedCaseRhs <$> trfAnnList " " trfRhsGuard' guards <*> trfExpr body