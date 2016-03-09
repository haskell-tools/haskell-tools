{-# LANGUAGE LambdaCase
           , ViewPatterns
           #-}
module Language.Haskell.Tools.AST.FromGHC.Stmts where
 
import Data.Maybe
import Control.Monad.Reader

import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import HsPat as GHC
import HsExpr as GHC
import ApiAnnotation as GHC

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Types
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.Exprs
import Language.Haskell.Tools.AST.FromGHC.Patterns
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.Binds
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Stmts as AST
 
trfDoStmt :: TransformName n r => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.Stmt r)
trfDoStmt = trfLoc trfDoStmt'

trfDoStmt' :: TransformName n r => Stmt n (LHsExpr n) -> Trf (AST.Stmt r)
trfDoStmt' (BindStmt pat expr _ _) = AST.BindStmt <$> trfPattern pat <*> trfExpr expr
trfDoStmt' (BodyStmt expr _ _ _) = AST.ExprStmt <$> annCont (trfExpr' (unLoc expr))
trfDoStmt' (LetStmt binds) = AST.LetStmt <$> trfLocalBinds binds
trfDoStmt' (RecStmt { recS_stmts = stmts }) = AST.RecStmt <$> trfAnnList trfDoStmt' stmts

trfListCompStmts :: TransformName n r => [Located (Stmt n (LHsExpr n))] -> Trf (AnnList AST.ListCompBody r)
trfListCompStmts [unLoc -> ParStmt blocks _ _, unLoc -> (LastStmt {})]
  = nonemptyAnnList
      <$> mapM (\(ParStmtBlock stmts _ _) -> 
                   let ann = toNodeAnnot $ collectLocs $ getNormalStmts stmts
                    in Ann ann . AST.ListCompBody . AnnList ann . concat 
                         <$> mapM trfListCompStmt stmts
               ) blocks
trfListCompStmts others 
  = let ann = (collectLocs $ getNormalStmts others)
     in AnnList (toNodeAnnot ann) . (:[]) 
          <$> annLoc (pure ann)
                     (AST.ListCompBody . AnnList (toNodeAnnot ann) . concat <$> mapM trfListCompStmt others) 

trfListCompStmt :: TransformName n r => Located (Stmt n (LHsExpr n)) -> Trf [Ann AST.CompStmt r]
trfListCompStmt (L l trst@(TransStmt { trS_stmts = stmts })) 
  = (++) <$> (concat <$> local (\s -> s { contRange = mkSrcSpan (srcSpanStart (contRange s)) (srcSpanEnd (getLoc (last stmts))) }) (mapM trfListCompStmt stmts)) 
         <*> ((:[]) <$> extractActualStmt trst)
-- last statement is extracted
trfListCompStmt (unLoc -> LastStmt _ _) = pure []
trfListCompStmt other = (:[]) <$> copyAnnot AST.CompStmt (trfDoStmt other)
  
extractActualStmt :: TransformName n r => Stmt n (LHsExpr n) -> Trf (Ann AST.CompStmt r)
extractActualStmt = \case
  TransStmt { trS_form = ThenForm, trS_using = using, trS_by = by } 
    -> addAnnotation by using (AST.ThenStmt <$> trfExpr using <*> trfMaybe trfExpr by)
  TransStmt { trS_form = GroupForm, trS_using = using, trS_by = by } 
    -> addAnnotation by using (AST.GroupStmt <$> (makeJust <$> trfExpr using) <*> trfMaybe trfExpr by)
  where addAnnotation by using
          = annLoc (combineSrcSpans (getLoc using) . combineSrcSpans (maybe noSrcSpan getLoc by)
                      <$> tokenLocBack AnnThen)
  
getNormalStmts :: [Located (Stmt n (LHsExpr n))] -> [Located (Stmt n (LHsExpr n))]
getNormalStmts (L _ (LastStmt body _) : rest) = getNormalStmts rest
getNormalStmts (stmt : rest) = stmt : getNormalStmts rest 
getNormalStmts [] = []
 
getLastStmt :: [Located (Stmt n (LHsExpr n))] -> Located (HsExpr n)
getLastStmt (L _ (LastStmt body _) : rest) = body
getLastStmt (_ : rest) = getLastStmt rest
  