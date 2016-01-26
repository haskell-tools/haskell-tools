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
 
trfDoStmt :: Located (Stmt RdrName (LHsExpr RdrName)) -> Trf (Ann AST.Stmt RI)
trfDoStmt = trfLoc $ \case
  BindStmt pat expr _ _ -> AST.BindStmt <$> trfPattern pat <*> trfExpr expr
  BodyStmt expr _ _ _ -> AST.ExprStmt <$> annCont (trfExpr' (unLoc expr))
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
trfListCompStmt other = (:[]) <$> copyAnnot AST.CompStmt (trfDoStmt other)
  
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
  