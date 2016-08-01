{-# LANGUAGE LambdaCase
           , ViewPatterns
           , TypeFamilies
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

import Language.Haskell.Tools.AST (Ann(..), AnnList(..), AnnMaybe(..), Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST
 
trfDoStmt :: TransformName n r => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.Stmt (Dom r) RangeStage)
trfDoStmt = trfLocNoSema trfDoStmt'

trfDoStmt' :: TransformName n r => Stmt n (Located (HsExpr n)) -> Trf (AST.Stmt' AST.Expr (Dom r) RangeStage)
trfDoStmt' = gTrfDoStmt' trfExpr

gTrfDoStmt' :: TransformName n r => (Located (ge n) -> Trf (Ann ae (Dom r) RangeStage)) -> Stmt n (Located (ge n)) -> Trf (AST.Stmt' ae (Dom r) RangeStage)
gTrfDoStmt' et (BindStmt pat expr _ _ _) = AST.BindStmt <$> trfPattern pat <*> et expr
gTrfDoStmt' et (BodyStmt expr _ _ _) = AST.ExprStmt <$> et expr
gTrfDoStmt' et (LetStmt (unLoc -> binds)) = AST.LetStmt <$> addToScope binds (trfLocalBinds binds)
gTrfDoStmt' et (LastStmt body _ _) = AST.ExprStmt <$> et body
gTrfDoStmt' et (RecStmt { recS_stmts = stmts }) = AST.RecStmt <$> trfAnnList "," (gTrfDoStmt' et) stmts

trfListCompStmts :: TransformName n r => [Located (Stmt n (LHsExpr n))] -> Trf (AnnList AST.ListCompBody (Dom r) RangeStage)
trfListCompStmts [unLoc -> ParStmt blocks _ _ _, unLoc -> (LastStmt {})]
  = nonemptyAnnList
      <$> trfScopedSequence (\(ParStmtBlock stmts _ _) -> 
                                let ann = collectLocs $ getNormalStmts stmts
                                 in annLocNoSema (pure ann) (AST.ListCompBody <$> makeList "," (pure $ srcSpanStart ann) (concat <$> trfScopedSequence trfListCompStmt stmts))
                            ) blocks
trfListCompStmts others 
  = let ann = (collectLocs $ getNormalStmts others)
     in makeList "|" (pure $ srcSpanStart ann)
          ((:[]) <$> annLocNoSema (pure ann)
                                  (AST.ListCompBody <$> makeList "," (pure $ srcSpanStart ann) (concat <$> trfScopedSequence trfListCompStmt others)))

trfListCompStmt :: TransformName n r => Located (Stmt n (LHsExpr n)) -> Trf [Ann AST.CompStmt (Dom r) RangeStage]
trfListCompStmt (L l trst@(TransStmt { trS_stmts = stmts })) 
  = (++) <$> (concat <$> local (\s -> s { contRange = mkSrcSpan (srcSpanStart (contRange s)) (srcSpanEnd (getLoc (last stmts))) }) (trfScopedSequence trfListCompStmt stmts)) 
         <*> ((:[]) <$> extractActualStmt trst)
-- last statement is extracted
trfListCompStmt (unLoc -> LastStmt _ _ _) = pure []
trfListCompStmt other = (:[]) <$> copyAnnot AST.CompStmt (trfDoStmt other)
  
extractActualStmt :: TransformName n r => Stmt n (LHsExpr n) -> Trf (Ann AST.CompStmt (Dom r) RangeStage)
extractActualStmt = \case
  TransStmt { trS_form = ThenForm, trS_using = using, trS_by = by } 
    -> addAnnotation by using (AST.ThenStmt <$> trfExpr using <*> trfMaybe "," "" trfExpr by)
  TransStmt { trS_form = GroupForm, trS_using = using, trS_by = by } 
    -> addAnnotation by using (AST.GroupStmt <$> trfMaybe "," "" trfExpr by <*> (makeJust <$> trfExpr using))
  where addAnnotation by using
          = annLocNoSema (combineSrcSpans (getLoc using) . combineSrcSpans (maybe noSrcSpan getLoc by)
                            <$> tokenLocBack AnnThen)
  
getNormalStmts :: [Located (Stmt n (LHsExpr n))] -> [Located (Stmt n (LHsExpr n))]
getNormalStmts (L _ (LastStmt body _ _) : rest) = getNormalStmts rest
getNormalStmts (stmt : rest) = stmt : getNormalStmts rest 
getNormalStmts [] = []
 
getLastStmt :: [Located (Stmt n (LHsExpr n))] -> Located (HsExpr n)
getLastStmt (L _ (LastStmt body _ _) : rest) = body
getLastStmt (_ : rest) = getLastStmt rest
  