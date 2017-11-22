{-# LANGUAGE LambdaCase, TypeFamilies, ViewPatterns #-}
-- | Functions that convert the statement-related elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.BackendGHC.Stmts where

import Control.Monad.Reader (MonadReader(..))

import ApiAnnotation as GHC (AnnKeywordId(..))
import HsExpr as GHC
import Outputable (Outputable)
import SrcLoc as GHC

import Language.Haskell.Tools.AST (Ann, AnnListG, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST
import {-# SOURCE #-} Language.Haskell.Tools.BackendGHC.Binds (trfLocalBinds)
import {-# SOURCE #-} Language.Haskell.Tools.BackendGHC.Exprs (trfExpr)
import Language.Haskell.Tools.BackendGHC.Monad (TrfInput(..), Trf, addToScope)
import Language.Haskell.Tools.BackendGHC.Names (TransformName(..))
import Language.Haskell.Tools.BackendGHC.Patterns (trfPattern)
import Language.Haskell.Tools.BackendGHC.Utils

import Data.Data (Data)

trfDoStmt :: TransformName n r => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.UStmt (Dom r) RangeStage)
trfDoStmt = trfLocNoSema trfDoStmt'

trfDoStmt' :: TransformName n r => Stmt n (Located (HsExpr n)) -> Trf (AST.UStmt' AST.UExpr (Dom r) RangeStage)
trfDoStmt' = gTrfDoStmt' trfExpr

gTrfDoStmt' :: (TransformName n r, Data (ge n), Outputable (ge n))
            => (Located (ge n) -> Trf (Ann ae (Dom r) RangeStage)) -> Stmt n (Located (ge n)) -> Trf (AST.UStmt' ae (Dom r) RangeStage)
gTrfDoStmt' et (BindStmt pat expr _ _ _) = AST.UBindStmt <$> trfPattern pat <*> et expr
gTrfDoStmt' et (BodyStmt expr _ _ _) = AST.UExprStmt <$> et expr
gTrfDoStmt' _ (LetStmt (unLoc -> binds)) = AST.ULetStmt . orderAnnList <$> addToScope binds (trfLocalBinds AnnLet binds)
gTrfDoStmt' et (LastStmt body _ _) = AST.UExprStmt <$> et body
gTrfDoStmt' et (RecStmt { recS_stmts = stmts }) = AST.URecStmt <$> trfAnnList "," (gTrfDoStmt' et) stmts
gTrfDoStmt' _ stmt = unhandledElement "simple statement" stmt

trfListCompStmts :: TransformName n r => [Located (Stmt n (LHsExpr n))] -> Trf (AnnListG AST.UListCompBody (Dom r) RangeStage)
trfListCompStmts [unLoc -> ParStmt blocks _ _ _, unLoc -> (LastStmt {})]
  = nonemptyAnnList
      <$> trfScopedSequence (\(ParStmtBlock stmts _ _) ->
                                let ann = collectLocs $ getNormalStmts stmts
                                 in annLocNoSema (pure ann) (AST.UListCompBody <$> makeList "," (pure $ srcSpanStart ann) (concat <$> trfScopedSequence trfListCompStmt stmts))
                            ) blocks
trfListCompStmts others
  = let ann = (collectLocs $ getNormalStmts others)
     in makeList "|" (pure $ srcSpanStart ann)
          ((:[]) <$> annLocNoSema (pure ann)
                                  (AST.UListCompBody <$> makeList "," (pure $ srcSpanStart ann) (concat <$> trfScopedSequence trfListCompStmt others)))

trfListCompStmt :: TransformName n r => Located (Stmt n (LHsExpr n)) -> Trf [Ann AST.UCompStmt (Dom r) RangeStage]
trfListCompStmt (L _ trst@(TransStmt { trS_stmts = stmts }))
  = (++) <$> (concat <$> local (\s -> s { contRange = mkSrcSpan (srcSpanStart (contRange s)) (srcSpanEnd (getLoc (last stmts))) }) (trfScopedSequence trfListCompStmt stmts))
         <*> ((:[]) <$> extractActualStmt trst)
-- last statement is extracted
trfListCompStmt (unLoc -> LastStmt _ _ _) = pure []
trfListCompStmt other = (:[]) <$> copyAnnot AST.UCompStmt (trfDoStmt other)

extractActualStmt :: TransformName n r => Stmt n (LHsExpr n) -> Trf (Ann AST.UCompStmt (Dom r) RangeStage)
extractActualStmt = \case
  TransStmt { trS_form = ThenForm, trS_using = using, trS_by = by }
    -> addAnnotation by using (AST.UThenStmt <$> trfExpr using <*> trfMaybe "," "" trfExpr by)
  TransStmt { trS_form = GroupForm, trS_using = using, trS_by = by }
    -> addAnnotation by using (AST.UGroupStmt <$> trfMaybe "," "" trfExpr by <*> (makeJust <$> trfExpr using))
  _ -> convertionProblem "extractActualStmt: called on a statement that is not then or group"
  where addAnnotation by using
          = annLocNoSema (combineSrcSpans (getLoc using) . combineSrcSpans (maybe noSrcSpan getLoc by)
                            <$> tokenLocBack AnnThen)

getNormalStmts :: [Located (Stmt n (LHsExpr n))] -> [Located (Stmt n (LHsExpr n))]
getNormalStmts (L _ (LastStmt _ _ _) : rest) = getNormalStmts rest
getNormalStmts (stmt : rest) = stmt : getNormalStmts rest
getNormalStmts [] = []

getLastStmt :: [Located (Stmt n (LHsExpr n))] -> Located (HsExpr n)
getLastStmt (L _ (LastStmt body _ _) : _) = body
getLastStmt (_ : rest) = getLastStmt rest
getLastStmt [] = convProblem "getLastStmt: empty"
