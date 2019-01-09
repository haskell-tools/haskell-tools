{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.Tools.BackendGHC.Binds where

import ApiAnnotation (AnnKeywordId)
import HsBinds as GHC (HsLocalBinds)
import HsExpr as GHC (Stmt, LHsExpr)
import Language.Haskell.Tools.AST (Ann, AnnMaybeG, AnnListG, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC.Monad (Trf)
import Language.Haskell.Tools.BackendGHC.Names (TransformName(..))
import SrcLoc as GHC (Located, SrcSpan)
import HsExtension (GhcPass)

trfLocalBinds :: (TransformName n r, n ~ GhcPass p)=> AnnKeywordId -> HsLocalBinds n -> Trf (AnnListG AST.ULocalBind (Dom r) RangeStage)
trfWhereLocalBinds :: (TransformName n r, n ~ GhcPass p) => SrcSpan -> HsLocalBinds n -> Trf (AnnMaybeG AST.ULocalBinds (Dom r) RangeStage)
trfRhsGuard :: (TransformName n r, n ~ GhcPass p) => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.URhsGuard (Dom r) RangeStage)
trfRhsGuard' :: (TransformName n r, n ~ GhcPass p) => Stmt n (LHsExpr n) -> Trf (AST.URhsGuard (Dom r) RangeStage)
