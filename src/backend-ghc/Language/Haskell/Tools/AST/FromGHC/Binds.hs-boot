module Language.Haskell.Tools.AST.FromGHC.Binds where

import ApiAnnotation (AnnKeywordId)
import HsBinds as GHC (HsLocalBinds)
import HsExpr as GHC (Stmt, LHsExpr)
import Language.Haskell.Tools.AST (Ann, AnnMaybeG, AnnListG, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.FromGHC.Monad (Trf)
import Language.Haskell.Tools.AST.FromGHC.Names (TransformName(..))
import SrcLoc as GHC (Located, SrcSpan)

trfLocalBinds :: TransformName n r => AnnKeywordId -> HsLocalBinds n -> Trf (AnnListG AST.ULocalBind (Dom r) RangeStage)
trfWhereLocalBinds :: TransformName n r => SrcSpan -> HsLocalBinds n -> Trf (AnnMaybeG AST.ULocalBinds (Dom r) RangeStage)
trfRhsGuard :: TransformName n r => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.URhsGuard (Dom r) RangeStage)
trfRhsGuard' :: TransformName n r => Stmt n (LHsExpr n) -> Trf (AST.URhsGuard (Dom r) RangeStage)
