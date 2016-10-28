module Language.Haskell.Tools.AST.FromGHC.Binds where

import Outputable as GHC
import RdrName as GHC
import SrcLoc as GHC
import HsBinds as GHC
import HsExpr as GHC
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.Names
import Language.Haskell.Tools.AST (Ann(..), AnnMaybeG(..), AnnListG(..), Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfLocalBinds :: TransformName n r => HsLocalBinds n -> Trf (AnnListG AST.ULocalBind (Dom r) RangeStage)
trfWhereLocalBinds :: TransformName n r => HsLocalBinds n -> Trf (AnnMaybeG AST.ULocalBinds (Dom r) RangeStage)
trfRhsGuard :: TransformName n r => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.URhsGuard (Dom r) RangeStage)
trfRhsGuard' :: TransformName n r => Stmt n (LHsExpr n) -> Trf (AST.URhsGuard (Dom r) RangeStage)
