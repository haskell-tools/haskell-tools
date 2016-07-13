module Language.Haskell.Tools.AST.FromGHC.Binds where

import Outputable as GHC
import RdrName as GHC
import SrcLoc as GHC
import HsBinds as GHC
import HsExpr as GHC
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST (Ann(..), AnnMaybe(..), AnnList(..), Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfLocalBinds :: TransformName n r => HsLocalBinds n -> Trf (AnnList AST.LocalBind (Dom r) RangeStage)
trfWhereLocalBinds :: TransformName n r => HsLocalBinds n -> Trf (AnnMaybe AST.LocalBinds (Dom r) RangeStage)
trfRhsGuard :: TransformName n r => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.RhsGuard (Dom r) RangeStage)
trfRhsGuard' :: TransformName n r => Stmt n (LHsExpr n) -> Trf (AST.RhsGuard (Dom r) RangeStage)
