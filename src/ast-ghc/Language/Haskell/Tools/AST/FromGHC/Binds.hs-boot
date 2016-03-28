module Language.Haskell.Tools.AST.FromGHC.Binds where

import Outputable as GHC
import RdrName as GHC
import SrcLoc as GHC
import HsBinds as GHC
import HsExpr as GHC
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Binds as AST

trfLocalBinds :: TransformName n r => HsLocalBinds n -> Trf (AnnList AST.LocalBind r)
trfWhereLocalBinds :: TransformName n r => HsLocalBinds n -> Trf (AnnMaybe AST.LocalBinds r)
trfRhsGuard :: TransformName n r => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.RhsGuard r)
trfRhsGuard' :: TransformName n r => Stmt n (LHsExpr n) -> Trf (AST.RhsGuard r)
