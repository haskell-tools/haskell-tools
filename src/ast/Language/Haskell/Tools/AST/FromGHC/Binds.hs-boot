module Language.Haskell.Tools.AST.FromGHC.Binds where

import Outputable as GHC
import RdrName as GHC
import SrcLoc as GHC
import HsBinds as GHC
import HsExpr as GHC
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Binds as AST

trfLocalBinds :: HsLocalBinds RdrName -> Trf (AnnList AST.LocalBind RI)
trfWhereLocalBinds :: HsLocalBinds RdrName -> Trf (AnnMaybe AST.LocalBinds RI)
trfRhsGuard :: Located (Stmt RdrName (LHsExpr RdrName)) -> Trf (Ann AST.RhsGuard RI)
