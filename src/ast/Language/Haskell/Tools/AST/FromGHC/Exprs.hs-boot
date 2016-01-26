module Language.Haskell.Tools.AST.FromGHC.Exprs where

import Outputable as GHC
import RdrName as GHC
import SrcLoc as GHC
import HsExpr as GHC
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Exprs as AST

trfExpr' :: HsExpr RdrName -> Trf (AST.Expr RI)
trfExpr :: Located (HsExpr RdrName) -> Trf (Ann AST.Expr RI)