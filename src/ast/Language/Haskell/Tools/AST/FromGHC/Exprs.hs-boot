module Language.Haskell.Tools.AST.FromGHC.Exprs where

import Outputable as GHC
import RdrName as GHC
import SrcLoc as GHC
import HsExpr as GHC
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Exprs as AST

trfExpr :: TransformName n => Located (HsExpr n) -> Trf (Ann AST.Expr (AnnotType n))
trfExpr' :: TransformName n => HsExpr n -> Trf (AST.Expr (AnnotType n))