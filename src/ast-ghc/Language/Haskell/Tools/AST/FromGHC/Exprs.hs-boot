module Language.Haskell.Tools.AST.FromGHC.Exprs where

import Outputable as GHC
import RdrName as GHC
import SrcLoc as GHC
import HsExpr as GHC
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST (Ann(..), Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfExpr :: TransformName n r => Located (HsExpr n) -> Trf (Ann AST.Expr (Dom r) RangeStage)
trfExpr' :: TransformName n r => HsExpr n -> Trf (AST.Expr (Dom r) RangeStage)
trfCmd' :: TransformName n r => HsCmd n -> Trf (AST.Cmd (Dom r) RangeStage)