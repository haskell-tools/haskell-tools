module Language.Haskell.Tools.BackendGHC.Exprs where

import HsExpr as GHC (HsExpr, HsCmd)
import Language.Haskell.Tools.AST (Ann, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC.Monad (Trf)
import Language.Haskell.Tools.BackendGHC.Names (TransformName(..))
import SrcLoc as GHC (Located)

trfExpr :: TransformName n r => Located (HsExpr n) -> Trf (Ann AST.UExpr (Dom r) RangeStage)
trfExpr' :: TransformName n r => HsExpr n -> Trf (AST.UExpr (Dom r) RangeStage)
trfCmd' :: TransformName n r => HsCmd n -> Trf (AST.UCmd (Dom r) RangeStage)