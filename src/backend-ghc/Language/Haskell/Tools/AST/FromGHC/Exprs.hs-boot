module Language.Haskell.Tools.AST.FromGHC.Exprs where

import SrcLoc as GHC
import HsExpr as GHC
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Names
import Language.Haskell.Tools.AST (Ann(..), Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfExpr :: TransformName n r => Located (HsExpr n) -> Trf (Ann AST.UExpr (Dom r) RangeStage)
trfExpr' :: TransformName n r => HsExpr n -> Trf (AST.UExpr (Dom r) RangeStage)
trfCmd' :: TransformName n r => HsCmd n -> Trf (AST.UCmd (Dom r) RangeStage)