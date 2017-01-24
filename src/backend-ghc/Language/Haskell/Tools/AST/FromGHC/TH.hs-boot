module Language.Haskell.Tools.AST.FromGHC.TH where

import HsExpr as GHC (HsSplice, HsBracket)
import Language.Haskell.Tools.AST (Ann, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.FromGHC.Monad (Trf)
import Language.Haskell.Tools.AST.FromGHC.Names (TransformName(..))
import SrcLoc as GHC (Located)

trfQuasiQuotation' :: TransformName n r => HsSplice n -> Trf (AST.UQuasiQuote (Dom r) RangeStage)
trfSplice :: TransformName n r => Located (HsSplice n) -> Trf (Ann AST.USplice (Dom r) RangeStage)
trfSplice' :: TransformName n r => HsSplice n -> Trf (AST.USplice (Dom r) RangeStage)
trfBracket' :: TransformName n r => HsBracket n -> Trf (AST.UBracket (Dom r) RangeStage)
