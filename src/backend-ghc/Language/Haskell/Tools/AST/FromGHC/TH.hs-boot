module Language.Haskell.Tools.AST.FromGHC.TH where

import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import HsExpr as GHC
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.Names
import Language.Haskell.Tools.AST (Ann(..), Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfQuasiQuotation' :: TransformName n r => HsSplice n -> Trf (AST.UQuasiQuote (Dom r) RangeStage)
trfSplice :: TransformName n r => Located (HsSplice n) -> Trf (Ann AST.USplice (Dom r) RangeStage)
trfSplice' :: TransformName n r => HsSplice n -> Trf (AST.USplice (Dom r) RangeStage)
trfBracket' :: TransformName n r => HsBracket n -> Trf (AST.UBracket (Dom r) RangeStage)
