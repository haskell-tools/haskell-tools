module Language.Haskell.Tools.AST.FromGHC.TH where

import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import HsExpr as GHC

import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import qualified Language.Haskell.Tools.AST.TH as AST

trfQuasiQuotation' :: HsQuasiQuote RdrName -> Trf (AST.QuasiQuote RI)
trfQuasiQuotation' = undefined

trfSplice' :: HsSplice RdrName -> Trf (AST.Splice RI)
trfSplice' = undefined

trfBracket' :: HsBracket RdrName -> Trf (AST.Bracket RI)
trfBracket' = undefined
  