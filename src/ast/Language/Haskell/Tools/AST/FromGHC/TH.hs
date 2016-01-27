module Language.Haskell.Tools.AST.FromGHC.TH where

import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import HsExpr as GHC

import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.Base

import qualified Language.Haskell.Tools.AST.TH as AST

trfQuasiQuotation' :: TransformName n => HsQuasiQuote n -> Trf (AST.QuasiQuote (AnnotType n))
trfQuasiQuotation' = undefined

trfSplice' :: TransformName n => HsSplice n -> Trf (AST.Splice (AnnotType n))
trfSplice' = undefined

trfBracket' :: TransformName n => HsBracket n -> Trf (AST.Bracket (AnnotType n))
trfBracket' = undefined
  