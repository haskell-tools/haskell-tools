module Language.Haskell.Tools.AST.FromGHC.Literals where

import qualified Data.ByteString.Char8 as BS

import SrcLoc as GHC
import ApiAnnotation as GHC
import FastString as GHC
import BasicTypes as GHC
import HsLit as GHC
import HsTypes as GHC

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST (Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfLiteral' :: HsLit -> Trf (AST.Literal (Dom r) RangeStage)
trfLiteral' (HsChar _ ch) = pure $ AST.CharLit ch
trfLiteral' (HsCharPrim _ ch) = pure $ AST.PrimCharLit ch
trfLiteral' (HsString _ str) = pure $ AST.StringLit (unpackFS str)
trfLiteral' (HsStringPrim _ str) = pure $ AST.PrimStringLit (BS.foldr (:) "" str)
trfLiteral' (HsInt _ i) = pure $ AST.IntLit i
trfLiteral' (HsIntPrim _ i) = pure $ AST.PrimIntLit i
trfLiteral' (HsWordPrim _ i) = pure $ AST.PrimWordLit i
trfLiteral' (HsInt64Prim _ i) = pure $ AST.PrimIntLit i
trfLiteral' (HsWord64Prim _ i) = pure $ AST.PrimWordLit i
trfLiteral' (HsInteger _ i _) = pure $ AST.PrimIntLit i
trfLiteral' (HsRat frac _) = pure $ AST.FracLit (fl_value frac)
trfLiteral' (HsFloatPrim frac) = pure $ AST.PrimFloatLit (fl_value frac)
trfLiteral' (HsDoublePrim frac) = pure $ AST.PrimDoubleLit (fl_value frac)
  
trfOverloadedLit :: OverLitVal -> Trf (AST.Literal (Dom r) RangeStage)
trfOverloadedLit (HsIntegral _ i) = pure $ AST.IntLit i
trfOverloadedLit (HsFractional frac) = pure $ AST.FracLit (fl_value frac)
trfOverloadedLit (HsIsString _ str) = pure $ AST.StringLit (unpackFS str)