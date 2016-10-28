-- | Functions that convert the literals of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.AST.FromGHC.Literals where

import qualified Data.ByteString.Char8 as BS

import SrcLoc as GHC
import ApiAnnotation as GHC
import FastString as GHC
import BasicTypes as GHC
import HsLit as GHC
import HsTypes as GHC

import Language.Haskell.Tools.AST.FromGHC.Names
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST (Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfLiteral' :: HsLit -> Trf (AST.ULiteral (Dom r) RangeStage)
trfLiteral' (HsChar _ ch) = pure $ AST.UCharLit ch
trfLiteral' (HsCharPrim _ ch) = pure $ AST.UPrimCharLit ch
trfLiteral' (HsString _ str) = pure $ AST.UStringLit (unpackFS str)
trfLiteral' (HsStringPrim _ str) = pure $ AST.UPrimStringLit (BS.foldr (:) "" str)
trfLiteral' (HsInt _ i) = pure $ AST.UIntLit i
trfLiteral' (HsIntPrim _ i) = pure $ AST.UPrimIntLit i
trfLiteral' (HsWordPrim _ i) = pure $ AST.UPrimWordLit i
trfLiteral' (HsInt64Prim _ i) = pure $ AST.UPrimIntLit i
trfLiteral' (HsWord64Prim _ i) = pure $ AST.UPrimWordLit i
trfLiteral' (HsInteger _ i _) = pure $ AST.UPrimIntLit i
trfLiteral' (HsRat frac _) = pure $ AST.UFracLit (fl_value frac)
trfLiteral' (HsFloatPrim frac) = pure $ AST.UPrimFloatLit (fl_value frac)
trfLiteral' (HsDoublePrim frac) = pure $ AST.UPrimDoubleLit (fl_value frac)
  
trfOverloadedLit :: OverLitVal -> Trf (AST.ULiteral (Dom r) RangeStage)
trfOverloadedLit (HsIntegral _ i) = pure $ AST.UIntLit i
trfOverloadedLit (HsFractional frac) = pure $ AST.UFracLit (fl_value frac)
trfOverloadedLit (HsIsString _ str) = pure $ AST.UStringLit (unpackFS str)