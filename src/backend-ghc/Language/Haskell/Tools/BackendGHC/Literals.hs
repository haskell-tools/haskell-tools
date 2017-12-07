-- | Functions that convert the literals of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.BackendGHC.Literals where

import qualified Data.ByteString.Char8 as BS (foldr)

import BasicTypes as GHC (FractionalLit(..))
import FastString as GHC (unpackFS)
import HsLit as GHC (OverLitVal(..), HsLit(..))
import Type as GHC
import PrelNames as GHC
import TysWiredIn as GHC
import TysPrim as GHC

import Language.Haskell.Tools.AST (Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST (ULiteral(..))
import Language.Haskell.Tools.BackendGHC.Monad (Trf)
import Language.Haskell.Tools.BackendGHC.Utils

trfLiteral' :: HsLit -> Trf (AST.ULiteral (Dom r) RangeStage)
trfLiteral' (HsChar _ ch) = pure $ AST.UCharLit ch
trfLiteral' (HsCharPrim _ ch) = pure $ AST.UPrimCharLit ch
trfLiteral' (HsString _ str) = pure $ AST.UStringLit (unpackFS str)
trfLiteral' (HsStringPrim _ str) = pure $ AST.UPrimStringLit (BS.foldr (:) "" str)
trfLiteral' (HsIntPrim _ i) = pure $ AST.UPrimIntLit i
trfLiteral' (HsWordPrim _ i) = pure $ AST.UPrimWordLit i
trfLiteral' (HsInt64Prim _ i) = pure $ AST.UPrimIntLit i
trfLiteral' (HsWord64Prim _ i) = pure $ AST.UPrimWordLit i
trfLiteral' (HsFloatPrim frac) = pure $ AST.UPrimFloatLit (fl_value frac)
trfLiteral' (HsDoublePrim frac) = pure $ AST.UPrimDoubleLit (fl_value frac)
trfLiteral' l = unhandledElement "literal" l

monoLiteralType :: HsLit -> Type
monoLiteralType (HsChar _ _) = charTy
monoLiteralType (HsCharPrim _ _) = charPrimTy
monoLiteralType (HsString _ _) = stringTy
monoLiteralType (HsStringPrim _ _) = addrPrimTy
monoLiteralType (HsIntPrim _ _) = intPrimTy
monoLiteralType (HsWordPrim _ _) = wordTy
monoLiteralType (HsInt64Prim _ _) = int64PrimTy
monoLiteralType (HsWord64Prim _ _) = word64PrimTy
monoLiteralType (HsFloatPrim _) = floatX4PrimTy
monoLiteralType (HsDoublePrim _) = doubleX2PrimTy

trfOverloadedLit :: OverLitVal -> Trf (AST.ULiteral (Dom r) RangeStage)
trfOverloadedLit (HsIntegral _ i) = pure $ AST.UIntLit i
trfOverloadedLit (HsFractional frac) = pure $ AST.UFracLit (fl_value frac)
trfOverloadedLit (HsIsString _ str) = pure $ AST.UStringLit (unpackFS str)
