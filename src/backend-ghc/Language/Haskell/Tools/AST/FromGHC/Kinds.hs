{-# LANGUAGE ViewPatterns
           , TypeFamilies 
           #-}
-- | Functions that convert the kind-related elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.AST.FromGHC.Kinds where

import ApiAnnotation as GHC (AnnKeywordId(..))
import FastString as GHC (unpackFS)
import HsTypes as GHC
import Name as GHC (occNameString, nameOccName, isWiredInName)
import OccName as GHC (occNameString)
import Outputable as GHC (Outputable(..), showSDocUnsafe)
import RdrName as GHC (RdrName(..))
import SrcLoc as GHC

import Control.Monad.Reader (Monad(..), asks)
import Data.Data (toConstr)

import Language.Haskell.Tools.AST (Ann, AnnMaybeG, Dom, RangeStage, HasNoSemanticInfo)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.FromGHC.GHCUtils (GHCName(..), cleanHsType)
import Language.Haskell.Tools.AST.FromGHC.Monad (TrfInput(..), Trf, transformingPossibleVar)
import Language.Haskell.Tools.AST.FromGHC.Names (TransformName(..), trfName)
import Language.Haskell.Tools.AST.FromGHC.Utils


trfKindSig :: TransformName n r => Maybe (LHsKind n) -> Trf (AnnMaybeG AST.UKindConstraint (Dom r) RangeStage)
trfKindSig = trfMaybe "" "" trfKindSig'

trfKindSig' :: TransformName n r => Located (HsKind n) -> Trf (Ann AST.UKindConstraint (Dom r) RangeStage)
trfKindSig' k = annLocNoSema (combineSrcSpans (getLoc k) <$> (tokenBefore (srcSpanStart (getLoc k)) AnnDcolon)) 
                             (AST.UKindConstraint <$> trfLocNoSema trfKind' k)

trfKind :: TransformName n r => Located (HsKind n) -> Trf (Ann AST.UKind (Dom r) RangeStage)
trfKind = trfLocNoSema (trfKind' . cleanHsType)

trfKind' ::TransformName n r => HsKind n -> Trf (AST.UKind (Dom r) RangeStage)
trfKind' = trfKind'' . cleanHsType where
  trfKind'' (HsTyVar (rdrName . unLoc -> Exact n)) 
    | isWiredInName n && occNameString (nameOccName n) == "*"
    = pure AST.UStarKind
    | isWiredInName n && occNameString (nameOccName n) == "#"
    = pure AST.UUnboxKind
  trfKind'' (HsParTy kind) = AST.UParenKind <$> trfKind kind
  trfKind'' (HsFunTy k1 k2) = AST.UFunKind <$> trfKind k1 <*> trfKind k2
  trfKind'' (HsAppTy k1 k2) = AST.UAppKind <$> trfKind k1 <*> trfKind k2
  trfKind'' (HsTyVar kv) = transformingPossibleVar kv (AST.UVarKind <$> trfName kv)
  trfKind'' (HsListTy kind) = AST.UListKind <$> trfKind kind
  trfKind'' (HsAppsTy [unLoc -> HsAppPrefix t]) = trfKind' (unLoc t)
  trfKind'' (HsAppsTy [unLoc -> HsAppInfix n]) = AST.UVarKind <$> trfName n
  trfKind'' pt@(HsExplicitListTy {}) = AST.UPromotedKind <$> annContNoSema (trfPromoted' trfKind' pt) 
  trfKind'' pt@(HsExplicitTupleTy {}) = AST.UPromotedKind <$> annContNoSema (trfPromoted' trfKind' pt) 
  trfKind'' pt@(HsTyLit {}) = AST.UPromotedKind <$> annContNoSema (trfPromoted' trfKind' pt) 
  trfKind'' k = error ("Illegal kind: " ++ showSDocUnsafe (ppr k) ++ " (ctor: " ++ show (toConstr k) ++ ")")

trfPromoted' :: (TransformName n r, HasNoSemanticInfo (Dom r) a) 
                  => (HsType n -> Trf (a (Dom r) RangeStage)) -> HsType n -> Trf (AST.UPromoted a (Dom r) RangeStage)
trfPromoted' _ (HsTyLit (HsNumTy _ int)) = pure $ AST.UPromotedInt int
trfPromoted' _ (HsTyLit (HsStrTy _ str)) = pure $ AST.UPromotedString (unpackFS str)
trfPromoted' _ (HsTyVar name) = AST.UPromotedCon <$> trfName name
trfPromoted' f (HsExplicitListTy _ elems) = AST.UPromotedList <$> between AnnOpenS AnnCloseS (trfAnnList ", " f elems)
trfPromoted' f (HsExplicitTupleTy _ elems) = AST.UPromotedTuple <$> between AnnOpenP AnnCloseP (trfAnnList ", " f elems)
trfPromoted' _ t = asks contRange >>= \r -> error $ "Unknown promoted type/kind: " ++ (showSDocUnsafe (ppr t) ++ " at: " ++ show r)