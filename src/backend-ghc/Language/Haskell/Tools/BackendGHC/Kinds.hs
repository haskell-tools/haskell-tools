{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions that convert the kind-related elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.BackendGHC.Kinds where

import Data.Data

import ApiAnnotation as GHC (AnnKeywordId(..))
import FastString as GHC (unpackFS)
import HsTypes as GHC
import Name as GHC (occNameString, nameOccName, isWiredInName)
import RdrName as GHC (RdrName(..))
import SrcLoc as GHC
import HsExtension (GhcPass)
import Outputable

import Language.Haskell.Tools.AST (Ann, AnnMaybeG, Dom, RangeStage, HasNoSemanticInfo)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC.GHCUtils (GHCName(..))
import Language.Haskell.Tools.BackendGHC.Monad (Trf, transformingPossibleVar)
import Language.Haskell.Tools.BackendGHC.Names (TransformName, trfOperator, trfName)
import {-# SOURCE #-} Language.Haskell.Tools.BackendGHC.Types (trfType')
import Language.Haskell.Tools.BackendGHC.Utils


trfKindSig :: (TransformName n r, Outputable (HsType n), Data (HsType n), n ~ GhcPass p) 
           => Maybe (LHsKind n) -> Trf (AnnMaybeG AST.UKindConstraint (Dom r) RangeStage)
trfKindSig = trfMaybe "" "" trfKindSig'

trfKindSig' :: (TransformName n r, Outputable (HsType n), Data (HsType n), n ~ GhcPass p) 
            => Located (HsKind n) -> Trf (Ann AST.UKindConstraint (Dom r) RangeStage)
trfKindSig' k = annLocNoSema (combineSrcSpans (getLoc k) <$> (tokenBefore (srcSpanStart (getLoc k)) AnnDcolon))
                             (AST.UKindConstraint <$> trfLocNoSema trfKind' k)

trfKind :: (TransformName n r, Outputable (HsType n), Data (HsType n), n ~ GhcPass p) => Located (HsKind n) -> Trf (Ann AST.UKind (Dom r) RangeStage)
trfKind = trfLocNoSema trfKind'

trfKind' :: forall n r p . (TransformName n r, Outputable (HsType n), Data (HsType n), n ~ GhcPass p) => HsKind n -> Trf (AST.UKind (Dom r) RangeStage)
trfKind' = trfKind'' where
  trfKind'' (HsTyVar _ _ (rdrName @n . unLoc -> Exact n))
    | isWiredInName n && occNameString (nameOccName n) == "*"
    = pure AST.UStarKind
    | isWiredInName n && occNameString (nameOccName n) == "#"
    = pure AST.UUnboxKind
  trfKind'' (HsStarTy _ _) = pure AST.UStarKind
  trfKind'' (HsParTy _ kind) = AST.UParenKind <$> trfKind kind
  trfKind'' (HsFunTy _ k1 k2) = AST.UFunKind <$> trfKind k1 <*> trfKind k2
  trfKind'' (HsAppTy _ k1 k2) = AST.UAppKind <$> trfKind k1 <*> trfKind k2
  trfKind'' (HsOpTy _ k1 op k2) = AST.UInfixAppKind <$> trfKind k1 <*> trfOperator @n op <*> trfKind k2
  trfKind'' (HsTyVar _ _ kv) = transformingPossibleVar kv (AST.UVarKind <$> trfName @n kv)
  trfKind'' (HsListTy _ kind) = AST.UListKind <$> trfKind kind
  trfKind'' (HsTupleTy _ _ kinds) = AST.UTupleKind <$> makeList ", " atTheStart (mapM trfKind kinds)
  trfKind'' pt@(HsExplicitListTy {}) = AST.UPromotedKind <$> annContNoSema (trfPromoted' trfKind' pt)
  trfKind'' pt@(HsExplicitTupleTy {}) = AST.UPromotedKind <$> annContNoSema (trfPromoted' trfKind' pt)
  trfKind'' pt@(HsTyLit {}) = AST.UPromotedKind <$> annContNoSema (trfPromoted' trfKind' pt)
  trfKind'' t = AST.UTypeKind <$> annContNoSema (trfType' t)

trfPromoted' :: forall n r a . (TransformName n r, HasNoSemanticInfo (Dom r) a, Outputable (HsType n), Data (HsType n))
                  => (HsType n -> Trf (a (Dom r) RangeStage)) -> HsType n -> Trf (AST.UPromoted a (Dom r) RangeStage)
trfPromoted' _ (HsTyLit _ (HsNumTy _ int)) = pure $ AST.UPromotedInt int
trfPromoted' _ (HsTyLit _ (HsStrTy _ str)) = pure $ AST.UPromotedString (unpackFS str)
trfPromoted' _ (HsTyVar _ _ name) = AST.UPromotedCon <$> trfName @n name
trfPromoted' f (HsExplicitListTy _ _ elems) = AST.UPromotedList <$> between AnnOpenS AnnCloseS (trfAnnList ", " f elems)
trfPromoted' f (HsExplicitTupleTy _ elems) = AST.UPromotedTuple <$> between AnnOpenP AnnCloseP (trfAnnList ", " f elems)
trfPromoted' _ t = unhandledElement "promoted type/kind" t
