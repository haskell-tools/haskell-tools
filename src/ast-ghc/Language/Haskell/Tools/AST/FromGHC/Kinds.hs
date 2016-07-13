{-# LANGUAGE ViewPatterns
           , TypeFamilies 
           #-}
module Language.Haskell.Tools.AST.FromGHC.Kinds where

import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import OccName as GHC
import Name as GHC
import ApiAnnotation as GHC
import Outputable as GHC
import FastString as GHC

import Control.Monad.Reader
import Data.Data (toConstr)

import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Language.Haskell.Tools.AST.FromGHC.Literals
import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST (Ann(..), AnnMaybe(..), Dom, RangeStage, SemanticInfo, NoSemanticInfo)
import qualified Language.Haskell.Tools.AST as AST

import Debug.Trace

trfKindSig :: TransformName n r => Maybe (LHsKind n) -> Trf (AnnMaybe AST.KindConstraint (Dom r) RangeStage)
trfKindSig = trfMaybe "" "" trfKindSig'

trfKindSig' :: TransformName n r => Located (HsKind n) -> Trf (Ann AST.KindConstraint (Dom r) RangeStage)
trfKindSig' k = annLocNoSema (combineSrcSpans (getLoc k) <$> (tokenBefore (srcSpanStart (getLoc k)) AnnDcolon)) 
                             (AST.KindConstraint <$> trfLocNoSema trfKind' k)

trfKind :: TransformName n r => Located (HsKind n) -> Trf (Ann AST.Kind (Dom r) RangeStage)
trfKind = trfLocNoSema (trfKind' . cleanHsType)

trfKind' ::TransformName n r => HsKind n -> Trf (AST.Kind (Dom r) RangeStage)
trfKind' = trfKind'' . cleanHsType where
  trfKind'' (HsTyVar (rdrName . unLoc -> Exact n)) 
    | isWiredInName n && occNameString (nameOccName n) == "*"
    = pure AST.KindStar
    | isWiredInName n && occNameString (nameOccName n) == "#"
    = pure AST.KindUnbox
  trfKind'' (HsParTy kind) = AST.KindParen <$> trfKind kind
  trfKind'' (HsFunTy k1 k2) = AST.KindFn <$> trfKind k1 <*> trfKind k2
  trfKind'' (HsAppTy k1 k2) = AST.KindApp <$> trfKind k1 <*> trfKind k2
  trfKind'' (HsTyVar kv) = transformingPossibleVar kv (AST.KindVar <$> trfName kv)
  trfKind'' (HsListTy kind) = AST.KindList <$> trfKind kind
  trfKind'' (HsAppsTy [unLoc -> HsAppPrefix t]) = trfKind' (unLoc t)
  trfKind'' (HsAppsTy [unLoc -> HsAppInfix n]) = AST.KindVar <$> trfName n
  trfKind'' pt@(HsExplicitListTy {}) = AST.KindPromoted <$> annContNoSema (trfPromoted' trfKind' pt) 
  trfKind'' pt@(HsExplicitTupleTy {}) = AST.KindPromoted <$> annContNoSema (trfPromoted' trfKind' pt) 
  trfKind'' pt@(HsTyLit {}) = AST.KindPromoted <$> annContNoSema (trfPromoted' trfKind' pt) 
  trfKind'' k = error ("Illegal kind: " ++ showSDocUnsafe (ppr k) ++ " (ctor: " ++ show (toConstr k) ++ ")")

trfPromoted' :: (TransformName n r, SemanticInfo (Dom r) a ~ NoSemanticInfo) 
                  => (HsType n -> Trf (a (Dom r) RangeStage)) -> HsType n -> Trf (AST.Promoted a (Dom r) RangeStage)
trfPromoted' f (HsTyLit (HsNumTy _ int)) = pure $ AST.PromotedInt int
trfPromoted' f (HsTyLit (HsStrTy _ str)) = pure $ AST.PromotedString (unpackFS str)
trfPromoted' f (HsTyVar name) = AST.PromotedCon <$> trfName name
trfPromoted' f (HsExplicitListTy _ elems) = AST.PromotedList <$> between AnnOpenS AnnCloseS (trfAnnList ", " f elems)
trfPromoted' f (HsExplicitTupleTy _ elems) = AST.PromotedTuple <$> between AnnOpenP AnnCloseP (trfAnnList ", " f elems)
trfPromoted' _ t = asks contRange >>= \r -> error $ "Unknown promoted type/kind: " ++ (showSDocUnsafe (ppr t) ++ " at: " ++ show r)