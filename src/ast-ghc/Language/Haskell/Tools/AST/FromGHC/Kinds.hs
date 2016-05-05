{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.Tools.AST.FromGHC.Kinds where

import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import OccName as GHC
import Name as GHC
import ApiAnnotation as GHC

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST (Ann(..), AnnMaybe(..))
import qualified Language.Haskell.Tools.AST as AST

trfKindSig :: TransformName n r => Maybe (LHsKind n) -> Trf (AnnMaybe AST.KindConstraint r)
trfKindSig = trfMaybe "" "" (\k -> annLoc (combineSrcSpans (getLoc k) <$> (tokenLoc AnnDcolon)) 
                                         (fmap AST.KindConstraint $ trfLoc trfKind' k))

trfKindSig' :: TransformName n r => Located (HsKind n) -> Trf (Ann AST.KindConstraint r)
trfKindSig' k = annLoc (combineSrcSpans (getLoc k) <$> (tokenLoc AnnDcolon)) 
                       (AST.KindConstraint <$> trfLoc trfKind' k)

trfKind :: TransformName n r => Located (HsKind n) -> Trf (Ann AST.Kind r)
trfKind = trfLoc trfKind'

trfKind' :: TransformName n r => HsKind n -> Trf (AST.Kind r)
trfKind' (HsTyVar (rdrName -> Exact n)) 
  | isWiredInName n && occNameString (nameOccName n) == "*"
  = pure AST.KindStar
  | isWiredInName n && occNameString (nameOccName n) == "#"
  = pure AST.KindUnbox
trfKind' (HsParTy kind) = AST.KindParen <$> trfKind kind
trfKind' (HsFunTy k1 k2) = AST.KindFn <$> trfKind k1 <*> trfKind k2
trfKind' (HsAppTy k1 k2) = AST.KindApp <$> trfKind k1 <*> trfKind k2
trfKind' (HsTyVar kv) = AST.KindVar <$> annCont (define (trfName' kv))
trfKind' (HsExplicitTupleTy _ kinds) = AST.KindTuple <$> trfAnnList "," trfKind' kinds
trfKind' (HsExplicitListTy _ kinds) = AST.KindList <$> trfAnnList "," trfKind' kinds
  