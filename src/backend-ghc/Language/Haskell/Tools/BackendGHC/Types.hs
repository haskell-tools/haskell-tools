{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions that convert the type-related elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.BackendGHC.Types where

import ApiAnnotation as GHC (AnnKeywordId(..))
import HsExpr (HsSplice(..))
import HsTypes as GHC
import SrcLoc as GHC
import HsExtension (GhcPass)

import Control.Applicative (Applicative(..), (<$>), Alternative(..))
import Control.Monad.Reader.Class (asks)
import Control.Reference ((^.))
import Data.Function (on)
import Data.List
import Data.Maybe (Maybe(..), fromJust)
import GHC.Stack (HasCallStack)

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC.Kinds (trfKindSig, trfKind, trfPromoted')
import Language.Haskell.Tools.BackendGHC.Monad
import Language.Haskell.Tools.BackendGHC.Names
import {-# SOURCE #-} Language.Haskell.Tools.BackendGHC.TH (trfSplice, trfQuasiQuotation')
import Language.Haskell.Tools.BackendGHC.Utils

trfType :: forall n r p . (TransformName n r, n ~ GhcPass p, HasCallStack) => Located (HsType n) -> Trf (Ann AST.UType (Dom r) RangeStage)
trfType typ | RealSrcSpan loce <- getLoc typ
  = do othSplices <- asks typeSplices
       let contSplice = filter (\sp -> case getLoc sp of (RealSrcSpan spLoc) -> spLoc `containsSpan` loce; _ -> False) othSplices
       case contSplice of [] -> trfLocNoSema trfType' typ
                          _ -> let lsp@(L l sp) = minimumBy (compareSpans `on` getLoc) contSplice
                                in typeSpliceInserted lsp (annLocNoSema (pure l) (AST.UTySplice <$> (trfSplice =<< rdrSplice sp)))
  | otherwise = trfLocNoSema trfType' typ

trfType' :: forall n r p . (TransformName n r, n ~ GhcPass p, HasCallStack) => HsType n -> Trf (AST.UType (Dom r) RangeStage)
trfType' = trfType'' where
  trfType'' :: HsType n -> Trf (AST.UType (Dom r) RangeStage)
  trfType'' (HsForAllTy _ [] typ) = trfType' (unLoc typ)
  trfType'' (HsForAllTy _ bndrs typ) = AST.UTyForall <$> defineTypeVars (trfBindings bndrs)
                                                     <*> addToScope bndrs (trfType typ)
  trfType'' (HsQualTy _ (L _ []) typ) = trfType' (unLoc typ)
  trfType'' (HsQualTy _ ctx typ) = AST.UTyCtx <$> (fromJust . (^. annMaybe) <$> trfCtx atTheStart ctx)
                                              <*> trfType typ
  trfType'' (HsTyVar _ _ name) = AST.UTyVar <$> transformingPossibleVar name (trfName @n name)
  trfType'' (HsAppTy _ t1 t2) = AST.UTyApp <$> trfType t1 <*> trfType t2
  trfType'' (HsFunTy _ t1 t2) = AST.UTyFun <$> trfType t1 <*> trfType t2
  trfType'' (HsListTy _ typ) = AST.UTyList <$> trfType typ
  -- trfType'' (HsPArrTy _ typ) = AST.UTyParArray <$> trfType typ
  trfType'' (HsTupleTy _ HsBoxedOrConstraintTuple typs) = AST.UTyTuple <$> trfAnnList ", " trfType' typs
  trfType'' (HsTupleTy _ HsBoxedTuple typs) = AST.UTyTuple <$> trfAnnList ", " trfType' typs
  trfType'' (HsTupleTy _ HsUnboxedTuple typs) = AST.UTyUnbTuple <$> trfAnnList ", " trfType' typs
  trfType'' (HsOpTy _ t1 op t2) = AST.UTyInfix <$> trfType t1 <*> trfOperator @n op <*> trfType t2
  trfType'' (HsParTy _ typ) = AST.UTyParen <$> trfType typ
  trfType'' (HsKindSig _ typ kind) = AST.UTyKinded <$> trfType typ <*> trfKind kind
  trfType'' (HsSpliceTy _ qq@(HsQuasiQuote {})) = AST.UTyQuasiQuote <$> annContNoSema (trfQuasiQuotation' qq)
  trfType'' (HsSpliceTy _ splice) = AST.UTySplice <$> trfSplice splice
  trfType'' (HsBangTy _ (HsSrcBang _ SrcUnpack _) typ) = AST.UTyUnpack <$> trfType typ
  trfType'' (HsBangTy _ (HsSrcBang _ SrcNoUnpack _) typ) = AST.UTyNoUnpack <$> trfType typ
  trfType'' (HsBangTy _ (HsSrcBang _ _ SrcStrict) typ) = AST.UTyBang <$> trfType typ
  trfType'' (HsBangTy _ (HsSrcBang _ _ SrcLazy) typ) = AST.UTyLazy <$> trfType typ
  trfType'' pt@(HsExplicitListTy {}) = AST.UTyPromoted <$> annContNoSema (trfPromoted' trfType' pt)
  trfType'' pt@(HsExplicitTupleTy {}) = AST.UTyPromoted <$> annContNoSema (trfPromoted' trfType' pt)
  trfType'' pt@(HsTyLit {}) = AST.UTyPromoted <$> annContNoSema (trfPromoted' trfType' pt)
  trfType'' (HsWildCardTy _) = pure AST.UTyWildcard -- TODO: named wildcards
  trfType'' (HsSumTy _ types) = AST.UUnbSumType <$> trfAnnList " | " trfType' types
  trfType'' t = unhandledElement "type" t

trfBindings :: (TransformName n r, n ~ GhcPass p, HasCallStack) => [Located (HsTyVarBndr n)] -> Trf (AnnListG AST.UTyVar (Dom r) RangeStage)
trfBindings [] = makeList " " atTheStart (pure [])
trfBindings vars = trfAnnList " " trfTyVar' vars

trfTyVar :: (TransformName n r, n ~ GhcPass p, HasCallStack) => Located (HsTyVarBndr n) -> Trf (Ann AST.UTyVar (Dom r) RangeStage)
trfTyVar = trfLocNoSema trfTyVar'

trfTyVar' :: forall n r p . (TransformName n r, n ~ GhcPass p, HasCallStack) => HsTyVarBndr n -> Trf (AST.UTyVar (Dom r) RangeStage)
trfTyVar' (UserTyVar _ name) = AST.UTyVarDecl <$> typeVarTransform (trfName @n name)
                                              <*> (nothing " " "" atTheEnd)
trfTyVar' (KindedTyVar _ name kind) = AST.UTyVarDecl <$> typeVarTransform (trfName @n name)
                                                     <*> trfKindSig (Just kind)

trfCtx :: (TransformName n r, n ~ GhcPass p, HasCallStack) => Trf SrcLoc -> Located (HsContext n) -> Trf (AnnMaybeG AST.UContext (Dom r) RangeStage)
trfCtx sp (L _ []) = nothing " " "" sp
trfCtx _ (L l [L _ (HsParTy _ t)])
  = makeJust <$> annLocNoSema (combineSrcSpans l <$> tokenLoc AnnDarrow)
                              (AST.UContext <$> annLocNoSema (pure l) (AST.UTupleAssert <$> (trfAnnList ", " trfAssertion' [t])))
trfCtx _ (L l [t])
  = makeJust <$> annLocNoSema (combineSrcSpans l <$> tokenLoc AnnDarrow)
                              (AST.UContext <$> trfAssertion t)
trfCtx _ (L l ctx) = makeJust <$> annLocNoSema (combineSrcSpans l <$> tokenLoc AnnDarrow)
                                               (AST.UContext <$> annLocNoSema (pure l) (AST.UTupleAssert <$> (trfAnnList ", " trfAssertion' ctx)))

trfAssertion :: (TransformName n r, n ~ GhcPass p, HasCallStack) => LHsType n -> Trf (Ann AST.UAssertion (Dom r) RangeStage)
trfAssertion = trfLocNoSema trfAssertion'

trfAssertion' :: forall n r p . (TransformName n r, n ~ GhcPass p, HasCallStack) => HsType n -> Trf (AST.UAssertion (Dom r) RangeStage)
trfAssertion' (HsParTy _ t)
  = trfAssertion' (unLoc t)
trfAssertion' (HsOpTy _ left op right)
  = AST.UInfixAssert <$> trfType left <*> trfOperator @n op <*> trfType right
trfAssertion' (HsTupleTy _ _ tys)
  = AST.UTupleAssert <$> makeList ", " (after AnnOpenP) (mapM trfAssertion tys)
trfAssertion' (HsWildCardTy _)
  = pure AST.UWildcardAssert
trfAssertion' t = case base of
   HsTyVar _ _ name -> AST.UClassAssert <$> trfName @n name <*> trfAnnList " " trfType' args
   -- HsEqTy t1 t2 -> AST.UInfixAssert <$> trfType t1 <*> annLocNoSema (tokenLoc AnnTilde) (trfOperator' @n typeEq) <*> trfType t2
   HsIParamTy _ name t -> AST.UImplicitAssert <$> define (focusOn (getLoc name) (trfImplicitName (unLoc name))) <*> trfType t
   t -> unhandledElement "assertion" t
  where (args, _, base) = getArgs t

        getArgs :: HsType n -> ([LHsType n], Maybe SrcSpan, HsType n)
        getArgs (HsAppTy _ (L l ft) at) = case getArgs ft of (args, sp, base) -> (args++[at], sp <|> Just l, base)
        getArgs t = ([], Nothing, t)

        -- typeEq :: IdP n
        -- typeEq = nameFromId @n (mkVanillaGlobal (tyConName heqTyCon) (tyConKind heqTyCon))
