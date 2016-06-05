{-# LANGUAGE LambdaCase
           , ViewPatterns
           , ScopedTypeVariables
           #-}
module Language.Haskell.Tools.AST.FromGHC.Types where
 
import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import ApiAnnotation as GHC
import FastString as GHC
import Type as GHC
import TyCon as GHC
import Outputable as GHC
import TysWiredIn (heqTyCon)
import Id (mkVanillaGlobal)

import Control.Monad.Reader.Class
import Control.Applicative
import Control.Reference
import Data.Maybe

import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Language.Haskell.Tools.AST.FromGHC.Base
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.TH
import Language.Haskell.Tools.AST.FromGHC.Kinds
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.Literals

import Language.Haskell.Tools.AST as AST

trfType :: TransformName n r => Located (HsType n) -> Trf (Ann AST.Type r)
trfType = trfLoc trfType'

trfType' :: TransformName n r => HsType n -> Trf (AST.Type r)
trfType' (HsForAllTy [] typ) = trfType' (unLoc typ)
trfType' (HsForAllTy bndrs typ) = AST.TyForall <$> define (trfBindings bndrs) 
                                               <*> addToScope bndrs (trfType typ)
trfType' (HsQualTy ctx typ) = AST.TyCtx <$> (fromJust . (^. annMaybe) <$> trfCtx atTheStart ctx) 
                                        <*> trfType typ
trfType' (HsTyVar name) = AST.TyVar <$> define (trfName name)
trfType' (HsAppsTy [unLoc -> HsAppPrefix t]) = trfType' (unLoc t)
trfType' (HsAppsTy [unLoc -> HsAppInfix n]) = AST.TyVar <$> trfName n
trfType' (HsAppTy t1 t2) = AST.TyApp <$> trfType t1 <*> trfType t2
trfType' (HsFunTy t1 t2) = AST.TyFun <$> trfType t1 <*> trfType t2
trfType' (HsListTy typ) = AST.TyList <$> trfType typ
trfType' (HsPArrTy typ) = AST.TyParArray <$> trfType typ
trfType' (HsTupleTy HsBoxedOrConstraintTuple typs) = AST.TyTuple <$> trfAnnList ", " trfType' typs
trfType' (HsTupleTy HsBoxedTuple typs) = AST.TyTuple <$> trfAnnList ", " trfType' typs
trfType' (HsTupleTy HsConstraintTuple typs) = error "HsTupleTy HsConstraintTuple"
trfType' (HsTupleTy HsUnboxedTuple typs) = AST.TyUnbTuple <$> trfAnnList ", " trfType' typs
trfType' (HsOpTy t1 op t2) = AST.TyInfix <$> trfType t1 <*> trfOperator op <*> trfType t2
trfType' (HsParTy typ) = AST.TyParen <$> trfType typ
trfType' (HsIParamTy _ _) = error "HsIParamType"
trfType' (HsEqTy _ _) = error "HsEqTy"
trfType' (HsKindSig typ kind) = AST.TyKinded <$> trfType typ <*> trfKind kind
trfType' (HsSpliceTy splice _) = AST.TySplice <$> trfSplice' splice
trfType' (HsDocTy _ _) = error "HsDocTy"
trfType' (HsBangTy _ typ) = AST.TyBang <$> trfType typ
trfType' (HsRecTy _) = error "HsRecTy"
trfType' (HsCoreTy _) = error "HsCoreTy"
trfType' pt@(HsExplicitListTy {}) = AST.TyPromoted <$> annCont (trfPromoted' trfType' pt) 
trfType' pt@(HsExplicitTupleTy {}) = AST.TyPromoted <$> annCont (trfPromoted' trfType' pt) 
trfType' pt@(HsTyLit {}) = AST.TyPromoted <$> annCont (trfPromoted' trfType' pt) 
-- HsRecTy only appears as part of GADT constructor declarations, so it is omitted
trfType' (HsWildCardTy _) = pure AST.TyWildcard
--trfType' (HsNamedWildCardTy name) = AST.TyNamedWildc <$> annCont (define (trfName' name))
-- must be a promoted type
  
trfBindings :: TransformName n r => [Located (HsTyVarBndr n)] -> Trf (AnnList AST.TyVar r)
trfBindings vars = trfAnnList "\n" trfTyVar' vars
  
trfTyVar :: TransformName n r => Located (HsTyVarBndr n) -> Trf (Ann AST.TyVar r)
trfTyVar = trfLoc trfTyVar' 
  
trfTyVar' :: TransformName n r => HsTyVarBndr n -> Trf (AST.TyVar r)
trfTyVar' (UserTyVar name) = AST.TyVarDecl <$> trfName name
                                           <*> (nothing " " "" atTheEnd)
trfTyVar' (KindedTyVar name kind) = AST.TyVarDecl <$> trfName name <*> trfKindSig (Just kind)
  
trfCtx :: TransformName n r => Trf SrcLoc -> Located (HsContext n) -> Trf (AnnMaybe AST.Context r)
trfCtx sp (L l []) = nothing " " "" sp
trfCtx _ (L l [L _ (HsParTy t)]) 
  = makeJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                        (AST.ContextMulti <$> trfAnnList ", " trfAssertion' [t])
trfCtx _ (L l [t]) 
  = makeJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                        (AST.ContextOne <$> trfAssertion t)
trfCtx _ (L l ctx) = makeJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                                         (AST.ContextMulti <$> trfAnnList ", " trfAssertion' ctx) 
  
trfAssertion :: TransformName n r => LHsType n -> Trf (Ann AST.Assertion r)
trfAssertion = trfLoc trfAssertion'

trfAssertion' :: forall n r . TransformName n r => HsType n -> Trf (AST.Assertion r)
trfAssertion' (HsOpTy left op right) 
  = AST.InfixAssert <$> trfType left <*> trfOperator op <*> trfType right
trfAssertion' t = case base of
   HsTyVar name -> AST.ClassAssert <$> trfName name <*> trfAnnList " " trfType' args
   HsEqTy t1 t2 -> AST.InfixAssert <$> trfType t1 <*> annLoc (tokenLoc AnnTilde) (trfOperator' typeEq) <*> trfType t2
  where (args, sp, base) = getArgs t
        getArgs :: HsType n -> ([LHsType n], Maybe SrcSpan, HsType n)
        getArgs (HsAppTy (L l ft) at) = case getArgs ft of (args, sp, base) -> (args++[at], sp <|> Just l, base)
        getArgs t = ([], Nothing, t)

        typeEq :: n 
        typeEq = nameFromId (mkVanillaGlobal (tyConName heqTyCon) (tyConKind heqTyCon))