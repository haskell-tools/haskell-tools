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
import TysWiredIn (eqTyCon)
import Id (mkVanillaGlobal)

import Control.Monad.Reader.Class
import Control.Applicative
import Control.Reference
import Data.Maybe

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
trfType' (HsForAllTy Implicit _ _ (unLoc -> []) typ) = trfType' (unLoc typ)
trfType' (HsForAllTy Implicit _ _ ctx typ) = AST.TyCtx <$> (fromJust . (^. annMaybe) <$> trfCtx atTheStart ctx) 
                                                       <*> trfType typ
trfType' (HsForAllTy _ _ bndrs ctx typ) = AST.TyForall <$> define (trfBindings (hsq_tvs bndrs)) 
                                                       <*> trfCtx (after AnnDot) ctx
                                                       <*> addToScope bndrs (trfType typ)
trfType' (HsTyVar name) = AST.TyVar <$> annCont (define (trfName' name))
trfType' (HsAppTy t1 t2) = AST.TyApp <$> trfType t1 <*> trfType t2
trfType' (HsFunTy t1 t2) = AST.TyFun <$> trfType t1 <*> trfType t2
trfType' (HsListTy typ) = AST.TyList <$> trfType typ
trfType' (HsPArrTy typ) = AST.TyParArray <$> trfType typ
trfType' (HsTupleTy HsBoxedOrConstraintTuple typs) = AST.TyTuple <$> trfAnnList ", " trfType' typs
trfType' (HsTupleTy HsBoxedTuple typs) = AST.TyTuple <$> trfAnnList ", " trfType' typs
trfType' (HsTupleTy HsUnboxedTuple typs) = AST.TyUnbTuple <$> trfAnnList ", " trfType' typs
trfType' (HsOpTy t1 op t2) = AST.TyInfix <$> trfType t1 <*> trfOperator (snd op) <*> trfType t2
trfType' (HsParTy typ) = AST.TyParen <$> trfType typ
trfType' (HsKindSig typ kind) = AST.TyKinded <$> trfType typ <*> trfKind kind
trfType' (HsQuasiQuoteTy qq) = AST.TyQuasiQuote <$> trfQuasiQuotation' qq
trfType' (HsSpliceTy splice _) = AST.TySplice <$> trfSplice' splice
trfType' (HsBangTy _ typ) = AST.TyBang <$> trfType typ
-- HsRecTy only appears as part of GADT constructor declarations, so it is omitted
trfType' (HsWrapTy _ typ) = trfType' typ
trfType' HsWildcardTy = pure AST.TyWildcard
-- not implemented as ghc 7.10.3
trfType' (HsNamedWildcardTy name) = AST.TyNamedWildc <$> annCont (define (trfName' name))
-- must be a promoted type
trfType' t = AST.TyPromoted <$> annCont (trfPromoted' trfType' t) 
  
trfBindings :: TransformName n r => [Located (HsTyVarBndr n)] -> Trf (AnnList AST.TyVar r)
trfBindings vars = trfAnnList "\n" trfTyVar' vars
  
trfTyVar :: TransformName n r => Located (HsTyVarBndr n) -> Trf (Ann AST.TyVar r)
trfTyVar = trfLoc trfTyVar' 
  
trfTyVar' :: TransformName n r => HsTyVarBndr n -> Trf (AST.TyVar r)
trfTyVar' (UserTyVar name) = AST.TyVarDecl <$> annCont (trfName' name)
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
trfAssertion' (HsOpTy left op right) = AST.InfixAssert <$> trfType left 
                                                       <*> trfOperator (snd op) 
                                                       <*> trfType right
trfAssertion' t = case base of
   HsTyVar name -> AST.ClassAssert <$> (annLoc (case sp of Just l -> pure l; _ -> asks contRange) $ trfName' name)
                                   <*> trfAnnList " " trfType' args
   HsEqTy t1 t2 -> AST.InfixAssert <$> trfType t1 <*> annLoc (tokenLoc AnnTilde) (trfOperator' typeEq) <*> trfType t2
  where (args, sp, base) = getArgs t
        getArgs :: HsType n -> ([LHsType n], Maybe SrcSpan, HsType n)
        getArgs (HsAppTy (L l ft) at) = case getArgs ft of (args, sp, base) -> (args++[at], sp <|> Just l, base)
        getArgs t = ([], Nothing, t)

        typeEq :: n 
        typeEq = nameFromId (mkVanillaGlobal (tyConName eqTyCon) (tyConKind eqTyCon))