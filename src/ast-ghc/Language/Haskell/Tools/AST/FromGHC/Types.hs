{-# LANGUAGE LambdaCase
           , ViewPatterns
           , ScopedTypeVariables
           #-}
-- | Functions that convert the type-related elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
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
import Data.List (find)
import Data.Data (Data(..), toConstr)

import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Language.Haskell.Tools.AST.FromGHC.Names
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.TH
import Language.Haskell.Tools.AST.FromGHC.Kinds
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.Literals

import Language.Haskell.Tools.AST as AST

import Debug.Trace

trfType :: TransformName n r => Located (HsType n) -> Trf (Ann AST.UType (Dom r) RangeStage)
trfType typ = do othSplices <- asks typeSplices
                 let RealSrcSpan loce = getLoc typ
                     contSplice = find (\sp -> case getSpliceLoc sp of (RealSrcSpan spLoc) -> spLoc `containsSpan` loce; _ -> False) othSplices
                 case contSplice of Just sp -> let loc = pure $ getSpliceLoc sp
                                                in typeSpliceInserted sp (annLocNoSema loc (AST.UTySplice <$> annLocNoSema loc (trfSplice' sp)))
                                    Nothing -> trfLocNoSema trfType' typ

trfType' :: TransformName n r => HsType n -> Trf (AST.UType (Dom r) RangeStage)
trfType' = trfType'' . cleanHsType where
  trfType'' (HsForAllTy [] typ) = trfType' (unLoc typ)
  trfType'' (HsForAllTy bndrs typ) = AST.UTyForall <$> defineTypeVars (trfBindings bndrs) 
                                                  <*> addToScope bndrs (trfType typ)
  trfType'' (HsQualTy ctx typ) = AST.UTyCtx <$> (fromJust . (^. annMaybe) <$> trfCtx atTheStart ctx) 
                                           <*> trfType typ
  trfType'' (HsTyVar name) = AST.UTyVar <$> transformingPossibleVar name (trfName name)
  trfType'' (HsAppsTy apps) | Just (head, args) <- getAppsTyHead_maybe apps 
    = foldl (\core t -> AST.UTyApp <$> annLocNoSema (pure $ getLoc head `combineSrcSpans` getLoc t) core <*> trfType t) (trfType' (unLoc head)) args
  trfType'' (HsAppTy t1 t2) = AST.UTyApp <$> trfType t1 <*> trfType t2
  trfType'' (HsFunTy t1 t2) = AST.UTyFun <$> trfType t1 <*> trfType t2
  trfType'' (HsListTy typ) = AST.UTyList <$> trfType typ
  trfType'' (HsPArrTy typ) = AST.UTyParArray <$> trfType typ
  trfType'' (HsTupleTy HsBoxedOrConstraintTuple typs) = AST.UTyTuple <$> trfAnnList ", " trfType' typs
  trfType'' (HsTupleTy HsBoxedTuple typs) = AST.UTyTuple <$> trfAnnList ", " trfType' typs
  trfType'' (HsTupleTy HsUnboxedTuple typs) = AST.UTyUnbTuple <$> trfAnnList ", " trfType' typs
  trfType'' (HsOpTy t1 op t2) = AST.UTyInfix <$> trfType t1 <*> trfOperator op <*> trfType t2
  trfType'' (HsParTy typ) = AST.UTyParen <$> trfType typ
  trfType'' (HsKindSig typ kind) = AST.UTyKinded <$> trfType typ <*> trfKind kind
  trfType'' (HsSpliceTy splice _) = AST.UTySplice <$> annContNoSema (trfSplice' splice)
  trfType'' (HsBangTy (HsSrcBang _ SrcUnpack _) typ) = AST.UTyUnpack <$> trfType typ
  trfType'' (HsBangTy (HsSrcBang _ SrcNoUnpack _) typ) = AST.UTyNoUnpack <$> trfType typ
  trfType'' (HsBangTy (HsSrcBang _ _ SrcStrict) typ) = AST.UTyBang <$> trfType typ
  trfType'' (HsBangTy (HsSrcBang _ _ SrcLazy) typ) = AST.UTyLazy <$> trfType typ
  trfType'' pt@(HsExplicitListTy {}) = AST.UTyPromoted <$> annContNoSema (trfPromoted' trfType' pt) 
  trfType'' pt@(HsExplicitTupleTy {}) = AST.UTyPromoted <$> annContNoSema (trfPromoted' trfType' pt) 
  trfType'' pt@(HsTyLit {}) = AST.UTyPromoted <$> annContNoSema (trfPromoted' trfType' pt) 
  trfType'' (HsWildCardTy _) = pure AST.UTyWildcard -- TODO: named wildcards
  trfType'' t = error ("Illegal type: " ++ showSDocUnsafe (ppr t) ++ " (ctor: " ++ show (toConstr t) ++ ")")
  
trfBindings :: TransformName n r => [Located (HsTyVarBndr n)] -> Trf (AnnListG AST.UTyVar (Dom r) RangeStage)
trfBindings vars = trfAnnList "\n" trfTyVar' vars
  
trfTyVar :: TransformName n r => Located (HsTyVarBndr n) -> Trf (Ann AST.UTyVar (Dom r) RangeStage)
trfTyVar = trfLocNoSema trfTyVar' 
  
trfTyVar' :: TransformName n r => HsTyVarBndr n -> Trf (AST.UTyVar (Dom r) RangeStage)
trfTyVar' (UserTyVar name) = AST.UTyVarDecl <$> typeVarTransform (trfName name)
                                           <*> (nothing " " "" atTheEnd)
trfTyVar' (KindedTyVar name kind) = AST.UTyVarDecl <$> typeVarTransform (trfName name) 
                                                  <*> trfKindSig (Just kind)
  
trfCtx :: TransformName n r => Trf SrcLoc -> Located (HsContext n) -> Trf (AnnMaybeG AST.UContext (Dom r) RangeStage)
trfCtx sp (L l []) = nothing " " "" sp
trfCtx _ (L l [L _ (HsParTy t)]) 
  = makeJust <$> annLocNoSema (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                              (AST.UContextMulti <$> trfAnnList ", " trfAssertion' [t])
trfCtx _ (L l [t]) 
  = makeJust <$> annLocNoSema (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                              (AST.UContextOne <$> trfAssertion t)
trfCtx _ (L l ctx) = makeJust <$> annLocNoSema (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                                               (AST.UContextMulti <$> trfAnnList ", " trfAssertion' ctx) 
  
trfAssertion :: TransformName n r => LHsType n -> Trf (Ann AST.UAssertion (Dom r) RangeStage)
trfAssertion = trfLocNoSema trfAssertion'

trfAssertion' :: forall n r . TransformName n r => HsType n -> Trf (AST.UAssertion (Dom r) RangeStage)
trfAssertion' (cleanHsType -> HsParTy t) 
  = trfAssertion' (unLoc t)
trfAssertion' (cleanHsType -> HsOpTy left op right) 
  = AST.UInfixAssert <$> trfType left <*> trfOperator op <*> trfType right
trfAssertion' (cleanHsType -> t) = case cleanHsType base of
   HsTyVar name -> AST.UClassAssert <$> trfName name <*> trfAnnList " " trfType' args
   HsEqTy t1 t2 -> AST.UInfixAssert <$> trfType t1 <*> annLocNoSema (tokenLoc AnnTilde) (trfOperator' typeEq) <*> trfType t2
   HsIParamTy name t -> do loc <- tokenLoc AnnVal
                           AST.UImplicitAssert <$> define (focusOn loc (trfImplicitName name)) <*> trfType t
   t -> error ("Illegal trf assertion: " ++ showSDocUnsafe (ppr t) ++ " (ctor: " ++ show (toConstr t) ++ ")")
  where (args, sp, base) = getArgs t
        getArgs :: HsType n -> ([LHsType n], Maybe SrcSpan, HsType n)
        getArgs (HsAppTy (L l ft) at) = case getArgs ft of (args, sp, base) -> (args++[at], sp <|> Just l, base)
        getArgs t = ([], Nothing, t)

        typeEq :: n 
        typeEq = nameFromId (mkVanillaGlobal (tyConName heqTyCon) (tyConKind heqTyCon))