{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Tools.AST.FromGHC.Types where
 
import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import ApiAnnotation as GHC
import FastString as GHC

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.TH
import Language.Haskell.Tools.AST.FromGHC.Kinds
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Types as AST

trfType :: Located (HsType RdrName) -> Trf (Ann AST.Type RI)
trfType = trfLoc trfType'

trfType' :: HsType RdrName -> Trf (AST.Type RI)
trfType' (HsForAllTy _ _ bndrs ctx typ) = AST.TyForall <$> trfBindings (hsq_tvs bndrs) 
                                                       <*> trfCtx ctx <*> trfType typ
trfType' (HsTyVar name) = AST.TyVar <$> annCont (trfName' name)
trfType' (HsAppTy t1 t2) = AST.TyApp <$> trfType t1 <*> trfType t2
trfType' (HsFunTy t1 t2) = AST.TyFun <$> trfType t1 <*> trfType t2
trfType' (HsListTy typ) = AST.TyList <$> trfType typ
trfType' (HsPArrTy typ) = AST.TyParArray <$> trfType typ
-- HsBoxedOrConstraintTuple?
trfType' (HsTupleTy HsBoxedTuple typs) = AST.TyTuple . AnnList <$> mapM trfType typs
trfType' (HsTupleTy HsUnboxedTuple typs) = AST.TyUnbTuple . AnnList <$> mapM trfType typs
trfType' (HsOpTy t1 op t2) = AST.TyInfix <$> trfType t1 <*> trfName (snd op) <*> trfType t2
trfType' (HsParTy typ) = AST.TyParen <$> trfType typ
trfType' (HsKindSig typ kind) = AST.TyKinded <$> trfType typ <*> trfKind kind
trfType' (HsQuasiQuoteTy qq) = AST.TyQuasiQuote <$> trfQuasiQuotation' qq
trfType' (HsSpliceTy splice _) = AST.TySplice <$> trfSplice' splice
trfType' (HsBangTy _ typ) = AST.TyBang <$> trfType typ
-- HsRecTy
-- HsCoreTy
trfType' (HsTyLit (HsNumTy _ int)) = pure $ AST.TyNumLit int
trfType' (HsTyLit (HsStrTy _ str)) = pure $ AST.TyStrLit (unpackFS str)
trfType' (HsWrapTy _ typ) = trfType' typ
trfType' HsWildcardTy = pure AST.TyWildcard
-- not implemented as ghc 7.10.3
trfType' (HsNamedWildcardTy name) = AST.TyNamedWildc <$> annCont (trfName' name)
  
trfBindings :: [Located (HsTyVarBndr RdrName)] -> Trf (AnnList AST.TyVar RI)
trfBindings vars = AnnList <$> mapM trfTyVar vars
  
trfTyVar :: Located (HsTyVarBndr RdrName) -> Trf (Ann AST.TyVar RI)
trfTyVar var@(L l _) = trfLoc (\case
  UserTyVar name -> AST.TyVarDecl <$> annLoc (pure l) (trfName' name) <*> pure annNothing
  KindedTyVar name kind -> AST.TyVarDecl <$> trfName name <*> trfKindSig (Just kind)) var
  
trfCtx :: Located (HsContext RdrName) -> Trf (AnnMaybe AST.Context RI)
trfCtx (L l []) = pure annNothing
trfCtx (L l [L _ (HsParTy t)]) 
  = annJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                       (AST.ContextMulti . AnnList . (:[]) <$> trfAssertion t)
trfCtx (L l [t]) 
  = annJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                       (AST.ContextOne <$> trfAssertion t)
trfCtx (L l ctx) = annJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                                      (AST.ContextMulti . AnnList <$> mapM trfAssertion ctx) 
  
trfAssertion :: LHsType RdrName -> Trf (Ann AST.Assertion RI)
trfAssertion t = annLoc (pure $ getLoc t) $ case base of 
  L l (HsTyVar name) -> AST.ClassAssert <$> annLoc (pure l) (trfName' name) 
                                        <*> (AnnList <$> mapM trfType args)
  L l (HsOpTy left op right) -> AST.InfixAssert <$> trfType left <*> trfName (snd op) <*> trfType right
  where (args, base) = getArgs t
        getArgs :: LHsType RdrName -> ([LHsType RdrName], LHsType RdrName)
        getArgs (L l (HsAppTy ft at)) = case getArgs ft of (args, base) -> (args++[at], base)
        getArgs t = ([], t)
  