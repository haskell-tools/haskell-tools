{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Tools.AST.FromGHC.Decl where

import RdrName as GHC
import HsSyn as GHC
import SrcLoc as GHC
import HsDecls as GHC
import Name as GHC
import OccName as GHC
import ApiAnnotation as GHC
import FastString as GHC

import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Decl as AST

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

trfDecls :: [LHsDecl RdrName] -> Trf (AnnList AST.Decl RI)
trfDecls decls = AnnList <$> mapM trfDecl decls

trfDecl :: Located (HsDecl RdrName) -> Trf (Ann AST.Decl RI)
trfDecl = trfLoc $ \case
  TyClD (FamDecl (FamilyDecl DataFamily name tyVars kindSig)) 
    -> AST.DataFamilyDecl <$> createDeclHead name tyVars <*> trfKindSig kindSig
  TyClD (FamDecl (FamilyDecl OpenTypeFamily name tyVars kindSig)) 
    -> AST.TypeFamilyDecl <$> createDeclHead name tyVars <*> trfKindSig kindSig
  TyClD (FamDecl (FamilyDecl (ClosedTypeFamily typeEqs) name tyVars kindSig)) 
    -> AST.ClosedTypeFamilyDecl <$> createDeclHead name tyVars <*> trfKindSig kindSig <*> trfTypeEqs typeEqs
  TyClD (SynDecl name vars rhs _) 
    -> AST.TypeDecl <$> createDeclHead name vars <*> trfType rhs
  -- TyClD (DataDecl name vars (HsDataDefn nd ctx ct kind cons derivs) _) 
    -- -> AST.DataDecl 
  -- TyClD (ClassDecl ctx name vars funDeps sigs defs typeFuns typeFunDefs docs _) 
    -- -> AST.ClassDecl <$> trfCtx ctx <*> 

trfKindSig :: Maybe (LHsKind RdrName) -> Trf (AnnMaybe AST.KindConstraint RI)
trfKindSig = trfMaybe (\k -> annLoc (combineSrcSpans (getLoc k) <$> (tokenLoc AnnDcolon)) 
                                    (fmap AST.KindConstraint $ trfLoc trfKind' k))

trfKind :: Located (HsKind RdrName) -> Trf (Ann AST.Kind RI)
trfKind = trfLoc trfKind'

trfKind' :: HsKind RdrName -> Trf (AST.Kind RI)
trfKind' (HsTyVar (Exact n)) 
  | isWiredInName n && occNameString (nameOccName n) == "*"
  = pure AST.KindStar
  | isWiredInName n && occNameString (nameOccName n) == "#"
  = pure AST.KindUnbox
trfKind' (HsParTy kind) = AST.KindParen <$> trfKind kind
trfKind' (HsFunTy k1 k2) = AST.KindFn <$> trfKind k1 <*> trfKind k2
trfKind' (HsAppTy k1 k2) = AST.KindApp <$> trfKind k1 <*> trfKind k2
trfKind' (HsTyVar kv) = AST.KindVar <$> trfName' kv
trfKind' (HsExplicitTupleTy _ kinds) = AST.KindTuple . AnnList <$> mapM trfKind kinds
trfKind' (HsExplicitListTy _ kinds) = AST.KindList . AnnList <$> mapM trfKind kinds
  
trfTypeEqs :: [Located (TyFamInstEqn RdrName)] -> Trf (AnnList AST.TypeEqn RI)
trfTypeEqs = fmap AnnList . mapM trfTypeEq

trfTypeEq :: Located (TyFamInstEqn RdrName) -> Trf (Ann AST.TypeEqn RI)
trfTypeEq = trfLoc $ \(TyFamEqn name pats rhs) 
  -> AST.TypeEqn <$> combineTypes name pats <*> trfType rhs
  where combineTypes :: Located RdrName -> HsTyPats RdrName -> Trf (Ann AST.Type RI)
        combineTypes name pats 
          = foldl (\t p -> do typ <- t
                              annLoc (pure $ combineSrcSpans (annotation typ) (getLoc p)) 
                                     (AST.TyApp <$> pure typ <*> trfType p)) 
                  (annLoc (pure $ getLoc name) (AST.TyCon <$> trfName' (unLoc name))) 
                  (hswb_cts pats)
                 
  
trfType :: Located (HsType RdrName) -> Trf (Ann AST.Type RI)
trfType = trfLoc trfType'

trfType' :: HsType RdrName -> Trf (AST.Type RI)
trfType' (HsForAllTy _ _ bndrs ctx typ) = AST.TyForall <$> trfBindings (hsq_tvs bndrs) 
                                                       <*> trfCtx ctx <*> trfType typ
trfType' (HsTyVar name) | isRdrTc name = AST.TyCon <$> trfName' name
trfType' (HsTyVar name) | isRdrTyVar name = AST.TyVar <$> trfName' name
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
trfType' (HsNamedWildcardTy name) = AST.TyNamedWildcard <$> trfName' name


  
trfBindings :: [Located (HsTyVarBndr RdrName)] -> Trf (AnnList AST.TyVar RI)
trfBindings = undefined
  
trfCtx :: Located (HsContext RdrName) -> Trf (AnnMaybe AST.Context RI)
trfCtx (L l []) = pure annNothing
trfCtx (L l [L _ (HsParTy t)]) 
  = annJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                       (AST.ContextMulti . AnnList . (:[]) <$> trfAssertion t)
trfCtx (L l [L _ t]) 
  = annJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                       (AST.ContextOne <$> trfAssertion' t)
trfCtx (L l ctx) = annJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                                      (AST.ContextMulti . AnnList <$> mapM trfAssertion ctx) 
  
  
trfAssertion :: Located (HsType RdrName) -> Trf (Ann AST.Assertion RI)
trfAssertion = trfLoc trfAssertion'

trfAssertion' :: HsType RdrName -> Trf (AST.Assertion RI)
trfAssertion' = undefined
-- trfAssertion' (HsIParamTy typ) = _
-- trfAssertion' (HsEqTy t1 t2) = _
  
createDeclHead :: Located RdrName -> LHsTyVarBndrs RdrName -> Trf (Ann AST.DeclHead RI)
createDeclHead name vars
  = foldl (\t p -> do typ <- t
                      annLoc (pure $ combineSrcSpans (annotation typ) (getLoc p)) 
                             (AST.DHApp typ <$> trfTyVar p)) 
          (annLoc (pure $ getLoc name) (AST.DeclHead <$> trfName' (unLoc name))) 
          (hsq_tvs vars)

trfTyVar :: Located (HsTyVarBndr RdrName) -> Trf (Ann AST.TyVar RI)
trfTyVar var@(L l _) = trfLoc (\case
  UserTyVar name -> AST.TyVarDecl <$> annLoc (pure l) (trfName' name) <*> pure annNothing
  KindedTyVar name kind -> AST.TyVarDecl <$> trfName name <*> trfKindSig (Just kind)) var
          
trfQuasiQuotation' :: HsQuasiQuote RdrName -> Trf (AST.QuasiQuote RI)
trfQuasiQuotation' = undefined

trfSplice' :: HsSplice RdrName -> Trf (AST.Splice RI)
trfSplice' = undefined
  