{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Tools.AST.FromGHC.Decl where

import RdrName as GHC
import HsSyn as GHC
import SrcLoc as GHC
import HsDecls as GHC
import Name as GHC
import OccName as GHC
import ApiAnnotation as GHC

import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Decl as AST

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

trfDecls :: [LHsDecl RdrName] -> Trf (AnnList AST.Decl RI)
trfDecls decls = AnnList <$> mapM trfDecl decls

trfDecl :: Located (HsDecl RdrName) -> Trf (Ann AST.Decl RI)
trfDecl = trfLoc $ \case
  TyClD (FamDecl (FamilyDecl DataFamily name tyVars kindSig)) -> AST.DataFamilyDecl <$> createDeclHead name tyVars <*> trfKindSig kindSig
  TyClD (FamDecl (FamilyDecl OpenTypeFamily name tyVars kindSig)) -> AST.TypeFamilyDecl <$> createDeclHead name tyVars <*> trfKindSig kindSig
  TyClD (FamDecl (FamilyDecl (ClosedTypeFamily typeEqs) name tyVars kindSig)) -> AST.ClosedTypeFamilyDecl <$> createDeclHead name tyVars <*> trfKindSig kindSig <*> trfTypeEqs typeEqs
  TyClD (SynDecl name vars rhs _) -> undefined
  TyClD (DataDecl name vars decl _) -> undefined
  TyClD (ClassDecl ctx name vars funDeps sigs defs typeFuns typeFunDefs docs _) -> undefined

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
trfTypeEqs = undefined 
  
createDeclHead :: Located RdrName -> LHsTyVarBndrs RdrName -> Trf (Ann AST.DeclHead RI)
createDeclHead = undefined
  