{-# LANGUAGE LambdaCase
           , ViewPatterns
           #-}
module Language.Haskell.Tools.AST.FromGHC.Binds where

import SrcLoc as GHC
import RdrName as GHC
import HsBinds as GHC
import HsExpr as GHC
import BasicTypes as GHC
import ApiAnnotation as GHC
import Bag as GHC
import Outputable as GHC

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Exprs
import Language.Haskell.Tools.AST.FromGHC.Patterns
import Language.Haskell.Tools.AST.FromGHC.Types
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Base as AST
import qualified Language.Haskell.Tools.AST.Patterns as AST
import qualified Language.Haskell.Tools.AST.Binds as AST

trfBind :: TransformName n => Located (HsBind n) -> Trf (Ann AST.ValueBind (AnnotType n))
trfBind = trfLoc trfBind'
  
trfBind' :: TransformName n => HsBind n -> Trf (AST.ValueBind (AnnotType n))
trfBind' (FunBind { fun_id = id, fun_matches = MG { mg_alts = [L matchLoc (Match { m_pats = [], m_grhss = GRHSs [L rhsLoc (GRHS [] expr)] locals })]} }) = AST.SimpleBind <$> (copyAnnot AST.VarPat (trfName id)) <*> annLoc (combineSrcSpans (getLoc expr) <$> tokenLoc AnnEqual) (AST.UnguardedRhs <$> trfExpr expr) <*> trfWhereLocalBinds locals
trfBind' (FunBind id isInfix (MG matches _ _ _) _ _ _) = AST.FunBind . AnnList <$> mapM (trfMatch id) matches
trfBind' (PatBind pat (GRHSs rhs locals) _ _ _) = AST.SimpleBind <$> trfPattern pat <*> trfRhss rhs <*> trfWhereLocalBinds locals
trfBind' (AbsBinds typeVars vars exports _ _) = undefined
trfBind' (PatSynBind psb) = undefined
  
trfMatch :: TransformName n => Located n -> Located (Match n (LHsExpr n)) -> Trf (Ann AST.Match (AnnotType n))
trfMatch name = trfLoc $ \(Match funid pats typ (GRHSs rhss locBinds))
  -> AST.Match <$> trfName (maybe name fst funid) <*> (AnnList <$> mapM trfPattern pats) <*> trfMaybe trfType typ 
               <*> trfRhss rhss <*> trfWhereLocalBinds locBinds
  
trfRhss :: TransformName n => [Located (GRHS n (LHsExpr n))] -> Trf (Ann AST.Rhs (AnnotType n))
trfRhss [unLoc -> GRHS [] body] = annLoc (combineSrcSpans (getLoc body) <$> tokenLoc AnnEqual) 
                                         (AST.UnguardedRhs <$> trfExpr body)
trfRhss rhss = annLoc (pure $ collectLocs rhss) 
                      (AST.GuardedRhss . AnnList <$> mapM trfGuardedRhs rhss)
                      
trfGuardedRhs :: TransformName n => Located (GRHS n (LHsExpr n)) -> Trf (Ann AST.GuardedRhs (AnnotType n))
trfGuardedRhs = trfLoc $ \(GRHS guards body) 
  -> AST.GuardedRhs . AnnList <$> mapM trfRhsGuard guards <*> trfExpr body
  
trfRhsGuard :: TransformName n => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.RhsGuard (AnnotType n))
trfRhsGuard = trfLoc $ \case
  BindStmt pat body _ _ -> AST.GuardBind <$> trfPattern pat <*> trfExpr body
  BodyStmt body _ _ _ -> AST.GuardCheck <$> trfExpr body
  LetStmt binds -> AST.GuardLet <$> trfLocalBinds binds
  
trfWhereLocalBinds :: TransformName n => HsLocalBinds n -> Trf (AnnMaybe AST.LocalBinds (AnnotType n))
trfWhereLocalBinds EmptyLocalBinds = pure annNothing  
trfWhereLocalBinds binds
  = annJust <$> annLoc (pure $ getBindLocs binds) (AST.LocalBinds <$> trfLocalBinds binds)

getBindLocs :: HsLocalBinds n -> SrcSpan
getBindLocs (HsValBinds (ValBindsIn binds sigs)) = foldLocs $ map getLoc (bagToList binds) ++ map getLoc sigs
getBindLocs (HsValBinds (ValBindsOut binds sigs)) = foldLocs $ map getLoc (concatMap (bagToList . snd) binds) ++ map getLoc sigs
  
trfLocalBinds :: TransformName n => HsLocalBinds n -> Trf (AnnList AST.LocalBind (AnnotType n))
trfLocalBinds (HsValBinds (ValBindsIn binds sigs)) 
  = AnnList . orderDefs <$> ((++) <$> mapM (copyAnnot AST.LocalValBind . trfBind) (bagToList binds) 
                                  <*> mapM trfLocalSig sigs)
trfLocalBinds (HsValBinds (ValBindsOut binds sigs)) 
  = AnnList . orderDefs <$> 
     ((++) <$> (concat <$> mapM (mapM (copyAnnot AST.LocalValBind . trfBind) . bagToList . snd) binds)
           <*> mapM (error . showSDocUnsafe . ppr) sigs)
             
trfLocalSig :: TransformName n => Located (Sig n) -> Trf (Ann AST.LocalBind (AnnotType n))
trfLocalSig = trfLoc $ \case
  ts@(TypeSig {}) -> AST.LocalSignature <$> annCont (trfTypeSig' ts)
  (FixSig fs) -> AST.LocalFixity <$> annCont (trfFixitySig fs)
  
trfTypeSig :: TransformName n => Located (Sig n) -> Trf (Ann AST.TypeSignature (AnnotType n))
trfTypeSig = trfLoc trfTypeSig'

trfTypeSig' :: TransformName n => Sig n -> Trf (AST.TypeSignature (AnnotType n))
trfTypeSig' (TypeSig [name] typ _) = AST.TypeSignature <$> trfName name <*> trfType typ
  
trfFixitySig :: TransformName n => FixitySig n -> Trf (AST.FixitySignature (AnnotType n))
trfFixitySig (FixitySig names (Fixity prec dir)) 
  = AST.FixitySignature <$> transformDir dir
                        <*> annLoc (tokenLoc AnnVal) (pure $ AST.Precedence prec) 
                        <*> (AnnList <$> mapM trfName names)
  where transformDir InfixL = directionChar (pure AST.AssocLeft)
        transformDir InfixR = directionChar (pure AST.AssocRight)
        transformDir InfixN = annLoc (srcLocSpan . srcSpanEnd <$> tokenLoc AnnInfix) (pure AST.AssocNone)
        
        directionChar = annLoc ((\l -> mkSrcSpan (moveBackOneCol l) l) . srcSpanEnd <$> tokenLoc AnnInfix)
        moveBackOneCol (RealSrcLoc rl) = mkSrcLoc (srcLocFile rl) (srcLocLine rl) (srcLocCol rl - 1)
        moveBackOneCol (UnhelpfulLoc fs) = UnhelpfulLoc fs
   