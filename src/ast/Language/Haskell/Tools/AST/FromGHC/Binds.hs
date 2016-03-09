{-# LANGUAGE LambdaCase
           , ViewPatterns
           #-}
module Language.Haskell.Tools.AST.FromGHC.Binds where

import Control.Monad.Reader

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

trfBind :: TransformName n r => Located (HsBind n) -> Trf (Ann AST.ValueBind r)
trfBind = trfLoc trfBind'
  
trfBind' :: TransformName n r => HsBind n -> Trf (AST.ValueBind r)
-- a value binding (not a function)
trfBind' (FunBind { fun_id = id, fun_matches = MG { mg_alts = [L matchLoc (Match { m_pats = [], m_grhss = GRHSs [L rhsLoc (GRHS [] expr)] locals })]} }) 
  = AST.SimpleBind <$> (copyAnnot AST.VarPat (trfName id)) 
                   <*> annLoc (combineSrcSpans (getLoc expr) <$> tokenLoc AnnEqual) (AST.UnguardedRhs <$> trfExpr expr) 
                   <*> trfWhereLocalBinds locals
trfBind' (FunBind id _ (MG matches _ _ _) _ _ _) = AST.FunBind <$> trfAnnList (trfMatch' id) matches
trfBind' (PatBind pat (GRHSs rhs locals) _ _ _) = AST.SimpleBind <$> trfPattern pat <*> trfRhss rhs <*> trfWhereLocalBinds locals
trfBind' (AbsBinds typeVars vars exports _ _) = error "AbsBinds"
trfBind' (PatSynBind psb) = error "PatSynBind"
  
trfMatch :: TransformName n r => Located n -> Located (Match n (LHsExpr n)) -> Trf (Ann AST.Match r)
trfMatch id = trfLoc (trfMatch' id)

trfMatch' :: TransformName n r => Located n -> Match n (LHsExpr n) -> Trf (AST.Match r)
trfMatch' name (Match funid pats typ (GRHSs rhss locBinds))
  -- TODO: add the optional typ to pats
  = AST.Match <$> trfName (maybe name fst funid) 
              <*> makeList (before AnnEqual) (mapM trfPattern pats)
              <*> trfRhss rhss 
              <*> trfWhereLocalBinds locBinds
  
trfRhss :: TransformName n r => [Located (GRHS n (LHsExpr n))] -> Trf (Ann AST.Rhs r)
-- the original location on the GRHS misleadingly contains the local bindings
trfRhss [unLoc -> GRHS [] body] = annLoc (combineSrcSpans (getLoc body) <$> tokenLoc AnnEqual) 
                                         (AST.UnguardedRhs <$> trfExpr body)
trfRhss rhss = annLoc (pure $ collectLocs rhss) 
                      (AST.GuardedRhss . nonemptyAnnList <$> mapM trfGuardedRhs rhss)
                      
trfGuardedRhs :: TransformName n r => Located (GRHS n (LHsExpr n)) -> Trf (Ann AST.GuardedRhs r)
trfGuardedRhs = trfLoc $ \(GRHS guards body) 
  -> AST.GuardedRhs . nonemptyAnnList <$> mapM trfRhsGuard guards <*> trfExpr body
  
trfRhsGuard :: TransformName n r => Located (Stmt n (LHsExpr n)) -> Trf (Ann AST.RhsGuard r)
trfRhsGuard = trfLoc trfRhsGuard'
  
trfRhsGuard' :: TransformName n r => Stmt n (LHsExpr n) -> Trf (AST.RhsGuard r)
trfRhsGuard' (BindStmt pat body _ _) = AST.GuardBind <$> trfPattern pat <*> trfExpr body
trfRhsGuard' (BodyStmt body _ _ _) = AST.GuardCheck <$> trfExpr body
trfRhsGuard' (LetStmt binds) = AST.GuardLet <$> trfLocalBinds binds
  
trfWhereLocalBinds :: TransformName n r => HsLocalBinds n -> Trf (AnnMaybe AST.LocalBinds r)
trfWhereLocalBinds EmptyLocalBinds = nothing atTheEnd
trfWhereLocalBinds binds
-- TODO: add the where keyword
  = makeJust <$> annLoc (pure $ getBindLocs binds) (AST.LocalBinds <$> trfLocalBinds binds)

getBindLocs :: HsLocalBinds n -> SrcSpan
getBindLocs (HsValBinds (ValBindsIn binds sigs)) = foldLocs $ map getLoc (bagToList binds) ++ map getLoc sigs
getBindLocs (HsValBinds (ValBindsOut binds sigs)) = foldLocs $ map getLoc (concatMap (bagToList . snd) binds) ++ map getLoc sigs
  
trfLocalBinds :: TransformName n r => HsLocalBinds n -> Trf (AnnList AST.LocalBind r)
trfLocalBinds (HsValBinds (ValBindsIn binds sigs)) 
  = makeList (after AnnWhere)
             (orderDefs <$> ((++) <$> mapM (copyAnnot AST.LocalValBind . trfBind) (bagToList binds) 
                                  <*> mapM trfLocalSig sigs))
trfLocalBinds (HsValBinds (ValBindsOut binds sigs)) 
  = makeList (after AnnWhere)
             (orderDefs <$> ((++) <$> (concat <$> mapM (mapM (copyAnnot AST.LocalValBind . trfBind) . bagToList . snd) binds)
                                  <*> mapM trfLocalSig sigs))
             
trfLocalSig :: TransformName n r => Located (Sig n) -> Trf (Ann AST.LocalBind r)
trfLocalSig = trfLoc $ \case
  ts@(TypeSig {}) -> AST.LocalSignature <$> annCont (trfTypeSig' ts)
  (FixSig fs) -> AST.LocalFixity <$> annCont (trfFixitySig fs)
  
trfTypeSig :: TransformName n r => Located (Sig n) -> Trf (Ann AST.TypeSignature r)
trfTypeSig = trfLoc trfTypeSig'

trfTypeSig' :: TransformName n r => Sig n -> Trf (AST.TypeSignature r)
trfTypeSig' (TypeSig [name] typ _) = AST.TypeSignature <$> trfName name <*> trfType typ
  
trfFixitySig :: TransformName n r => FixitySig n -> Trf (AST.FixitySignature r)
trfFixitySig (FixitySig names (Fixity prec dir)) 
  = AST.FixitySignature <$> transformDir dir
                        <*> annLoc (tokenLoc AnnVal) (pure $ AST.Precedence prec) 
                        <*> (nonemptyAnnList <$> mapM trfName names)
  where transformDir InfixL = directionChar (pure AST.AssocLeft)
        transformDir InfixR = directionChar (pure AST.AssocRight)
        transformDir InfixN = annLoc (srcLocSpan . srcSpanEnd <$> tokenLoc AnnInfix) (pure AST.AssocNone)
        
        directionChar = annLoc ((\l -> mkSrcSpan (moveBackOneCol l) l) . srcSpanEnd <$> tokenLoc AnnInfix)
        moveBackOneCol (RealSrcLoc rl) = mkSrcLoc (srcLocFile rl) (srcLocLine rl) (srcLocCol rl - 1)
        moveBackOneCol (UnhelpfulLoc fs) = UnhelpfulLoc fs
   