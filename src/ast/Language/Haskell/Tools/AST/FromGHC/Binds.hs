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

trfBind :: Located (HsBind RdrName) -> Trf (Ann AST.ValueBind RI)
trfBind = trfLoc trfBind'
  
trfBind' :: HsBind RdrName -> Trf (AST.ValueBind RI)
trfBind' (FunBind { fun_id = id, fun_matches = MG { mg_alts = [L matchLoc (Match { m_pats = [], m_grhss = GRHSs [L rhsLoc (GRHS [] expr)] locals })]} }) = AST.SimpleBind <$> (copyAnnot AST.VarPat (trfName id)) <*> annLoc (combineSrcSpans (getLoc expr) <$> tokenLoc AnnEqual) (AST.UnguardedRhs <$> trfExpr expr) <*> trfWhereLocalBinds locals
trfBind' (FunBind id isInfix (MG matches _ _ _) _ _ _) = AST.FunBind . AnnList <$> mapM (trfMatch id) matches
trfBind' (PatBind pat (GRHSs rhs locals) _ _ _) = AST.SimpleBind <$> trfPattern pat <*> trfRhss rhs <*> trfWhereLocalBinds locals
trfBind' (AbsBinds typeVars vars exports _ _) = undefined
trfBind' (PatSynBind psb) = undefined
  
trfMatch :: Located RdrName -> Located (Match RdrName (LHsExpr RdrName)) -> Trf (Ann AST.Match RI)
trfMatch name = trfLoc $ \(Match funid pats typ (GRHSs rhss locBinds))
  -> AST.Match <$> trfName (maybe name fst funid) <*> (AnnList <$> mapM trfPattern pats) <*> trfMaybe trfType typ 
               <*> trfRhss rhss <*> trfWhereLocalBinds locBinds
  
trfRhss :: [Located (GRHS RdrName (LHsExpr RdrName))] -> Trf (Ann AST.Rhs RI)
trfRhss [unLoc -> GRHS [] body] = annLoc (combineSrcSpans (getLoc body) <$> tokenLoc AnnEqual) 
                                         (AST.UnguardedRhs <$> trfExpr body)
trfRhss rhss = annLoc (pure $ collectLocs rhss) 
                      (AST.GuardedRhss . AnnList <$> mapM trfGuardedRhs rhss)
                      
trfGuardedRhs :: Located (GRHS RdrName (LHsExpr RdrName)) -> Trf (Ann AST.GuardedRhs RI)
trfGuardedRhs = trfLoc $ \(GRHS guards body) 
  -> AST.GuardedRhs . AnnList <$> mapM trfRhsGuard guards <*> trfExpr body
  
trfRhsGuard :: Located (Stmt RdrName (LHsExpr RdrName)) -> Trf (Ann AST.RhsGuard RI)
trfRhsGuard = trfLoc $ \case
  BindStmt pat body _ _ -> AST.GuardBind <$> trfPattern pat <*> trfExpr body
  BodyStmt body _ _ _ -> AST.GuardCheck <$> trfExpr body
  LetStmt binds -> AST.GuardLet <$> trfLocalBinds binds
  
trfWhereLocalBinds :: HsLocalBinds RdrName -> Trf (AnnMaybe AST.LocalBinds RI)
trfWhereLocalBinds EmptyLocalBinds = pure annNothing  
trfWhereLocalBinds binds@(HsValBinds (ValBindsIn vals sigs)) 
  = annJust <$> annLoc (collectAnnots . _fromAnnList <$> localBinds) (AST.LocalBinds <$> localBinds)
      where localBinds = trfLocalBinds binds

trfLocalBinds :: HsLocalBinds RdrName -> Trf (AnnList AST.LocalBind RI)
trfLocalBinds (HsValBinds (ValBindsIn binds sigs)) 
  = AnnList . orderDefs <$> ((++) <$> mapM (copyAnnot AST.LocalValBind . trfBind) (bagToList binds) 
                                  <*> mapM trfLocalSig sigs)
             

trfLocalSig :: Located (Sig RdrName) -> Trf (Ann AST.LocalBind RI)
trfLocalSig = trfLoc $ \case
  ts@(TypeSig {}) -> AST.LocalSignature <$> annCont (trfTypeSig ts)
  (FixSig fs) -> AST.LocalFixity <$> annCont (trfFixitySig fs)
  
trfTypeSig :: Sig RdrName -> Trf (AST.TypeSignature RI)
trfTypeSig (TypeSig [name] typ _) = AST.TypeSignature <$> trfName name <*> trfType typ
  
trfFixitySig :: FixitySig RdrName -> Trf (AST.FixitySignature RI)
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
   