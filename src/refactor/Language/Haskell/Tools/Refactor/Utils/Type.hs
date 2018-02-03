{-# LANGUAGE ExplicitNamespaces, FlexibleContexts, KindSignatures, MonoLocalBinds, RankNTypes, ScopedTypeVariables, TypeApplications #-}

module Language.Haskell.Tools.Refactor.Utils.Type (typeExpr, appTypeMatches, literalType) where

import Data.List
import Control.Monad.State
import Control.Reference

import GHC
import Module as GHC
import InstEnv as GHC
import Unify as GHC
import Type as GHC
import Name as GHC
import Var as GHC
import UniqSupply as GHC
import Unique as GHC
import TysWiredIn as GHC
import TysPrim as GHC
import PrelNames as GHC
import ConLike as GHC
import PatSyn as GHC
import BasicTypes as GHC

import Language.Haskell.Tools.Rewrite.ElementTypes as AST
import Language.Haskell.Tools.Rewrite.Match as AST
import Language.Haskell.Tools.AST as AST

typeExpr :: Expr -> Ghc GHC.Type
typeExpr e = do usupp <- liftIO $ mkSplitUniqSupply 'z'
                evalStateT (typeExpr' e) (uniqsFromSupply usupp)

typeExpr' :: Expr -> StateT [Unique] Ghc GHC.Type
typeExpr' (Var n) = return $ idType $ semanticsId (n ^. simpleName)
typeExpr' (Lit l) = literalType' l
typeExpr' (InfixApp lhs op rhs) = do
  lhsType <- typeExpr' lhs
  rhsType <- typeExpr' rhs
  let opType = idType $ semanticsId (op ^. operatorName)
  let (opt, repack) = splitType' opType
  case splitFunTys opt of
    (lhsT:rhsT:rest, resTyp) -> do
      let subst = tcUnifyTys (\_ -> BindMe) [lhsType, rhsType] [lhsT, rhsT]
      return $ maybe id substTy subst $ repack (mkFunTys rest resTyp)
    _ -> resultType
typeExpr' (PrefixApp op rhs) = do
  let opType = idType $ semanticsId (op ^. operatorName)
  argType <- typeExpr' rhs
  let (ft, repack) = splitType' opType
  case splitFunTy_maybe ft of
    Just (argTyp, resTyp) -> do
      let subst = tcUnifyTy argType argTyp
      return $ maybe id substTy subst $ repack resTyp
    _ -> resultType
typeExpr' (App f arg) = do
  argType <- typeExpr' arg
  funType <- typeExpr' f
  let (ft, repack) = splitType' funType
  case splitFunTy_maybe ft of
    Just (argTyp, resTyp) -> do
      let subst = tcUnifyTy argType argTyp
      return $ maybe id substTy subst $ repack resTyp
    Nothing -> return funType
typeExpr' (Lambda args e) = do
  (resType, recomp) <- splitType' <$> typeExpr' e
  (argTypes, recomps) <- unzip <$> mapM (\_ -> splitType' <$> resultType) (args ^? annList)
  return $ foldr (.) recomp recomps $ mkFunTys argTypes resType
typeExpr' (Let _ e) = typeExpr' e
typeExpr' (If _ then' _) = typeExpr' then'
typeExpr' (MultiIf alts) =
  case alts ^? annList & caseGuardExpr of e:_ -> typeExpr' e
                                          _   -> resultType
typeExpr' (Case _ alts) =
  case alts ^? annList & altRhs & rhsCaseExpr of e:_ -> typeExpr' e
                                                 _   -> resultType
typeExpr' (Do stmts) =
  case stmts ^? annList & stmtExpr of e:_ -> typeExpr' e
                                      _   -> resultType
typeExpr' (Tuple elems) = do
  (elemTypes, pack) <- unzip . map splitType' <$> mapM typeExpr' (elems ^? annList)
  return $ foldr (.) id pack $ mkTupleTy Boxed elemTypes
typeExpr' (AST.UnboxedTuple elems) = do
  (elemTypes, pack) <- unzip . map splitType' <$> mapM typeExpr' (elems ^? annList)
  return $ foldr (.) id pack $ mkTupleTy Unboxed elemTypes
-- typeExpr' (TupleSection elems) 
--   = let (elems, holes) <- partition (\case Present _ -> True; Missing -> False) elems
--      in do args <- mapM resultType holes
-- 
-- typeExpr' (UnboxedTupSec elems) = 
typeExpr' (List elems) = do
  case elems ^? annList of []  -> do (typ, pack) <- splitType' <$> resultType
                                     return $ pack $ mkListTy typ
                           e:_ -> do (typ, pack) <- splitType' <$> typeExpr' e
                                     return $ pack $ mkListTy typ
typeExpr' (ParArray elems) = do
  case elems ^? annList of []  -> do (typ, pack) <- splitType' <$> resultType
                                     return $ pack $ mkPArrTy typ
                           e:_ -> do (typ, pack) <- splitType' <$> typeExpr' e
                                     return $ pack $ mkPArrTy typ
typeExpr' (Paren inner) = typeExpr' inner
typeExpr' (LeftSection lhs op) = do
  let opType = idType $ semanticsId (op ^. operatorName)
  argType <- typeExpr' lhs
  let (ft, repack) = splitType' opType
  case splitFunTy_maybe ft of 
    Just (argTyp, resTyp) -> do
      let subst = tcUnifyTy argType argTyp
      return $ maybe id substTy subst $ repack resTyp
    _ -> resultType
typeExpr' (RightSection op rhs) = do
  let opType = idType $ semanticsId (op ^. operatorName)
  argType <- typeExpr' rhs
  let (ft, repack) = splitType' opType
  case splitFunTys ft of 
    (arg1:arg2:rest, resTyp) -> do 
      let subst = tcUnifyTy arg2 argType
      return $ maybe id substTy subst $ repack (mkFunTys (arg1:rest) resTyp)
    _ -> resultType
typeExpr' (AST.RecCon name _) 
  = do def <- lift $ maybe (return Nothing) GHC.lookupName (semanticsName (name ^. simpleName))
       case def of 
         Just (AConLike (RealDataCon con)) -> return $ dataConSig con ^. _4
         Just (AConLike (PatSynCon patSyn)) -> return $ patSynSig patSyn ^. _6
         _ -> resultType
typeExpr' (AST.RecUpdate e _) = typeExpr' e
typeExpr' (AST.Enum from _ _) = mkListTy <$> typeExpr' from
typeExpr' (AST.ParArrayEnum from _ _) = mkPArrTy <$> typeExpr' from
typeExpr' (AST.ListComp e _) = mkListTy <$> typeExpr' e
typeExpr' (AST.ParArrayComp e _) = mkPArrTy <$> typeExpr' e
-- typeExpr' (AST.UTypeSig _ t) = -- TODO: evaluate type
typeExpr' Hole = resultType
typeExpr' _ = resultType

literalType :: Literal -> Ghc GHC.Type
literalType e = do usupp <- liftIO $ mkSplitUniqSupply 'z'
                   evalStateT (literalType' e) (uniqsFromSupply usupp)

literalType' :: Literal -> StateT [Unique] Ghc GHC.Type
literalType' (StringLit {}) = return stringTy
literalType' (CharLit {}) = return charTy
literalType' (IntLit {}) = litType numClassName
literalType' (FracLit {}) = litType fractionalClassName
literalType' (PrimIntLit {}) = return $ intPrimTy
literalType' (PrimWordLit {}) = return $ word32PrimTy
literalType' (PrimFloatLit {}) = return $ floatX4PrimTy
literalType' (PrimDoubleLit {}) = return $ doubleX2PrimTy
literalType' (PrimCharLit {}) = return $ charPrimTy
literalType' (PrimStringLit {}) = return $ addrPrimTy

appTypeMatches :: [ClsInst] -> GHC.Type -> [GHC.Type] -> Maybe (TCvSubst, GHC.Type)
appTypeMatches insts functionT argTs -- TODO: check instances
  = let (funT, repackFun) = splitType functionT
        (args, resT) = splitFunTys funT
        argTypes = fst $ unzip $ map splitType argTs
     in if length args >= length argTypes
          then case tcUnifyTys (\_ -> BindMe) (take (length argTypes) args) argTypes of
                 Just st -> let (t', check) = repackFun st insts (substTy st $ mkFunTys (drop (length argTypes) args) resT)
                             in if check then Just (st, t')
                                         else Nothing
                 Nothing -> Nothing
          else Nothing

splitType' :: GHC.Type -> (GHC.Type, GHC.Type -> GHC.Type)
splitType' t = case splitType t of (t', repack) -> (t', fst . repack emptyTCvSubst [])

splitType :: GHC.Type -> (GHC.Type, TCvSubst -> [ClsInst] -> GHC.Type -> (GHC.Type, Bool))
splitType typ =
  let (foralls, typ') = splitForAllTys typ
      (args, coreType) = splitFunTys typ'
      (implicitArgs, realArgs) = partition isPredTy args
      repack subst insts t = (mkInvForAllTys (filter (`elem` tvs) foralls) . mkFunTys keptConstraints $ t, check)
        where
          check = and $ map checkInst implicitArgs
          checkInst t =
            case splitTyConApp_maybe (substTy subst t) of
              Just (tc, args) -> case tyConClass_maybe tc of 
                                   Just cls -> case lookupInstEnv False instEnvs cls args of 
                                                 ([],[],[]) -> False
                                                 _          -> True
                                   Nothing -> True
              Nothing -> True
          instEnvs = InstEnvs (extendInstEnvList emptyInstEnv insts) emptyInstEnv emptyModuleSet
          keptConstraints = filter hasCommonTv implicitArgs
          tvs = tyCoVarsOfTypeWellScoped t
          hasCommonTv = not . null . intersect tvs . tyCoVarsOfTypeWellScoped
   in (mkFunTys realArgs coreType, repack)

resultType :: StateT [Unique] Ghc GHC.Type
resultType = do 
  name <- newName
  let tv = mkTyVar name (mkTyConTy starKindTyCon)
  return $ mkInvForAllTys [tv] (mkTyVarTy tv)

litType :: GHC.Name -> StateT [Unique] Ghc GHC.Type
litType constraint = do 
  Just (ATyCon numTyCon) <- lift $ lookupName constraint
  name <- newName
  let tv = mkTyVar name (mkTyConTy starKindTyCon)
  return $ mkInvForAllTys [tv] $ mkFunTy (mkTyConApp numTyCon [mkTyVarTy tv]) (mkTyVarTy tv)

newName :: Monad m => StateT [Unique] m GHC.Name
newName = do uniq <- gets head
             modify tail
             let occn  = mkOccName tvName "a"
             return (mkInternalName uniq occn noSrcSpan)
