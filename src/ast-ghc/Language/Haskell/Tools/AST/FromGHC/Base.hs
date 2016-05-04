{-# LANGUAGE LambdaCase
           , TupleSections
           , TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , ScopedTypeVariables
           , MultiParamTypeClasses
           , UndecidableInstances
           #-}
module Language.Haskell.Tools.AST.FromGHC.Base where

import Control.Monad.Reader
import Data.List.Split
import Data.Char
import qualified Data.ByteString.Char8 as BS

import Control.Reference hiding (element)

import HsSyn as GHC
import Module as GHC
import RdrName as GHC
import Id as GHC
import Name as GHC hiding (Name, occName)
import qualified Name as GHC (Name)
import Outputable as GHC
import SrcLoc as GHC
import BasicTypes as GHC
import FastString as GHC
import ApiAnnotation as GHC
import ForeignCall as GHC
import CoAxiom as GHC
import Bag as GHC

import Language.Haskell.Tools.AST (Ann(..), AnnList(..), AnnMaybe(..), SemanticInfo(..), annotation, semanticInfo)
import qualified Language.Haskell.Tools.AST as AST

import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.GHCUtils

trfOperator :: TransformName name res => Located name -> Trf (Ann AST.Operator res)
trfOperator = trfLoc trfOperator'

trfOperator' :: TransformName name res => name -> Trf (AST.Operator res)
trfOperator' n
  | isSymOcc (occName n) = AST.NormalOp <$> (addNameInfo n =<< annCont (trfName' n))
  | otherwise = AST.BacktickOp <$> (addNameInfo n =<< annLoc loc (trfName' n))
     where loc = mkSrcSpan <$> (updateCol (+1) <$> atTheStart) <*> (updateCol (subtract 1) <$> atTheEnd)

class OutputableBndr name => GHCName name where 
  rdrName :: name -> RdrName
  getBindsAndSigs :: HsValBinds name -> ([LSig name], LHsBinds name)
  
instance GHCName RdrName where
  rdrName = id
  getBindsAndSigs (ValBindsIn binds sigs) = (sigs, binds)

occName :: GHCName n => n -> OccName
occName = rdrNameOcc . rdrName 
    
instance GHCName GHC.Name where
  rdrName = nameRdrName
  getBindsAndSigs (ValBindsOut bindGroups sigs) = (sigs, unionManyBags (map snd bindGroups))
  
-- | This class allows us to use the same transformation code for multiple variants of the GHC AST.
-- GHC Name annotated with 'name' can be transformed to our representation with semantic annotations of 'res'.
class (RangeAnnot res, SemanticAnnot res name, SemanticAnnot res GHC.Name, GHCName name, HsHasName name) 
        => TransformName name res where
  addNameInfo :: name -> Ann AST.Name res -> Trf (Ann AST.Name res)
  
instance TransformName RdrName AST.RangeInfo where
  addNameInfo _ = return

instance (RangeAnnot r, SemanticAnnot r GHC.Name) => TransformName GHC.Name r where
  addNameInfo name ast = do locals <- asks localsInScope
                            isDefining <- asks defining
                            return (annotation .- addSemanticInfo (NameInfo locals isDefining name) $ ast)
  
trfName :: TransformName name res => Located name -> Trf (Ann AST.Name res)
trfName name@(L l n) = addNameInfo n =<< trfLoc trfName' name

trfName' :: TransformName name res => name -> Trf (AST.Name res)
trfName' n = let occ = occName n
                 str = occNameString occ
              in (if isSymOcc occ then AST.Name <$> emptyList "." atTheStart <*> annCont (pure $ AST.SimpleName str)
                                  else AST.nameFromList . fst <$> trfNameStr str)

trfNameSp :: TransformName name res => name -> SrcSpan -> Trf (Ann AST.Name res)
trfNameSp n l = trfName (L l n)

trfNameSp' :: TransformName name res => name -> Trf (Ann AST.Name res)
trfNameSp' n = trfNameSp n =<< asks contRange
  
trfSimplName :: RangeAnnot a => SrcLoc -> OccName -> Trf (Ann AST.SimpleName a)
trfSimplName start n = (\srcLoc -> Ann (toNodeAnnot $ mkSrcSpan start srcLoc) $ AST.SimpleName (pprStr n)) <$> asks (srcSpanEnd . contRange)
  where pprStr :: Outputable a => a -> String
        pprStr = showSDocUnsafe . ppr
                

trfNameStr :: RangeAnnot a => String -> Trf (AnnList AST.SimpleName a, SrcLoc)
trfNameStr str = (\srcLoc -> (\(ls,loc) -> (AnnList (toListAnnot "" "" "." srcLoc) ls, loc))
  (foldl (\(r,loc) np -> let nextLoc = advanceAllSrcLoc loc np
                          in ( r ++ [Ann (toNodeAnnot $ mkSrcSpan loc nextLoc) (AST.SimpleName np)], advanceAllSrcLoc nextLoc "." ) ) 
  ([],srcLoc) (splitOn "." str))) <$> asks (srcSpanStart . contRange)
  where advanceAllSrcLoc :: SrcLoc -> String -> SrcLoc
        advanceAllSrcLoc (RealSrcLoc rl) str = RealSrcLoc $ foldl advanceSrcLoc rl str
        advanceAllSrcLoc oth _ = oth
  
  
trfModuleName :: RangeAnnot a => Located ModuleName -> Trf (Ann AST.Name a)
trfModuleName = trfLoc trfModuleName'

trfModuleName' :: RangeAnnot a => ModuleName -> Trf (AST.Name a)
trfModuleName' = (AST.nameFromList . fst <$>) . trfNameStr . moduleNameString

trfFastString :: RangeAnnot a => Located FastString -> Trf (Ann AST.StringNode a)
trfFastString = trfLoc $ pure . AST.StringNode . unpackFS
  
trfDataKeyword :: RangeAnnot a => NewOrData -> Trf (Ann AST.DataOrNewtypeKeyword a)
trfDataKeyword NewType = annLoc (tokenLoc AnnNewtype) (pure AST.NewtypeKeyword)
trfDataKeyword DataType = annLoc (tokenLoc AnnData) (pure AST.DataKeyword)
     
trfCallConv :: RangeAnnot a => Located CCallConv -> Trf (Ann AST.CallConv a)
trfCallConv = trfLoc trfCallConv'
   
trfCallConv' :: RangeAnnot a => CCallConv -> Trf (AST.CallConv a)
trfCallConv' CCallConv = pure AST.CCall
trfCallConv' CApiConv = pure AST.CApi
trfCallConv' StdCallConv = pure AST.StdCall
-- trfCallConv' PrimCallConv = 
trfCallConv' JavaScriptCallConv = pure AST.JavaScript

trfSafety :: RangeAnnot a => SrcSpan -> Located Safety -> Trf (AnnMaybe AST.Safety a)
trfSafety ccLoc lsaf@(L l _) | isGoodSrcSpan l 
  = makeJust <$> trfLoc (pure . \case
      PlaySafe -> AST.Safe
      PlayInterruptible -> AST.Interruptible
      PlayRisky -> AST.Unsafe) lsaf
  | otherwise = nothing " " "" (pure $ srcSpanEnd ccLoc)

trfOverlap :: RangeAnnot a => Located OverlapMode -> Trf (Ann AST.OverlapPragma a)
trfOverlap = trfLoc $ pure . \case
  NoOverlap _ -> AST.DisableOverlap
  Overlappable _ -> AST.Overlappable
  Overlapping _ -> AST.Overlapping
  Overlaps _ -> AST.Overlaps
  Incoherent _ -> AST.IncoherentOverlap

trfRole :: RangeAnnot a => Located (Maybe Role) -> Trf (Ann AST.Role a)
trfRole = trfLoc $ \case Just Nominal -> pure AST.Nominal
                         Just Representational -> pure AST.Representational
                         Just Phantom -> pure AST.Phantom
         
trfPhase :: RangeAnnot a => Activation -> Trf (AnnMaybe AST.PhaseControl a)
trfPhase AlwaysActive = nothing "" "" atTheEnd
trfPhase (ActiveAfter pn) = makeJust <$> annCont (AST.PhaseControl <$> nothing "" "" (before AnnCloseS) <*> trfPhaseNum pn)
trfPhase (ActiveBefore pn) = makeJust <$> annCont (AST.PhaseControl <$> (makeJust <$> annLoc (tokenLoc AnnTilde) (pure AST.PhaseInvert)) <*> trfPhaseNum pn)

trfPhaseNum :: RangeAnnot a => PhaseNum -> Trf (Ann AST.PhaseNumber a)
trfPhaseNum i = annLoc (tokenLoc AnnVal) $ pure (AST.PhaseNumber $ fromIntegral i) 
