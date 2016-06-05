{-# LANGUAGE LambdaCase
           , TupleSections
           , TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , ScopedTypeVariables
           , MultiParamTypeClasses
           , UndecidableInstances
           , AllowAmbiguousTypes
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
import Data.Data (Data)

import Language.Haskell.Tools.AST (Ann(..), AnnList(..), AnnMaybe(..), SemanticInfo(..), annotation, semanticInfo)
import qualified Language.Haskell.Tools.AST as AST

import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.GHCUtils

trfOperator :: TransformName name res => Located name -> Trf (Ann AST.Operator res)
trfOperator = trfLoc trfOperator'

trfOperator' :: TransformName name res => name -> Trf (AST.Operator res)
trfOperator' n
  | isSymOcc (occName n) = AST.NormalOp <$> (addNameInfo n =<< annCont (trfSimpleName' n))
  | otherwise = AST.BacktickOp <$> (addNameInfo n =<< annLoc loc (trfSimpleName' n))
     where loc = mkSrcSpan <$> (updateCol (+1) <$> atTheStart) <*> (updateCol (subtract 1) <$> atTheEnd)

trfName :: TransformName name res => Located name -> Trf (Ann AST.Name res)
trfName = trfLoc trfName'

trfName' :: TransformName name res => name -> Trf (AST.Name res)
trfName' n
  | isSymOcc (occName n) = AST.ParenName <$> (addNameInfo n =<< annLoc loc (trfSimpleName' n))
  | otherwise = AST.NormalName <$> (addNameInfo n =<< annCont (trfSimpleName' n))
     where loc = mkSrcSpan <$> (updateCol (+1) <$> atTheStart) <*> (updateCol (subtract 1) <$> atTheEnd)

class (DataId n, GHCName n) => TransformableName n where
  correctNameString :: n -> Trf String

instance TransformableName RdrName where
  correctNameString = pure . rdrNameStr

instance TransformableName GHC.Name where
  correctNameString n = getOriginalName (rdrName n)


-- | This class allows us to use the same transformation code for multiple variants of the GHC AST.
-- GHC Name annotated with 'name' can be transformed to our representation with semantic annotations of 'res'.
class (RangeAnnot res, SemanticAnnot res name, SemanticAnnot res GHC.Name, TransformableName name, HsHasName name) 
        => TransformName name res where
instance TransformName RdrName AST.RangeInfo where
instance (RangeAnnot r, SemanticAnnot r GHC.Name) => TransformName GHC.Name r where

addNameInfo :: TransformName n r => n -> Ann AST.SimpleName r -> Trf (Ann AST.SimpleName r)
addNameInfo name ast = do locals <- asks localsInScope
                          isDefining <- asks defining
                          return (annotation .- addSemanticInfo (NameInfo locals isDefining name) $ ast)

trfSimpleName :: TransformName name res => Located name -> Trf (Ann AST.SimpleName res)
trfSimpleName name@(L l n) = addNameInfo n =<< annLoc (pure l) (trfSimpleName' n)

trfSimpleName' :: TransformName name res => name -> Trf (AST.SimpleName res)
trfSimpleName' n = AST.nameFromList <$> (trfNameStr =<< correctNameString n)

-- | Creates a qualified name from a name string
trfNameStr :: RangeAnnot a => String -> Trf (AnnList AST.UnqualName a)
trfNameStr str = (\loc -> AnnList (toListAnnot "" "" "." loc) $ trfNameStr' str loc) <$> atTheStart

trfNameStr' :: RangeAnnot a => String -> SrcLoc -> [Ann AST.UnqualName a]
trfNameStr' str srcLoc = fst $
  foldl (\(r,loc) np -> let nextLoc = advanceAllSrcLoc loc np
                         in ( r ++ [Ann (toNodeAnnot $ mkSrcSpan loc nextLoc) (AST.UnqualName np)], advanceAllSrcLoc nextLoc "." ) ) 
  ([], srcLoc) (nameParts str)
  where -- | Move the source location according to a string
        advanceAllSrcLoc :: SrcLoc -> String -> SrcLoc
        advanceAllSrcLoc (RealSrcLoc rl) str = RealSrcLoc $ foldl advanceSrcLoc rl str
        advanceAllSrcLoc oth _ = oth

        -- | Break up a name into parts, but take care for operators
        nameParts :: String -> [String]
        nameParts = nameParts' ""

        nameParts' :: String -> String -> [String]
        nameParts' carry (c : rest) | isLetter c || isDigit c || c == '\'' || c == '_' || c == '#'
                                    = nameParts' (c:carry) rest
        nameParts' carry@(_:_) ('.' : rest) = reverse carry : nameParts rest
        nameParts' "" rest = [rest] 
        nameParts' carry [] = [reverse carry]
        nameParts' carry str = error $ "nameParts': " ++ show carry ++ " " ++ show str
  
trfModuleName :: RangeAnnot a => Located ModuleName -> Trf (Ann AST.SimpleName a)
trfModuleName = trfLoc trfModuleName'

trfModuleName' :: RangeAnnot a => ModuleName -> Trf (AST.SimpleName a)
trfModuleName' = (AST.nameFromList <$>) . trfNameStr . moduleNameString

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
                         Just GHC.Phantom -> pure AST.Phantom
         
trfPhase :: RangeAnnot a => Activation -> Trf (AnnMaybe AST.PhaseControl a)
trfPhase AlwaysActive = nothing "" "" atTheEnd
trfPhase (ActiveAfter _ pn) = makeJust <$> annCont (AST.PhaseControl <$> nothing "" "" (before AnnCloseS) <*> trfPhaseNum pn)
trfPhase (ActiveBefore _ pn) = makeJust <$> annCont (AST.PhaseControl <$> (makeJust <$> annLoc (tokenLoc AnnTilde) (pure AST.PhaseInvert)) <*> trfPhaseNum pn)

trfPhaseNum :: RangeAnnot a => PhaseNum -> Trf (Ann AST.PhaseNumber a)
trfPhaseNum i = annLoc (tokenLoc AnnVal) $ pure (AST.PhaseNumber $ fromIntegral i) 
