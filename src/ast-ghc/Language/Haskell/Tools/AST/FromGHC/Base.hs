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
           , TypeApplications
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

import Language.Haskell.Tools.AST (Ann(..), AnnList(..), AnnMaybe(..), SemanticInfo(..), RangeStage, Dom, annotation, semanticInfo)
import Language.Haskell.Tools.AST (NoSemanticInfo(..))
import qualified Language.Haskell.Tools.AST as AST

import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.GHCUtils

trfOperator :: TransformName n r => Located n -> Trf (Ann AST.Operator (Dom r) RangeStage)
trfOperator = trfLocNoSema trfOperator'

trfOperator' :: TransformName n r => n -> Trf (AST.Operator (Dom r) RangeStage)
trfOperator' n
  | isSymOcc (occName n) = AST.NormalOp <$> (annCont (createNameInfo (transformName n)) (trfQualifiedName' n))
  | otherwise = AST.BacktickOp <$> (annLoc (createNameInfo (transformName n)) loc (trfQualifiedName' n))
     where loc = mkSrcSpan <$> (updateCol (+1) <$> atTheStart) <*> (updateCol (subtract 1) <$> atTheEnd)

trfName :: TransformName n r => Located n -> Trf (Ann AST.Name (Dom r) RangeStage)
trfName = trfLocNoSema trfName'

trfName' :: TransformName n r => n -> Trf (AST.Name (Dom r) RangeStage)
trfName' n
  | isSymOcc (occName n) = AST.ParenName <$> (annLoc (createNameInfo (transformName n)) loc (trfQualifiedName' n))
  | otherwise = AST.NormalName <$> (annCont (createNameInfo (transformName n)) (trfQualifiedName' n))
     where loc = mkSrcSpan <$> (updateCol (+1) <$> atTheStart) <*> (updateCol (subtract 1) <$> atTheEnd)

trfAmbiguousFieldName :: TransformName n r => Located (AmbiguousFieldOcc n) -> Trf (Ann AST.Name (Dom r) RangeStage)
trfAmbiguousFieldName all@(L l af) = trfAmbiguousFieldName' l af

trfAmbiguousFieldName' :: forall n r . TransformName n r => SrcSpan -> AmbiguousFieldOcc n -> Trf (Ann AST.Name (Dom r) RangeStage)
trfAmbiguousFieldName' l (Unambiguous (L _ rdr) pr) = annLocNoSema (pure l) $ trfName' (unpackPostRn @n rdr pr)
-- no Id transformation is done, so we can basically ignore the postTC value
trfAmbiguousFieldName' _ (Ambiguous (L l rdr) _) 
  = do locals <- asks localsInScope
       isDefining <- asks defining
       annLocNoSema (pure l) 
         $ AST.NormalName 
         <$> (annLoc (createAmbigousNameInfo rdr l) (pure l) $ AST.nameFromList <$> trfNameStr (rdrNameStr rdr))

class (DataId n, Eq n, GHCName n) => TransformableName n where
  correctNameString :: n -> Trf String
  getDeclSplices :: Trf [Located (HsSplice n)]
  fromGHCName :: GHC.Name -> n 

instance TransformableName RdrName where
  correctNameString = pure . rdrNameStr
  getDeclSplices = pure []
  fromGHCName = rdrName

instance TransformableName GHC.Name where
  correctNameString n = getOriginalName (rdrName n)
  getDeclSplices = asks declSplices
  fromGHCName = id

-- | This class allows us to use the same transformation code for multiple variants of the GHC AST.
-- GHC Name annotated with 'name' can be transformed to our representation with semantic annotations of 'res'.
class (TransformableName name, HsHasName name, TransformableName res, HsHasName res, GHCName res) 
        => TransformName name res where
  -- | Demote a given name
  transformName :: name -> res

instance {-# OVERLAPPABLE #-} (n ~ r, TransformableName n, HsHasName n) => TransformName n r where
  transformName = id

instance {-# OVERLAPS #-} (TransformableName res, GHCName res, HsHasName res) => TransformName GHC.Name res where
  transformName = fromGHCName

trfImplicitName :: HsIPName -> Trf (Ann AST.Name (Dom r) RangeStage)
trfImplicitName (HsIPName fs) 
  = let nstr = unpackFS fs 
     in do rng <- asks contRange
           let rng' = mkSrcSpan (updateCol (+1) (srcSpanStart rng)) (srcSpanEnd rng)
           annContNoSema (AST.ImplicitName <$> annLoc (createImplicitNameInfo nstr) (pure rng') (AST.nameFromList <$> trfNameStr nstr))

trfQualifiedName :: TransformName n r => Located n -> Trf (Ann AST.QualifiedName (Dom r) RangeStage)
trfQualifiedName name@(L l n) = annLoc (createNameInfo (transformName n)) (pure l) (trfQualifiedName' n)

trfQualifiedName' :: TransformName n r => n -> Trf (AST.QualifiedName (Dom r) RangeStage)
trfQualifiedName' n = AST.nameFromList <$> (trfNameStr =<< correctNameString n)

-- | Creates a qualified name from a name string
trfNameStr :: String -> Trf (AnnList AST.UnqualName (Dom r) RangeStage)
trfNameStr str = makeList "." atTheStart (trfNameStr' str <$> atTheStart)

trfNameStr' :: String -> SrcLoc -> [Ann AST.UnqualName (Dom r) RangeStage]
trfNameStr' str srcLoc = fst $
  foldl (\(r,loc) np -> let nextLoc = advanceAllSrcLoc loc np
                         in ( r ++ [Ann (noSemaInfo $ AST.NodeSpan (mkSrcSpan loc nextLoc)) (AST.UnqualName np)], advanceAllSrcLoc nextLoc "." ) ) 
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
  
trfModuleName :: Located ModuleName -> Trf (Ann AST.ModuleName (Dom r) RangeStage)
trfModuleName = trfLocNoSema trfModuleName'

trfModuleName' :: ModuleName -> Trf (AST.ModuleName (Dom r) RangeStage)
trfModuleName' = pure . AST.ModuleName . moduleNameString

trfFastString :: Located FastString -> Trf (Ann AST.StringNode (Dom r) RangeStage)
trfFastString = trfLocNoSema $ pure . AST.StringNode . unpackFS
  
trfDataKeyword ::  NewOrData -> Trf (Ann AST.DataOrNewtypeKeyword (Dom r) RangeStage)
trfDataKeyword NewType = annLocNoSema (tokenLoc AnnNewtype) (pure AST.NewtypeKeyword)
trfDataKeyword DataType = annLocNoSema (tokenLoc AnnData) (pure AST.DataKeyword)
     
trfCallConv :: Located CCallConv -> Trf (Ann AST.CallConv (Dom r) RangeStage)
trfCallConv = trfLocNoSema trfCallConv'
   
trfCallConv' :: CCallConv -> Trf (AST.CallConv (Dom r) RangeStage)
trfCallConv' CCallConv = pure AST.CCall
trfCallConv' CApiConv = pure AST.CApi
trfCallConv' StdCallConv = pure AST.StdCall
-- trfCallConv' PrimCallConv = 
trfCallConv' JavaScriptCallConv = pure AST.JavaScript

trfSafety :: SrcSpan -> Located Safety -> Trf (AnnMaybe AST.Safety (Dom r) RangeStage)
trfSafety ccLoc lsaf@(L l _) | isGoodSrcSpan l 
  = makeJust <$> trfLocNoSema (pure . \case
      PlaySafe -> AST.Safe
      PlayInterruptible -> AST.Interruptible
      PlayRisky -> AST.Unsafe) lsaf
  | otherwise = nothing " " "" (pure $ srcSpanEnd ccLoc)

trfOverlap :: Located OverlapMode -> Trf (Ann AST.OverlapPragma (Dom r) RangeStage)
trfOverlap = trfLocNoSema $ pure . \case
  NoOverlap _ -> AST.DisableOverlap
  Overlappable _ -> AST.Overlappable
  Overlapping _ -> AST.Overlapping
  Overlaps _ -> AST.Overlaps
  Incoherent _ -> AST.IncoherentOverlap

trfRole :: Located (Maybe Role) -> Trf (Ann AST.Role (Dom r) RangeStage)
trfRole = trfLocNoSema $ \case Just Nominal -> pure AST.Nominal
                               Just Representational -> pure AST.Representational
                               Just GHC.Phantom -> pure AST.Phantom
         
trfPhase :: Trf SrcLoc -> Activation -> Trf (AnnMaybe AST.PhaseControl (Dom r) RangeStage)
trfPhase l AlwaysActive = nothing "" " " l
trfPhase _ (ActiveAfter _ pn) = makeJust <$> annLocNoSema (combineSrcSpans <$> tokenLoc AnnOpenS <*> tokenLoc AnnCloseS) 
                                                          (AST.PhaseControl <$> nothing "" "" (before AnnCloseS) <*> trfPhaseNum pn)
trfPhase _ (ActiveBefore _ pn) = makeJust <$> annLocNoSema (combineSrcSpans <$> tokenLoc AnnOpenS <*> tokenLoc AnnCloseS)
                                                           (AST.PhaseControl <$> (makeJust <$> annLocNoSema (tokenLoc AnnTilde) (pure AST.PhaseInvert)) <*> trfPhaseNum pn)

trfPhaseNum ::  PhaseNum -> Trf (Ann AST.PhaseNumber (Dom r) RangeStage)
trfPhaseNum i = annLocNoSema (tokenLoc AnnVal) $ pure (AST.PhaseNumber $ fromIntegral i) 
