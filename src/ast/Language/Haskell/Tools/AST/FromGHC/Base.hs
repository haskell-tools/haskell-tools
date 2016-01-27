{-# LANGUAGE LambdaCase
           , TupleSections
           , TypeFamilies
           , FlexibleContexts
           , TypeSynonymInstances
           #-}
module Language.Haskell.Tools.AST.FromGHC.Base where

import Control.Monad.Reader
import Data.List.Split
import qualified Data.ByteString.Char8 as BS

import Control.Lens

import HsSyn as GHC
import Module as GHC
import RdrName as GHC
import Id as GHC
import Name as GHC hiding (Name)
import qualified Name as GHC (Name)
import Outputable as GHC
import SrcLoc as GHC
import BasicTypes as GHC
import FastString as GHC
import ApiAnnotation as GHC
import ForeignCall as GHC

import Language.Haskell.Tools.AST.Ann as AST
import Language.Haskell.Tools.AST.Lenses as AST
import qualified Language.Haskell.Tools.AST.Base as AST
import qualified Language.Haskell.Tools.AST.Literals as AST
import Language.Haskell.Tools.AST.Base(Name(..), SimpleName(..))

import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

class Annot (AnnotType name) => TransformName name where
  type AnnotType name :: *
  rdrName :: name -> RdrName
  trfName :: Located name -> Trf (Ann Name (AnnotType name))
  
instance TransformName RdrName where
  type AnnotType RdrName = RI
  rdrName = id
  trfName = trfLoc trfName' 
  
data SourceAndName
  = SN { snName :: Maybe GHC.Name
       , snRange :: RI
       }

instance Annot SourceAndName where
  createAnnot = SN Nothing
  extractRange = snRange
       
snSetName :: GHC.Name -> SourceAndName -> SourceAndName
snSetName name sn = sn { snName = Just name }
  
instance TransformName GHC.Name where
  type AnnotType GHC.Name = SourceAndName
  rdrName = nameRdrName
  trfName name = (annotation %~ snSetName (unLoc name)) <$> trfLoc trfName' name
  
data SourceAndType
  = ST { stName :: Maybe GHC.Id
       , stRange :: RI
       }
       
instance Annot SourceAndType where
  createAnnot = ST Nothing
  extractRange = stRange
       
stSetName :: GHC.Id -> SourceAndType -> SourceAndType
stSetName name st = st { stName = Just name }
  
instance TransformName GHC.Id where
  type AnnotType GHC.Id = SourceAndType
  rdrName = nameRdrName . idName
  trfName name = (annotation %~ stSetName (unLoc name)) <$> trfLoc trfName' name
  
  
trfName' :: TransformName name => name -> Trf (Name (AnnotType name))
trfName' n = AST.nameFromList . fst <$> trfNameStr (occNameString (rdrNameOcc (rdrName n)))
  
trfSimplName :: Annot a => SrcLoc -> OccName -> Trf (Ann SimpleName a)
trfSimplName start n = (\srcLoc -> Ann (createAnnot $ mkSrcSpan start srcLoc) $ SimpleName (pprStr n)) <$> asks (srcSpanEnd . contRange)

trfNameStr :: Annot a => String -> Trf (AnnList SimpleName a, SrcLoc)
trfNameStr str = (\srcLoc -> (\(ls,loc) -> (AnnList ls, loc))
  (foldl (\(r,loc) np -> let nextLoc = advanceAllSrcLoc loc np
                          in ( r ++ [Ann (createAnnot $ mkSrcSpan loc nextLoc) (SimpleName np)], advanceAllSrcLoc nextLoc "." ) ) 
  ([],srcLoc) (splitOn "." str))) <$> asks (srcSpanStart . contRange)

  
trfModuleName :: Annot a => Located ModuleName -> Trf (Ann Name a)
trfModuleName = trfLoc trfModuleName'

trfModuleName' :: Annot a => ModuleName -> Trf (Name a)
trfModuleName' = (AST.nameFromList . fst <$>) . trfNameStr . moduleNameString
  
trfDataKeyword :: Annot a => NewOrData -> Trf (Ann AST.DataOrNewtypeKeyword a)
trfDataKeyword NewType = annLoc (tokenLoc AnnNewtype) (pure AST.NewtypeKeyword)
trfDataKeyword DataType = annLoc (tokenLoc AnnData) (pure AST.DataKeyword)
     
trfCallConv :: Located CCallConv -> Trf (Ann AST.CallConv a)
trfCallConv = undefined      
   
trfCallConv' :: CCallConv -> Trf (AST.CallConv a)
trfCallConv' = undefined 

trfSafety :: Located Safety -> Trf (AnnMaybe AST.Safety a)
trfSafety = undefined 

trfOverlap :: Annot a => Located OverlapMode -> Trf (Ann AST.OverlapPragma a)
trfOverlap = trfLoc $ pure . \case
  NoOverlap _ -> AST.DisableOverlap
  Overlappable _ -> AST.Overlappable
  Overlapping _ -> AST.Overlapping
  Overlaps _ -> AST.Overlaps
  Incoherent _ -> AST.IncoherentOverlap
          
          