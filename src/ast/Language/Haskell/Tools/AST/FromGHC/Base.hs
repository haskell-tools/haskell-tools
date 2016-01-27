{-# LANGUAGE LambdaCase
           , TupleSections
           , TypeFamilies
           , FlexibleContexts
           #-}
module Language.Haskell.Tools.AST.FromGHC.Base where

import Control.Monad.Reader
import Data.List.Split
import qualified Data.ByteString.Char8 as BS

import HsSyn as GHC
import Module as GHC
import RdrName as GHC
import Name as GHC hiding (Name)
import Outputable as GHC
import SrcLoc as GHC
import BasicTypes as GHC
import FastString as GHC
import ApiAnnotation as GHC
import ForeignCall as GHC

import Language.Haskell.Tools.AST.Ann as AST
import qualified Language.Haskell.Tools.AST.Base as AST
import qualified Language.Haskell.Tools.AST.Literals as AST
import Language.Haskell.Tools.AST.Base(Name(..), SimpleName(..))

import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

class Annot (AnnotType name) => TransformName name where
  type AnnotType name :: *
  rdrName :: name -> RdrName
  trfName :: Located name -> Trf (Ann Name (AnnotType name))
  trfName' :: name -> Trf (Name (AnnotType name))
  
instance TransformName RdrName where
  type AnnotType RdrName = RI
  rdrName = id
  trfName = trfLoc trfName' 
  trfName' n = AST.nameFromList . fst <$> trfNameStr (occNameString (rdrNameOcc n))

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
          
          