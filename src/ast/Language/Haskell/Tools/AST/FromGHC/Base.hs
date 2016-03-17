{-# LANGUAGE LambdaCase
           , TupleSections
           , TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , ScopedTypeVariables
           , MultiParamTypeClasses
           #-}
module Language.Haskell.Tools.AST.FromGHC.Base where

import Control.Monad.Reader
import Data.List.Split
import qualified Data.ByteString.Char8 as BS

import Control.Reference hiding (element)

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
import qualified Language.Haskell.Tools.AST.Base as AST
import qualified Language.Haskell.Tools.AST.Literals as AST
import Language.Haskell.Tools.AST.Base(Name(..), SimpleName(..))

import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

class GHCName name where 
  rdrName :: name -> RdrName
  
instance GHCName RdrName where
  rdrName = id
    
instance GHCName GHC.Name where
  rdrName = nameRdrName
  
instance GHCName Id where
  rdrName = nameRdrName . idName
  
-- | This class allows us to use the same transformation code for multiple variants of the GHC AST.
-- GHC Name annotated with 'name' can be transformed to our representation with semantic annotations of 'res'.
class (RangeAnnot res, GHCName name) => TransformName name res where
  trfName :: Located name -> Trf (Ann Name res)
  
instance TransformName RdrName RangeInfo where
  trfName = trfLoc trfName' 

instance RangeAnnot r => TransformName GHC.Name r where
  trfName name = (annotation .- addSemanticInfo (NameInfo (unLoc name))) <$> trfLoc trfName' name
  
instance TransformName GHC.Id RangeWithName where
  trfName name = (annotation&semanticInfo .= (NameInfo $ idName (unLoc name))) <$> trfLoc trfName' name
  
trfName' :: TransformName name res => name -> Trf (Name res)
trfName' n = AST.nameFromList . fst <$> trfNameStr (occNameString (rdrNameOcc (rdrName n)))
  
trfNameSp :: TransformName name res => name -> SrcSpan -> Trf (Ann Name res)
trfNameSp n l = trfName (L l n)

trfNameSp' :: TransformName name res => name -> Trf (Ann Name res)
trfNameSp' n = trfNameSp n =<< asks contRange
  
trfSimplName :: RangeAnnot a => SrcLoc -> OccName -> Trf (Ann SimpleName a)
trfSimplName start n = (\srcLoc -> Ann (toNodeAnnot $ mkSrcSpan start srcLoc) $ SimpleName (pprStr n)) <$> asks (srcSpanEnd . contRange)
  where pprStr :: Outputable a => a -> String
        pprStr = showSDocUnsafe . ppr
                

trfNameStr :: RangeAnnot a => String -> Trf (AnnList SimpleName a, SrcLoc)
trfNameStr str = (\srcLoc -> (\(ls,loc) -> (AnnList (toListAnnot "." srcLoc) ls, loc))
  (foldl (\(r,loc) np -> let nextLoc = advanceAllSrcLoc loc np
                          in ( r ++ [Ann (toNodeAnnot $ mkSrcSpan loc nextLoc) (SimpleName np)], advanceAllSrcLoc nextLoc "." ) ) 
  ([],srcLoc) (splitOn "." str))) <$> asks (srcSpanStart . contRange)
  where advanceAllSrcLoc :: SrcLoc -> String -> SrcLoc
        advanceAllSrcLoc (RealSrcLoc rl) str = RealSrcLoc $ foldl advanceSrcLoc rl str
        advanceAllSrcLoc oth _ = oth
  
  
trfModuleName :: RangeAnnot a => Located ModuleName -> Trf (Ann Name a)
trfModuleName = trfLoc trfModuleName'

trfModuleName' :: RangeAnnot a => ModuleName -> Trf (Name a)
trfModuleName' = (AST.nameFromList . fst <$>) . trfNameStr . moduleNameString
  
trfDataKeyword :: RangeAnnot a => NewOrData -> Trf (Ann AST.DataOrNewtypeKeyword a)
trfDataKeyword NewType = annLoc (tokenLoc AnnNewtype) (pure AST.NewtypeKeyword)
trfDataKeyword DataType = annLoc (tokenLoc AnnData) (pure AST.DataKeyword)
     
trfCallConv :: Located CCallConv -> Trf (Ann AST.CallConv a)
trfCallConv = error "trfCallConv"      
   
trfCallConv' :: CCallConv -> Trf (AST.CallConv a)
trfCallConv' = error "trfCallConv'" 

trfSafety :: Located Safety -> Trf (AnnMaybe AST.Safety a)
trfSafety = error "trfSafety" 

trfOverlap :: RangeAnnot a => Located OverlapMode -> Trf (Ann AST.OverlapPragma a)
trfOverlap = trfLoc $ pure . \case
  NoOverlap _ -> AST.DisableOverlap
  Overlappable _ -> AST.Overlappable
  Overlapping _ -> AST.Overlapping
  Overlaps _ -> AST.Overlaps
  Incoherent _ -> AST.IncoherentOverlap
         

          