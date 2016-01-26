{-# LANGUAGE LambdaCase
           , TupleSections
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

trfName :: Located RdrName -> Trf (Ann Name RI)
trfName = trfLoc trfName'

trfName' :: RdrName -> Trf (Name RI)
trfName' n = AST.nameFromList . fst <$> trfNameStr (occNameString (rdrNameOcc n))

trfSimplName :: SrcLoc -> OccName -> Trf (Ann SimpleName RI)
trfSimplName start n = (\srcLoc -> Ann (mkSrcSpan start srcLoc) $ SimpleName (pprStr n)) <$> asks (srcSpanEnd . contRange)

trfNameStr :: String -> Trf (AnnList SimpleName RI, SrcLoc)
trfNameStr str = (\srcLoc -> (\(ls,loc) -> (AnnList ls, loc))
  (foldl (\(r,loc) np -> let nextLoc = advanceAllSrcLoc loc np
                          in ( r ++ [Ann (mkSrcSpan loc nextLoc) (SimpleName np)], advanceAllSrcLoc nextLoc "." ) ) 
  ([],srcLoc) (splitOn "." str))) <$> asks (srcSpanStart . contRange)

  
trfModuleName :: Located ModuleName -> Trf (Ann Name RI)
trfModuleName = trfLoc trfModuleName'

trfModuleName' :: ModuleName -> Trf (Name RI)
trfModuleName' = (AST.nameFromList . fst <$>) . trfNameStr . moduleNameString
  
trfDataKeyword :: NewOrData -> Trf (Ann AST.DataOrNewtypeKeyword RI)
trfDataKeyword NewType = annLoc (tokenLoc AnnNewtype) (pure AST.NewtypeKeyword)
trfDataKeyword DataType = annLoc (tokenLoc AnnData) (pure AST.DataKeyword)
     
trfCallConv :: Located CCallConv -> Trf (Ann AST.CallConv RI)
trfCallConv = undefined      
   
trfCallConv' :: CCallConv -> Trf (AST.CallConv RI)
trfCallConv' = undefined 

trfSafety :: Located Safety -> Trf (AnnMaybe AST.Safety RI)
trfSafety = undefined 

trfOverlap :: Located OverlapMode -> Trf (Ann AST.OverlapPragma RI)
trfOverlap = trfLoc $ pure . \case
  NoOverlap _ -> AST.DisableOverlap
  Overlappable _ -> AST.Overlappable
  Overlapping _ -> AST.Overlapping
  Overlaps _ -> AST.Overlaps
  Incoherent _ -> AST.IncoherentOverlap
          
          