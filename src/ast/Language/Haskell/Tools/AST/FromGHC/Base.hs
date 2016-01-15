{-# LANGUAGE LambdaCase
           , TupleSections
           #-}
module Language.Haskell.Tools.AST.FromGHC.Base where

import Control.Monad.Reader
import Data.List.Split

import HsSyn as GHC
import Module as GHC
import RdrName as GHC
import Name as GHC hiding (Name)
import Outputable as GHC
import SrcLoc as GHC

import Language.Haskell.Tools.AST.Ann as AST
import qualified Language.Haskell.Tools.AST.Base as AST
import Language.Haskell.Tools.AST.Base(Name(..), SimpleName(..))

import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

trfName :: Located RdrName -> Trf (Ann Name RI)
trfName = trfLoc $ \case 
  Unqual n -> do begin <- asks (srcSpanStart . contRange)
                 Name (AnnList []) <$> (trfSimplName begin n)
  Qual mn n -> do (qual,loc) <- trfModuleName mn
                  unqual <- trfSimplName loc n
                  return (Name qual unqual)
  Orig m n -> do (qual,loc) <- trfModuleName (moduleName m)
                 unqual <- trfSimplName loc n
                 return (Name qual unqual)
  Exact n -> do (qual,loc) <- maybe ((AnnList [],) <$> asks (srcSpanEnd . contRange)) 
                                    (trfModuleName . moduleName) 
                                    (nameModule_maybe n)
                unqual <- trfSimplName loc (nameOccName n)
                return (Name qual unqual)

trfSimplName :: SrcLoc -> OccName -> Trf (Ann SimpleName RI)
trfSimplName start n = (\srcLoc -> Ann (mkSrcSpan start srcLoc) $ SimpleName (pprStr n)) <$> asks (srcSpanEnd . contRange)

trfModuleName :: ModuleName -> Trf (AnnList SimpleName RI, SrcLoc)
trfModuleName mn = (\srcLoc -> (\(ls,loc) -> (AnnList ls, loc))
  (foldl (\(r,loc) np -> let nextLoc = advanceAllSrcLoc loc np
                          in ( r ++ [Ann (mkSrcSpan loc nextLoc) (SimpleName np)], advanceAllSrcLoc nextLoc "." ) ) 
  ([],srcLoc) (splitOn "." (moduleNameString mn)))) <$> asks (srcSpanStart . contRange)

advanceAllSrcLoc :: SrcLoc -> String -> SrcLoc
advanceAllSrcLoc (RealSrcLoc rl) str = RealSrcLoc $ foldl advanceSrcLoc rl str
advanceAllSrcLoc oth _ = oth
  
trfModuleNameL :: Located ModuleName -> Trf (Ann Name RI)
trfModuleNameL = trfLoc ((AST.nameFromList . fst <$>) . trfModuleName)
     
pprStr :: Outputable a => a -> String
pprStr = showSDocUnsafe . ppr
                
                