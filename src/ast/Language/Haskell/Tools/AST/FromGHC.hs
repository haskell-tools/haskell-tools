{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Tools.AST.FromGHC where

import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Base as AST
import qualified Language.Haskell.Tools.AST.Literals as AST
import qualified Language.Haskell.Tools.AST.Decl as AST
import qualified Language.Haskell.Tools.AST.Module as AST
import Language.Haskell.Tools.AST.Base(Name(..), SimpleName(..))
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Data.Maybe
import Control.Monad.Reader

import HsSyn as GHC
import Module as GHC
import SrcLoc as GHC
import RdrName as GHC
import Name as GHC hiding (Name)
import BasicTypes as GHC
import Outputable as GHC
import FastString as GHC
import ApiAnnotation

import Data.List.Split


trfModule :: Located (HsModule RdrName) -> Trf (Ann AST.Module RI)
trfModule = trfLoc $ \(HsModule name exports imports decls deprec haddock) -> 
  AST.Module <$> trfModuleHead name exports
             <*> trfPragmas deprec haddock
             <*> trfImports imports
             <*> trfDecls decls
       
trfModuleHead :: Maybe (Located ModuleName) -> Maybe (Located [LIE RdrName]) -> Trf (AnnMaybe AST.ModuleHead RI) 
trfModuleHead (Just mn) exports 
  = annJust <$> (noAnn <$> (AST.ModuleHead <$> trfModuleNameL mn <*> trfExportList exports))
trfModuleHead Nothing _ = pure annNothing

trfPragmas :: Maybe (Located WarningTxt) -> Maybe LHsDocString -> Trf (AnnList AST.ModulePragma RI)
trfPragmas = undefined

trfExportList :: Maybe (Located [LIE RdrName]) -> Trf (AnnMaybe AST.ExportSpecList RI)
trfExportList Nothing = pure annNothing
trfExportList (Just (L l exps)) = annJust . Ann l . AST.ExportSpecList . AnnList . catMaybes <$> (mapM trfExport exps)
  
trfExport :: LIE RdrName -> Trf (Maybe (Ann AST.ExportSpec RI))
trfExport = trfMaybeLoc $ \case 
  IEVar n -> Just . AST.DeclExport <$> (AST.IESpec <$> trfName n <*> pure annNothing)
  IEThingAbs n -> Just . AST.DeclExport <$> (AST.IESpec <$> trfName n <*> pure annNothing)
  IEThingAll n -> Just . AST.DeclExport <$> (AST.IESpec <$> trfName n <*> (annJust <$> (Ann <$> tokenLoc AnnDotdot <*> pure AST.SubSpecAll)))
  IEThingWith n ls
    -> Just . AST.DeclExport <$> (AST.IESpec <$> trfName n
                                             <*> (annJust . noAnn . AST.SubSpecList . AnnList <$> mapM trfName ls))
  IEModuleContents n
    -> Just . AST.ModuleExport <$> (trfModuleNameL n)
  _ -> pure $ Nothing -- documentation. TODO: unify ranges
  
trfImports :: [LImportDecl RdrName] -> Trf (AnnList AST.ImportDecl RI)
trfImports imps = AnnList <$> mapM trfImport imps

trfImport :: LImportDecl RdrName -> Trf (Ann AST.ImportDecl RI)
trfImport (L l (GHC.ImportDecl src name pkg isSrc isSafe isQual isImpl declAs declHiding))
  = undefined -- AST.ImportDecl (if isQual then ImportQualified )

trfDecls :: [LHsDecl RdrName] -> Trf (AnnList AST.Decl RI)
trfDecls = undefined
  
trfName :: Located RdrName -> Trf (Ann Name RI)
trfName = trfLoc $ \case 
  Unqual n -> Name (AnnList []) <$> trfSimplName n
  Qual mn n -> Name <$> trfModuleName mn <*> trfSimplName n
  Orig m n -> Name <$> trfModuleName (moduleName m) <*> trfSimplName n
  Exact n -> Name <$> maybe (pure (AnnList [])) (trfModuleName . moduleName) (nameModule_maybe n)
                  <*> trfSimplName (nameOccName n)

trfSimplName :: OccName -> Trf (Ann SimpleName RI)
trfSimplName n = pure $ noAnn $ SimpleName (pprStr n)

trfModuleName :: ModuleName -> Trf (AnnList SimpleName RI)
trfModuleName mn = pure $ AnnList (map (noAnn . SimpleName) 
                                  (splitOn "." (moduleNameString mn)))

trfModuleNameL :: Located ModuleName -> Trf (Ann Name RI)
trfModuleNameL = trfLoc $ \mn -> trfModuleName mn >>= \case 
  AnnList xs | not (null xs) -> pure $ Name (AnnList $ init xs) (last xs) 
  _ -> error "Located ModuleName empty"
     

                
pprStr :: Outputable a => a -> String
pprStr = showSDocUnsafe . ppr
                
                
