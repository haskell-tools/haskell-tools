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
trfModule = trfLocCorrect (\sr -> combineSrcSpans sr <$> (uniqueTokenAnywhere AnnEofPos)) $ 
  \(HsModule name exports imports decls deprec haddock) -> 
    AST.Module <$> trfModuleHead name exports
               <*> trfPragmas deprec haddock
               <*> trfImports imports
               <*> trfDecls decls
       
trfModuleHead :: Maybe (Located ModuleName) -> Maybe (Located [LIE RdrName]) -> Trf (AnnMaybe AST.ModuleHead RI) 
trfModuleHead (Just mn) exports 
  = annJust <$> (Ann <$> tokensLoc [AnnModule, AnnWhere] 
                     <*> (AST.ModuleHead <$> trfModuleNameL mn 
                                         <*> trfExportList exports))
trfModuleHead Nothing _ = pure annNothing

trfPragmas :: Maybe (Located WarningTxt) -> Maybe LHsDocString -> Trf (AnnList AST.ModulePragma RI)
trfPragmas _ _ = pure $ AnnList []

trfExportList :: Maybe (Located [LIE RdrName]) -> Trf (AnnMaybe AST.ExportSpecList RI)
trfExportList Nothing = pure annNothing
trfExportList (Just (L l exps)) = annJust . Ann l . AST.ExportSpecList . AnnList . catMaybes <$> (mapM trfExport exps)
  
trfExport :: LIE RdrName -> Trf (Maybe (Ann AST.ExportSpec RI))
trfExport = trfMaybeLoc $ \case 
  IEModuleContents n -> Just . AST.ModuleExport <$> (trfModuleNameL n)
  other -> fmap AST.DeclExport <$> trfIESpec' other
  
trfImports :: [LImportDecl RdrName] -> Trf (AnnList AST.ImportDecl RI)
trfImports imps = AnnList <$> mapM trfImport imps

trfImport :: LImportDecl RdrName -> Trf (Ann AST.ImportDecl RI)
trfImport = trfLoc $ \(GHC.ImportDecl src name pkg isSrc isSafe isQual isImpl declAs declHiding) ->
  AST.ImportDecl 
    <$> (if isQual then annJust <$> (Ann <$> tokenLoc AnnQualified <*> pure AST.ImportQualified) else pure annNothing)
    -- if there is a source annotation the first open and close will mark its location
    <*> (if isSrc then annJust <$> (Ann <$> (combineSrcSpans <$> tokenLoc AnnOpen <*> tokenLoc AnnClose) 
                                        <*> pure AST.ImportSource)
                  else pure annNothing)
    <*> (if isSafe then annJust <$> (Ann <$> tokenLoc AnnSafe <*> pure AST.ImportSafe) else pure annNothing)
    <*> maybe (pure annNothing) (\str -> annJust <$> (Ann <$> tokenLoc AnnPackageName <*> pure (AST.StringNode (unpackFS str)))) pkg
    <*> trfModuleNameL name 
    <*> maybe (pure annNothing) (\mn -> annJust <$> (Ann <$> tokensLoc [AnnAs,AnnVal] 
                                                         <*> (AST.ImportRenaming <$> (Ann <$> tokenLoc AnnVal 
                                                                                          <*> (AST.nameFromList <$> trfModuleName mn))))) declAs
    <*> trfImportSpecs declHiding
  
trfImportSpecs :: Maybe (Bool, Located [LIE RdrName]) -> Trf (AnnMaybe AST.ImportSpec RI)
trfImportSpecs (Just (True, l)) = annJust <$> trfLoc (fmap (AST.ImportSpecHiding . AnnList . catMaybes) . mapM trfIESpec) l
trfImportSpecs (Just (False, l)) = annJust <$> trfLoc (fmap (AST.ImportSpecList . AnnList . catMaybes) . mapM trfIESpec) l
trfImportSpecs Nothing = pure annNothing
    
trfIESpec :: LIE RdrName -> Trf (Maybe (Ann AST.IESpec RI)) 
trfIESpec = trfMaybeLoc trfIESpec'
  
trfIESpec' :: IE RdrName -> Trf (Maybe (AST.IESpec RI))
trfIESpec' (IEVar n) = Just <$> (AST.IESpec <$> trfName n <*> pure annNothing)
trfIESpec' (IEThingAbs n) = Just <$> (AST.IESpec <$> trfName n <*> pure annNothing)
trfIESpec' (IEThingAll n) 
  = Just <$> (AST.IESpec <$> trfName n <*> (annJust <$> (Ann <$> tokenLoc AnnDotdot <*> pure AST.SubSpecAll)))
trfIESpec' (IEThingWith n ls)
  = Just <$> (AST.IESpec <$> trfName n
                         <*> (annJust . noAnn . AST.SubSpecList . AnnList <$> mapM trfName ls))
trfIESpec' _ = pure Nothing
  
    
trfDecls :: [LHsDecl RdrName] -> Trf (AnnList AST.Decl RI)
trfDecls _ = pure $ AnnList []
  
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
trfModuleNameL = trfLoc ((AST.nameFromList <$>) . trfModuleName)
     
pprStr :: Outputable a => a -> String
pprStr = showSDocUnsafe . ppr
                
                
