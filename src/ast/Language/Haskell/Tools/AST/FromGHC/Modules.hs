{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Tools.AST.FromGHC.Modules where

import Data.Maybe
import Control.Monad.Reader

import ApiAnnotation as GHC
import RdrName as GHC
import SrcLoc as GHC
import FastString as GHC
import Module as GHC
import BasicTypes as GHC
import HsSyn as GHC

import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Base as AST
import qualified Language.Haskell.Tools.AST.Modules as AST

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Decls
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

trfModule :: TransformName n => Located (HsModule n) -> Trf (Ann AST.Module (AnnotType n))
trfModule = trfLocCorrect (\sr -> combineSrcSpans sr <$> (uniqueTokenAnywhere AnnEofPos)) $ 
  \(HsModule name exports imports decls deprec haddock) -> 
    AST.Module <$> trfModuleHead name exports
               <*> trfPragmas deprec haddock
               <*> trfImports imports
               <*> trfDecls decls
       
trfModuleHead :: TransformName n => Maybe (Located ModuleName) -> Maybe (Located [LIE n]) -> Trf (AnnMaybe AST.ModuleHead (AnnotType n)) 
trfModuleHead (Just mn) exports 
  = annJust <$> (annLoc (tokensLoc [AnnModule, AnnWhere])
                        (AST.ModuleHead <$> trfModuleName mn 
                                        <*> trfExportList exports))
trfModuleHead Nothing _ = pure annNothing

trfPragmas :: Maybe (Located WarningTxt) -> Maybe LHsDocString -> Trf (AnnList AST.ModulePragma a)
trfPragmas _ _ = pure $ AnnList []

trfExportList :: TransformName n => Maybe (Located [LIE n]) -> Trf (AnnMaybe AST.ExportSpecList (AnnotType n))
trfExportList = trfMaybe $ trfLoc (\exps -> AST.ExportSpecList . AnnList . catMaybes <$> (mapM trfExport exps))
  
trfExport :: TransformName n => LIE n -> Trf (Maybe (Ann AST.ExportSpec (AnnotType n)))
trfExport = trfMaybeLoc $ \case 
  IEModuleContents n -> Just . AST.ModuleExport <$> (trfModuleName n)
  other -> fmap AST.DeclExport <$> trfIESpec' other
  
trfImports :: TransformName n => [LImportDecl n] -> Trf (AnnList AST.ImportDecl (AnnotType n))
trfImports imps = AnnList <$> mapM trfImport imps

trfImport :: TransformName n => LImportDecl n -> Trf (Ann AST.ImportDecl (AnnotType n))
trfImport = trfLoc $ \(GHC.ImportDecl src name pkg isSrc isSafe isQual isImpl declAs declHiding) ->
  AST.ImportDecl 
    <$> (if isQual then annJust <$> (annLoc (tokenLoc AnnQualified) (pure AST.ImportQualified)) else pure annNothing)
    -- if there is a source annotation the first open and close will mark its location
    <*> (if isSrc then annJust <$> (annLoc (tokensLoc [AnnOpen, AnnClose]) 
                                           (pure AST.ImportSource))
                  else pure annNothing)
    <*> (if isSafe then annJust <$> (annLoc (tokenLoc AnnSafe) (pure AST.ImportSafe)) else pure annNothing)
    <*> maybe (pure annNothing) (\str -> annJust <$> (annLoc (tokenLoc AnnPackageName) (pure (AST.StringNode (unpackFS str))))) pkg
    <*> trfModuleName name 
    <*> maybe (pure annNothing) (\mn -> annJust <$> (trfRenaming mn)) declAs
    <*> trfImportSpecs declHiding
  where trfRenaming mn
          = annLoc (tokensLoc [AnnAs,AnnVal])
                   (AST.ImportRenaming <$> (annLoc (tokenLoc AnnVal) 
                                           (trfModuleName' mn)))
  
  
trfImportSpecs :: TransformName n => Maybe (Bool, Located [LIE n]) -> Trf (AnnMaybe AST.ImportSpec (AnnotType n))
trfImportSpecs (Just (True, l)) = annJust <$> trfLoc (fmap (AST.ImportSpecHiding . AnnList . catMaybes) . mapM trfIESpec) l
trfImportSpecs (Just (False, l)) = annJust <$> trfLoc (fmap (AST.ImportSpecList . AnnList . catMaybes) . mapM trfIESpec) l
trfImportSpecs Nothing = pure annNothing
    
trfIESpec :: TransformName n => LIE n -> Trf (Maybe (Ann AST.IESpec (AnnotType n))) 
trfIESpec = trfMaybeLoc trfIESpec'
  
trfIESpec' :: TransformName n => IE n -> Trf (Maybe (AST.IESpec (AnnotType n)))
trfIESpec' (IEVar n) = Just <$> (AST.IESpec <$> trfName n <*> pure annNothing)
trfIESpec' (IEThingAbs n) = Just <$> (AST.IESpec <$> trfName n <*> pure annNothing)
trfIESpec' (IEThingAll n) 
  = Just <$> (AST.IESpec <$> trfName n <*> (annJust <$> (annLoc (tokenLoc AnnDotdot) (pure AST.SubSpecAll))))
trfIESpec' (IEThingWith n ls)
  = Just <$> (AST.IESpec <$> trfName n
                         <*> (annJust <$> annLoc (tokensLoc [AnnOpenP, AnnCloseP]) 
                                                 (AST.SubSpecList . AnnList <$> mapM trfName ls)))
trfIESpec' _ = pure Nothing
  
 