{-# LANGUAGE LambdaCase
           , ViewPatterns
           #-}
module Language.Haskell.Tools.AST.FromGHC.Modules where

import Control.Lens hiding (element)
import Data.Maybe
import Data.IORef
import Control.Monad.Reader

import Avail as GHC
import GHC as GHC
import ApiAnnotation as GHC
import RdrName as GHC
import Name as GHC
import Id as GHC
import SrcLoc as GHC
import FastString as GHC
import Module as GHC
import BasicTypes as GHC
import HsSyn as GHC
import HscTypes as GHC

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Lenses
import qualified Language.Haskell.Tools.AST.Base as AST
import qualified Language.Haskell.Tools.AST.Modules as AST

import Language.Haskell.Tools.AST.Helpers
import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Decls
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

trfModule :: Located (HsModule RdrName) -> Trf (Ann AST.Module RangeInfo)
trfModule = trfLocCorrect (\sr -> combineSrcSpans sr <$> (uniqueTokenAnywhere AnnEofPos)) $ 
  \(HsModule name exports imports decls deprec haddock) -> 
    AST.Module <$> trfModuleHead name exports
               <*> trfPragmas deprec haddock
               <*> trfImports imports
               <*> trfDecls decls
       
trfModuleRename :: (HsGroup Name, [LImportDecl Name], Maybe [LIE Name], Maybe LHsDocString) -> Located (HsModule RdrName) -> Trf (Ann AST.Module RangeWithName)
trfModuleRename (gr,imports,exps,_) 
  = trfLocCorrect (\sr -> combineSrcSpans sr <$> (uniqueTokenAnywhere AnnEofPos)) $ 
      \(HsModule name exports _ decls deprec haddock) -> 
        AST.Module <$> trfModuleHead name (case (exports, exps) of (Just (L l _), Just ie) -> Just (L l ie)
                                                                   _                       -> Nothing)
                   <*> trfPragmas deprec haddock
                   <*> (orderAnnList <$> (trfImports imports))
                   <*> trfDeclsGroup gr
       
trfModuleHead :: TransformName n r => Maybe (Located ModuleName) -> Maybe (Located [LIE n]) -> Trf (AnnMaybe AST.ModuleHead r) 
trfModuleHead (Just mn) exports 
  = annJust <$> (annLoc (tokensLoc [AnnModule, AnnWhere])
                        (AST.ModuleHead <$> trfModuleName mn 
                                        <*> trfExportList exports))
trfModuleHead Nothing _ = pure annNothing

trfPragmas :: Maybe (Located WarningTxt) -> Maybe LHsDocString -> Trf (AnnList AST.ModulePragma a)
trfPragmas _ _ = pure $ AnnList []

trfExportList :: TransformName n r => Maybe (Located [LIE n]) -> Trf (AnnMaybe AST.ExportSpecList r)
trfExportList = trfMaybe $ trfLoc (\exps -> AST.ExportSpecList . AnnList . orderDefs . catMaybes <$> (mapM trfExport exps))
  
trfExport :: TransformName n r => LIE n -> Trf (Maybe (Ann AST.ExportSpec r))
trfExport = trfMaybeLoc $ \case 
  IEModuleContents n -> Just . AST.ModuleExport <$> (trfModuleName n)
  other -> fmap AST.DeclExport <$> trfIESpec' other
  
trfImports :: TransformName n r => [LImportDecl n] -> Trf (AnnList AST.ImportDecl r)
trfImports imps = AnnList <$> mapM trfImport (filter (not . ideclImplicit . unLoc) imps)

trfImport :: TransformName n r => LImportDecl n -> Trf (Ann AST.ImportDecl r)
trfImport = (addImportData <=<) $ trfLoc $ \(GHC.ImportDecl src name pkg isSrc isSafe isQual isImpl declAs declHiding) ->
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
  
trfImportSpecs :: TransformName n r => Maybe (Bool, Located [LIE n]) -> Trf (AnnMaybe AST.ImportSpec r)
trfImportSpecs (Just (True, l)) = annJust <$> trfLoc (fmap (AST.ImportSpecHiding . AnnList . catMaybes) . mapM trfIESpec) l
trfImportSpecs (Just (False, l)) = annJust <$> trfLoc (fmap (AST.ImportSpecList . AnnList . catMaybes) . mapM trfIESpec) l
trfImportSpecs Nothing = pure annNothing
    
trfIESpec :: TransformName n r => LIE n -> Trf (Maybe (Ann AST.IESpec r)) 
trfIESpec = trfMaybeLoc trfIESpec'
  
trfIESpec' :: TransformName n r => IE n -> Trf (Maybe (AST.IESpec r))
trfIESpec' (IEVar n) = Just <$> (AST.IESpec <$> trfName n <*> pure annNothing)
trfIESpec' (IEThingAbs n) = Just <$> (AST.IESpec <$> trfName n <*> pure annNothing)
trfIESpec' (IEThingAll n) 
  = Just <$> (AST.IESpec <$> trfName n <*> (annJust <$> (annLoc (tokenLoc AnnDotdot) (pure AST.SubSpecAll))))
trfIESpec' (IEThingWith n ls)
  = Just <$> (AST.IESpec <$> trfName n
                         <*> (annJust <$> annLoc (tokensLoc [AnnOpenP, AnnCloseP]) 
                                                 (AST.SubSpecList . AnnList <$> mapM trfName ls)))
trfIESpec' _ = pure Nothing
  
 