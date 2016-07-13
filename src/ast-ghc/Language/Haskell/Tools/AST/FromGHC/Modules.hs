{-# LANGUAGE LambdaCase
           , ViewPatterns
           , FlexibleContexts
           , ScopedTypeVariables
           , TypeApplications
           #-}
module Language.Haskell.Tools.AST.FromGHC.Modules where

import Control.Reference hiding (element)
import Data.Maybe
import Data.List
import Data.Char
import Data.Map as Map hiding (map, filter)
import Data.IORef
import Data.Data
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data
import Data.StructuralTraversal
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import Avail as GHC
import GHC as GHC
import GhcMonad as GHC
import ApiAnnotation as GHC
import RdrName as GHC
import Name as GHC hiding (varName)
import Id as GHC
import TysWiredIn as GHC
import SrcLoc as GHC
import FastString as GHC
import Module as GHC
import BasicTypes as GHC
import HsSyn as GHC
import HscTypes as GHC
import Outputable as GHC
import TyCon as GHC
import ConLike as GHC
import DataCon as GHC
import Bag as GHC
import Var as GHC
import PatSyn as GHC
import Type as GHC
import Unique as GHC
import CoAxiom as GHC
import DynFlags as GHC
import Language.Haskell.TH.LanguageExtensions

import Language.Haskell.Tools.AST (Ann(..), AnnMaybe(..), AnnList(..), Dom, IdDom, RangeStage, NoSemanticInfo(..), NameInfo(..), CNameInfo(..), ScopeInfo(..), ImportInfo(..), ModuleInfo(..)
                                  , semanticInfo, sourceInfo, semantics, annotation, nameInfo, nodeSpan, semaTraverse)
import qualified Language.Haskell.Tools.AST as AST

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Decls
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.GHCUtils

addTypeInfos :: LHsBinds Id -> Ann AST.Module (Dom GHC.Name) RangeStage -> Ghc (Ann AST.Module IdDom RangeStage)
addTypeInfos bnds = semaTraverse 
  (AST.SemaTrf
    (\case (NameInfo sc def ni) -> CNameInfo sc def <$> getType' ni 
           (AmbiguousNameInfo sc d rdr l) -> return $ CNameInfo sc d (locMapping ! l))
    pure
    (\(ImportInfo mod access used) -> ImportInfo mod <$> mapM getType' access <*> mapM getType' used)
    (\(ModuleInfo mod imps) -> ModuleInfo mod <$> mapM getType' imps)
    pure)
  where locMapping = Map.fromList $ map (\(L l id) -> (l, id)) $ extractExprIds bnds
        getType' name = fromMaybe (error $ "Type of name '" ++ showSDocUnsafe (ppr name) ++ "' cannot be found")
                            <$> ((<|>) <$> getTopLevelId name <*> getLocalId bnds name)

extractExprIds :: LHsBinds Id -> [Located Id]
        -- expressions like HsRecFld are removed from the typechecked representation, they are replaced by HsVar
extractExprIds = catMaybes . map (\case (L l (HsVar (L _ n))) -> Just (L l n); _ -> Nothing) . concatMap universeBi . bagToList

createModuleInfo :: Module -> Trf (AST.ModuleInfo GHC.Name)
createModuleInfo mod = do prelude <- lift (xopt ImplicitPrelude . ms_hspp_opts <$> getModSummary (moduleName mod))
                          (_,preludeImports) <- if prelude then getImportedNames "Prelude" Nothing else return (mod, [])
                          return $ AST.ModuleInfo mod preludeImports

trfModule :: Module -> Located (HsModule RdrName) -> Trf (Ann AST.Module (Dom RdrName) RangeStage)
trfModule mod = trfLocCorrect (createModuleInfo mod) (\sr -> combineSrcSpans sr <$> (uniqueTokenAnywhere AnnEofPos)) $ 
                  \(HsModule name exports imports decls deprec _) -> 
                    AST.Module <$> trfFilePragmas
                               <*> trfModuleHead name exports deprec
                               <*> trfImports imports
                               <*> trfDecls decls
       
trfModuleRename :: Module -> Ann AST.Module (Dom RdrName) RangeStage 
                          -> (HsGroup Name, [LImportDecl Name], Maybe [LIE Name], Maybe LHsDocString) 
                          -> Located (HsModule RdrName) 
                          -> Trf (Ann AST.Module (Dom GHC.Name) RangeStage)
trfModuleRename mod rangeMod (gr,imports,exps,_) hsMod 
    = do info <- createModuleInfo mod
         trfLocCorrect (pure info) (\sr -> combineSrcSpans sr <$> (uniqueTokenAnywhere AnnEofPos)) (trfModuleRename' (info ^. AST.implicitNames)) hsMod      
  where originalNames = Map.fromList $ catMaybes $ map getSourceAndInfo (rangeMod ^? biplateRef) 
        getSourceAndInfo :: Ann AST.SimpleName (Dom RdrName) RangeStage -> Maybe (SrcSpan, RdrName)
        getSourceAndInfo n = (,) <$> (n ^? annotation&sourceInfo&nodeSpan) <*> (n ^? semantics&nameInfo)
        
        trfModuleRename' preludeImports hsMod@(HsModule name exports _ decls deprec _) = do
          transformedImports <- orderAnnList <$> (trfImports imports)
          setOriginalNames originalNames
            $ AST.Module <$> trfFilePragmas
                         <*> trfModuleHead name (case (exports, exps) of (Just (L l _), Just ie) -> Just (L l ie)
                                                                         _                       -> Nothing) deprec
                         <*> return transformedImports
                         <*> addToScope (concat @[] (transformedImports ^? AST.annList&semantics&AST.importedNames) ++ preludeImports) (trfDeclsGroup gr)

trfModuleHead :: TransformName n r => Maybe (Located ModuleName) -> Maybe (Located [LIE n]) -> Maybe (Located WarningTxt) -> Trf (AnnMaybe AST.ModuleHead (Dom r) RangeStage) 
trfModuleHead (Just mn) exports modPrag
  = makeJust <$> (annLocNoSema (tokensLoc [AnnModule, AnnWhere])
                               (AST.ModuleHead <$> trfModuleName mn 
                                               <*> trfExportList (srcSpanEnd $ getLoc mn) exports
                                               <*> trfModulePragma modPrag))
trfModuleHead _ Nothing _ = nothing "" "" moduleHeadPos
  where moduleHeadPos = after AnnClose >>= \case loc@(RealSrcLoc _) -> return loc
                                                 _ -> atTheStart

trfFilePragmas :: Trf (AnnList AST.FilePragma (Dom r) RangeStage)
trfFilePragmas = do pragmas <- asks pragmaComms
                    languagePragmas <- mapM trfLanguagePragma (fromMaybe [] $ (Map.lookup "LANGUAGE") pragmas)
                    optionsPragmas <- mapM trfOptionsPragma (fromMaybe [] $ (Map.lookup "OPTIONS_GHC") pragmas)
                    makeList "" atTheStart $ pure $ orderDefs $ languagePragmas ++ optionsPragmas

trfLanguagePragma :: Located String -> Trf (Ann AST.FilePragma (Dom r) RangeStage)
trfLanguagePragma lstr@(L l str) = annLocNoSema (pure l) (AST.LanguagePragma <$> makeList ", " (pure $ srcSpanStart $ getLoc $ last pragmaElems) 
                                                                                               (mapM (trfLocNoSema (pure . AST.LanguageExtension)) extensions))
  where pragmaElems = splitLocated lstr
        extensions = init $ drop 2 pragmaElems

trfOptionsPragma :: Located String -> Trf (Ann AST.FilePragma (Dom r) RangeStage)
trfOptionsPragma (L l str) = annLocNoSema (pure l) (AST.OptionsPragma <$> annContNoSema (pure $ AST.StringNode str))

trfModulePragma :: Maybe (Located WarningTxt) -> Trf (AnnMaybe AST.ModulePragma (Dom r) RangeStage)
trfModulePragma = trfMaybeDefault " " "" (trfLocNoSema $ \case WarningTxt _ txts -> AST.ModuleWarningPragma <$> trfAnnList " " trfText' txts
                                                               DeprecatedTxt _ txts -> AST.ModuleDeprecatedPragma <$> trfAnnList " " trfText' txts) 
                                  (before AnnWhere)

trfText' :: StringLiteral -> Trf (AST.StringNode (Dom r) RangeStage)
trfText' = pure . AST.StringNode . unpackFS . sl_fs



trfExportList :: TransformName n r => SrcLoc -> Maybe (Located [LIE n]) -> Trf (AnnMaybe AST.ExportSpecList (Dom r) RangeStage)
trfExportList loc = trfMaybeDefault " " "" (trfLocNoSema trfExportList') (pure loc)

trfExportList' :: TransformName n r => [LIE n] -> Trf (AST.ExportSpecList (Dom r) RangeStage)
trfExportList' exps = AST.ExportSpecList <$> (makeList ", " (after AnnOpenP) (orderDefs . catMaybes <$> (mapM trfExport exps)))
  
trfExport :: TransformName n r => LIE n -> Trf (Maybe (Ann AST.ExportSpec (Dom r) RangeStage))
trfExport = trfMaybeLocNoSema $ \case 
  IEModuleContents n -> Just . AST.ModuleExport <$> (trfModuleName n)
  other -> do trf <- trfIESpec' other
              fmap AST.DeclExport <$> (sequence $ fmap (annContNoSema . return) trf)

trfImports :: TransformName n r => [LImportDecl n] -> Trf (AnnList AST.ImportDecl (Dom r) RangeStage)
trfImports (filter (not . ideclImplicit . unLoc) -> imps) 
  = AnnList <$> importDefaultLoc <*> mapM trfImport imps
  where importDefaultLoc = noSemaInfo . AST.ListPos (if Data.List.null imps then "\n" else "") "" "\n" True . srcSpanEnd 
                             <$> (combineSrcSpans <$> asks (srcLocSpan . srcSpanStart . contRange) 
                                                  <*> (srcLocSpan . srcSpanEnd <$> tokenLoc AnnWhere))
trfImport :: TransformName n r => LImportDecl n -> Trf (Ann AST.ImportDecl (Dom r) RangeStage)
trfImport (L l (GHC.ImportDecl src name pkg isSrc isSafe isQual isImpl declAs declHiding)) =
  let -- default positions of optional parts of an import declaration
      annBeforeQual = if isSrc then AnnClose else AnnImport
      annBeforeSafe = if isQual then AnnQualified else annBeforeQual
      annBeforePkg = if isSafe then AnnSafe else annBeforeSafe
      atAsPos = if isJust declHiding then before AnnOpenP else atTheEnd
  in (\impdecl -> annLoc (createImportData =<< impdecl) (pure l) impdecl) $ AST.ImportDecl 
       <$> (if isSrc then makeJust <$> annLocNoSema (tokensLoc [AnnOpen, AnnClose]) (pure AST.ImportSource)
                     else nothing " " "" (after AnnImport))
       <*> (if isQual then makeJust <$> (annLocNoSema (tokenLoc AnnQualified) (pure AST.ImportQualified)) 
                      else nothing " " "" (after annBeforeQual))
       <*> (if isSafe then makeJust <$> (annLocNoSema (tokenLoc AnnSafe) (pure AST.ImportSafe)) 
                      else nothing " " "" (after annBeforeSafe))
       <*> maybe (nothing " " "" (after annBeforePkg)) 
                 (\str -> makeJust <$> (annLocNoSema (tokenLoc AnnPackageName) (pure (AST.StringNode (unpackFS $ sl_fs str))))) pkg
       <*> trfModuleName name 
       <*> maybe (nothing " " "" atAsPos) (\mn -> makeJust <$> (trfRenaming mn)) declAs
       <*> trfImportSpecs declHiding 
  where trfRenaming mn
          = annLocNoSema (tokensLoc [AnnAs,AnnVal])
                         (AST.ImportRenaming <$> (annLocNoSema (tokenLoc AnnVal) 
                                                 (trfModuleName' mn)))  
  
trfImportSpecs :: TransformName n r => Maybe (Bool, Located [LIE n]) -> Trf (AnnMaybe AST.ImportSpec (Dom r) RangeStage)
trfImportSpecs (Just (True, l)) 
  = makeJust <$> trfLocNoSema (\specs -> AST.ImportSpecHiding <$> (makeList ", " (after AnnOpenP) (catMaybes <$> mapM trfIESpec specs))) l
trfImportSpecs (Just (False, l)) 
  = makeJust <$> trfLocNoSema (\specs -> AST.ImportSpecList <$> (makeList ", " (after AnnOpenP) (catMaybes <$> mapM trfIESpec specs))) l
trfImportSpecs Nothing = nothing " " "" atTheEnd
    
trfIESpec :: TransformName n r => LIE n -> Trf (Maybe (Ann AST.IESpec (Dom r) RangeStage)) 
trfIESpec = trfMaybeLocNoSema trfIESpec'
  
trfIESpec' :: TransformName n r => IE n -> Trf (Maybe (AST.IESpec (Dom r) RangeStage))
trfIESpec' (IEVar n) = Just <$> (AST.IESpec <$> trfName n <*> (nothing "(" ")" atTheEnd))
trfIESpec' (IEThingAbs n) = Just <$> (AST.IESpec <$> trfName n <*> (nothing "(" ")" atTheEnd))
trfIESpec' (IEThingAll n) 
  = Just <$> (AST.IESpec <$> trfName n <*> (makeJust <$> (annLocNoSema (tokenLoc AnnDotdot) (pure AST.SubSpecAll))))
trfIESpec' (IEThingWith n _ ls _)
  = Just <$> (AST.IESpec <$> trfName n
                         <*> (makeJust <$> between AnnOpenP AnnCloseP 
                                                  (annContNoSema $ AST.SubSpecList <$> makeList ", " (after AnnOpenP) (mapM trfName ls))))
trfIESpec' _ = pure Nothing
  
 