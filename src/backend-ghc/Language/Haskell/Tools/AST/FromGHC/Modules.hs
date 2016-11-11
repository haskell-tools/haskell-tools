{-# LANGUAGE LambdaCase
           , ViewPatterns
           , FlexibleContexts
           , ScopedTypeVariables
           , TypeApplications
           , TupleSections
           #-}
-- | Functions that convert the module-related elements (modules, imports, exports) of the GHC AST to corresponding elements in the Haskell-tools AST representation
-- Also contains the entry point of the transformation that collects the information from different GHC AST representations.
module Language.Haskell.Tools.AST.FromGHC.Modules where

import Control.Reference hiding (element)
import Data.Maybe
import Data.Function (on)
import Data.List as List
import Data.Char
import Data.Map as Map hiding (map, filter)
import Data.IORef
import Data.Data
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import Avail as GHC
import GHC
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
import TcEvidence as GHC
import TcRnMonad as GHC
import RnEnv as GHC
import RnExpr as GHC
import ErrUtils as GHC
import PrelNames as GHC
import NameEnv as GHC
import TcRnDriver as GHC
import TcExpr as GHC
import TcType as GHC
import UniqSupply as GHC
import UniqFM as GHC
import HeaderInfo as GHC
import Language.Haskell.TH.LanguageExtensions

import Language.Haskell.Tools.AST (Ann(..), AnnMaybeG(..), AnnListG(..), Dom, IdDom, RangeStage
                                  , semanticInfo, sourceInfo, semantics, annotation, nodeSpan, semaTraverse)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.SemaInfoTypes as AST

import Language.Haskell.Tools.AST.FromGHC.Names
import Language.Haskell.Tools.AST.FromGHC.Decls
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.GHCUtils

import Debug.Trace

createModuleInfo :: ModSummary -> Trf (AST.ModuleInfo GHC.Name)
createModuleInfo mod = do let prelude = xopt ImplicitPrelude $ ms_hspp_opts mod
                          (_,preludeImports) <- if prelude then getImportedNames "Prelude" Nothing else return (ms_mod mod, [])
                          return $ mkModuleInfo (ms_mod mod) (case ms_hsc_src mod of HsSrcFile -> False; _ -> True) preludeImports

trfModule :: ModSummary -> Located (HsModule RdrName) -> Trf (Ann AST.UModule (Dom RdrName) RangeStage)
trfModule mod = trfLocCorrect (createModuleInfo mod) (\sr -> combineSrcSpans sr <$> (uniqueTokenAnywhere AnnEofPos)) $ 
                  \(HsModule name exports imports decls deprec _) -> 
                    AST.UModule <$> trfFilePragmas
                                <*> trfModuleHead name exports deprec
                                <*> trfImports imports
                                <*> trfDecls decls
       
trfModuleRename :: ModSummary -> Ann AST.UModule (Dom RdrName) RangeStage 
                              -> (HsGroup Name, [LImportDecl Name], Maybe [LIE Name], Maybe LHsDocString) 
                              -> Located (HsModule RdrName) 
                              -> Trf (Ann AST.UModule (Dom GHC.Name) RangeStage)
trfModuleRename mod rangeMod (gr,imports,exps,_) hsMod 
    = do info <- createModuleInfo mod
         trfLocCorrect (pure info) (\sr -> combineSrcSpans sr <$> (uniqueTokenAnywhere AnnEofPos)) (trfModuleRename' (info ^. implicitNames)) hsMod      
  where roleAnnots = rangeMod ^? AST.modDecl&AST.annList&filtered ((\case Ann _ (AST.URoleDecl {}) -> True; _ -> False))
        originalNames = Map.fromList $ catMaybes $ map getSourceAndInfo (rangeMod ^? biplateRef) 
        getSourceAndInfo :: Ann AST.UQualifiedName (Dom RdrName) RangeStage -> Maybe (SrcSpan, RdrName)
        getSourceAndInfo n = (,) <$> (n ^? annotation&sourceInfo&nodeSpan) <*> (n ^? semantics&nameInfo)
        
        trfModuleRename' preludeImports hsMod@(HsModule name exports _ decls deprec _) = do
          transformedImports <- orderAnnList <$> (trfImports imports)
           
          addToScope (concat @[] (transformedImports ^? AST.annList&semantics&importedNames) ++ preludeImports)
            $ loadSplices mod hsMod gr $ setOriginalNames originalNames . setDeclsToInsert roleAnnots
              $ AST.UModule <$> trfFilePragmas
                            <*> trfModuleHead name (case (exports, exps) of (Just (L l _), Just ie) -> Just (L l ie)
                                                                            _                       -> Nothing) deprec
                            <*> return transformedImports
                            <*> trfDeclsGroup gr

loadSplices :: ModSummary -> HsModule RdrName -> HsGroup Name -> Trf a -> Trf a
loadSplices mod hsMod group trf = do 
    let declSpls = map (\(SpliceDecl sp _) -> sp) $ hsMod ^? biplateRef :: [Located (HsSplice RdrName)]
        declLocs = map getLoc declSpls
    let exprSpls = catMaybes $ map (\case HsSpliceE sp -> Just sp; _ -> Nothing) $ hsMod ^? biplateRef :: [HsSplice RdrName]
        typeSpls = catMaybes $ map (\case HsSpliceTy sp _ -> Just sp; _ -> Nothing) $ hsMod ^? biplateRef :: [HsSplice RdrName]
    -- initialize reader environment
    env <- liftGhc getSession

    locals <- asks ((hsGetNames group ++) . concat . localsInScope)
    let createGRE n | Just modName <- nameModule_maybe n
                    = [GRE n NoParent False [ImpSpec (ImpDeclSpec (moduleName modName) (moduleName modName) False noSrcSpan) ImpAll]]
                    | otherwise = []
        readEnv = mkOccEnv (map (\n -> (GHC.occName n, createGRE n)) locals)

    tcdSplices <- liftIO $ runTcInteractive env { hsc_dflags = xopt_set (hsc_dflags env) TemplateHaskellQuotes }
      $ updGblEnv (\gbl -> gbl { tcg_rdr_env = readEnv }) 
      $ (,,) <$> mapM tcHsSplice declSpls <*> mapM tcHsSplice' typeSpls <*> mapM tcHsSplice' exprSpls
    let (declSplices, typeSplices, exprSplices) 
          = fromMaybe (error $ "Splice expression could not be typechecked: " 
                                 ++ showSDocUnsafe (vcat (pprErrMsgBagWithLoc (fst (fst tcdSplices))) 
                                                      <+> vcat (pprErrMsgBagWithLoc (snd (fst tcdSplices))))) 
                      (snd tcdSplices)
    setSplices declSplices typeSplices exprSplices trf
  where
    tcHsSplice :: Located (HsSplice RdrName) -> RnM (Located (HsSplice Name))
    tcHsSplice (L l s) = L l <$> tcHsSplice' s
    tcHsSplice' (HsTypedSplice id e) 
      = HsTypedSplice (mkUnboundNameRdr id) <$> (fst <$> rnLExpr e)
    tcHsSplice' (HsUntypedSplice id e) 
      = HsUntypedSplice (mkUnboundNameRdr id) <$> (fst <$> rnLExpr e)
    tcHsSplice' (HsQuasiQuote id1 id2 sp fs) 
      = pure $ HsQuasiQuote (mkUnboundNameRdr id1) (mkUnboundNameRdr id2) sp fs

trfModuleHead :: TransformName n r => Maybe (Located ModuleName) -> Maybe (Located [LIE n]) -> Maybe (Located WarningTxt) -> Trf (AnnMaybeG AST.UModuleHead (Dom r) RangeStage) 
trfModuleHead (Just mn) exports modPrag
  = makeJust <$> (annLocNoSema (tokensLoc [AnnModule, AnnWhere])
                               (AST.UModuleHead <$> trfModuleName mn 
                                                <*> trfExportList (srcSpanEnd $ getLoc mn) exports
                                                <*> trfModulePragma modPrag))
trfModuleHead _ Nothing _ = nothing "" "" moduleHeadPos
  where moduleHeadPos = after AnnClose >>= \case loc@(RealSrcLoc _) -> return loc
                                                 _ -> atTheStart

trfFilePragmas :: Trf (AnnListG AST.UFilePragma (Dom r) RangeStage)
trfFilePragmas = do pragmas <- asks pragmaComms
                    languagePragmas <- mapM trfLanguagePragma (fromMaybe [] $ (Map.lookup "LANGUAGE") pragmas)
                    optionsPragmas <- mapM trfOptionsPragma (fromMaybe [] $ (Map.lookup "OPTIONS_GHC") pragmas)
                    makeList "" atTheStart $ pure $ orderDefs $ languagePragmas ++ optionsPragmas

trfLanguagePragma :: Located String -> Trf (Ann AST.UFilePragma (Dom r) RangeStage)
trfLanguagePragma lstr@(L l str) = annLocNoSema (pure l) (AST.ULanguagePragma <$> makeList ", " (pure $ srcSpanStart $ getLoc $ last pragmaElems) 
                                                                                                (mapM (trfLocNoSema (pure . AST.ULanguageExtension)) extensions))
  where pragmaElems = splitLocated lstr
        extensions = init $ drop 2 pragmaElems

trfOptionsPragma :: Located String -> Trf (Ann AST.UFilePragma (Dom r) RangeStage)
trfOptionsPragma (L l str) = annLocNoSema (pure l) (AST.UOptionsPragma <$> annContNoSema (pure $ AST.UStringNode str))

trfModulePragma :: Maybe (Located WarningTxt) -> Trf (AnnMaybeG AST.UModulePragma (Dom r) RangeStage)
trfModulePragma = trfMaybeDefault " " "" (trfLocNoSema $ \case WarningTxt _ txts -> AST.UModuleWarningPragma <$> trfAnnList " " trfText' txts
                                                               DeprecatedTxt _ txts -> AST.UModuleDeprecatedPragma <$> trfAnnList " " trfText' txts) 
                                  (before AnnWhere)

trfText' :: StringLiteral -> Trf (AST.UStringNode (Dom r) RangeStage)
trfText' = pure . AST.UStringNode . unpackFS . sl_fs



trfExportList :: TransformName n r => SrcLoc -> Maybe (Located [LIE n]) -> Trf (AnnMaybeG AST.UExportSpecs (Dom r) RangeStage)
trfExportList loc = trfMaybeDefault " " "" (trfLocNoSema trfExportList') (pure loc)

trfExportList' :: TransformName n r => [LIE n] -> Trf (AST.UExportSpecs (Dom r) RangeStage)
trfExportList' exps = AST.UExportSpecs <$> (makeList ", " (after AnnOpenP) (orderDefs . catMaybes <$> (mapM trfExport exps)))
  
trfExport :: TransformName n r => LIE n -> Trf (Maybe (Ann AST.UExportSpec (Dom r) RangeStage))
trfExport = trfMaybeLocNoSema $ \case 
  IEModuleContents n -> Just . AST.UModuleExport <$> (trfModuleName n)
  other -> do trf <- trfIESpec' other
              fmap AST.UDeclExport <$> (sequence $ fmap (annContNoSema . return) trf)

trfImports :: TransformName n r => [LImportDecl n] -> Trf (AnnListG AST.UImportDecl (Dom r) RangeStage)
trfImports (filter (not . ideclImplicit . unLoc) -> imps) 
  = AnnListG <$> importDefaultLoc <*> mapM trfImport imps
  where importDefaultLoc = noSemaInfo . AST.ListPos (if List.null imps then "\n" else "") "" "\n" True . srcSpanEnd 
                             <$> (combineSrcSpans <$> asks (srcLocSpan . srcSpanStart . contRange) 
                                                  <*> (srcLocSpan . srcSpanEnd <$> tokenLoc AnnWhere))
trfImport :: TransformName n r => LImportDecl n -> Trf (Ann AST.UImportDecl (Dom r) RangeStage)
trfImport (L l impDecl@(GHC.ImportDecl src name pkg isSrc isSafe isQual isImpl declAs declHiding)) =
  let -- default positions of optional parts of an import declaration
      annBeforeQual = if isSrc then AnnClose else AnnImport
      annBeforeSafe = if isQual then AnnQualified else annBeforeQual
      annBeforePkg = if isSafe then AnnSafe else annBeforeSafe
  in annLoc (createImportData impDecl) (pure l) $ AST.UImportDecl 
       <$> (if isSrc then makeJust <$> annLocNoSema (tokensLoc [AnnOpen, AnnClose]) (pure AST.UImportSource)
                     else nothing " " "" (after AnnImport))
       <*> (if isQual then makeJust <$> (annLocNoSema (tokenLoc AnnQualified) (pure AST.UImportQualified)) 
                      else nothing " " "" (after annBeforeQual))
       <*> (if isSafe then makeJust <$> (annLocNoSema (tokenLoc AnnSafe) (pure AST.UImportSafe)) 
                      else nothing " " "" (after annBeforeSafe))
       <*> maybe (nothing " " "" (after annBeforePkg)) 
                 (\str -> makeJust <$> (annLocNoSema (tokenLoc AnnPackageName) (pure (AST.UStringNode (unpackFS $ sl_fs str))))) pkg
       <*> trfModuleName name 
       <*> maybe (nothing " " "" (pure $ srcSpanEnd (getLoc name))) (\mn -> makeJust <$> (trfRenaming mn)) declAs
       <*> trfImportSpecs declHiding 
  where trfRenaming mn
          = annLocNoSema (tokensLoc [AnnAs,AnnVal])
                         (AST.UImportRenaming <$> (annLocNoSema (tokenLoc AnnVal) 
                                                  (trfModuleName' mn)))  
  
trfImportSpecs :: TransformName n r => Maybe (Bool, Located [LIE n]) -> Trf (AnnMaybeG AST.UImportSpec (Dom r) RangeStage)
trfImportSpecs (Just (True, l)) 
  = makeJust <$> trfLocNoSema (\specs -> AST.UImportSpecHiding <$> (makeList ", " (after AnnOpenP) (catMaybes <$> mapM trfIESpec specs))) l
trfImportSpecs (Just (False, l)) 
  = makeJust <$> trfLocNoSema (\specs -> AST.UImportSpecList <$> (makeList ", " (after AnnOpenP) (catMaybes <$> mapM trfIESpec specs))) l
trfImportSpecs Nothing = nothing " " "" atTheEnd
    
trfIESpec :: TransformName n r => LIE n -> Trf (Maybe (Ann AST.UIESpec (Dom r) RangeStage)) 
trfIESpec = trfMaybeLocNoSema trfIESpec'
  
trfIESpec' :: TransformName n r => IE n -> Trf (Maybe (AST.UIESpec (Dom r) RangeStage))
trfIESpec' (IEVar n) = Just <$> (AST.UIESpec <$> trfName n <*> (nothing "(" ")" atTheEnd))
trfIESpec' (IEThingAbs n) = Just <$> (AST.UIESpec <$> trfName n <*> (nothing "(" ")" atTheEnd))
trfIESpec' (IEThingAll n) 
  = Just <$> (AST.UIESpec <$> trfName n <*> (makeJust <$> (annLocNoSema (tokenLoc AnnDotdot) (pure AST.USubSpecAll))))
trfIESpec' (IEThingWith n _ ls _)
  = Just <$> (AST.UIESpec <$> trfName n
                          <*> (makeJust <$> between AnnOpenP AnnCloseP 
                                                   (annContNoSema $ AST.USubSpecList <$> makeList ", " (after AnnOpenP) (mapM trfName ls))))
trfIESpec' _ = pure Nothing
  
trfModuleName :: Located ModuleName -> Trf (Ann AST.UModuleName (Dom r) RangeStage)
trfModuleName = trfLocNoSema trfModuleName'

trfModuleName' :: ModuleName -> Trf (AST.UModuleName (Dom r) RangeStage)
trfModuleName' = pure . AST.UModuleName . moduleNameString 
 