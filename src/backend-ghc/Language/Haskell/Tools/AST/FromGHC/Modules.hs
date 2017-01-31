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

import Control.Monad.Reader
import Control.Reference hiding (element)
import Data.Generics.Uniplate.Data ()
import Data.List as List
import Data.Map as Map (fromList, lookup)
import Data.Maybe

import BasicTypes as GHC (WarningTxt(..), StringLiteral(..))
import DynFlags as GHC (xopt_set)
import ErrUtils as GHC (pprErrMsgBagWithLoc)
import FastString as GHC (unpackFS)
import GHC
import HscTypes as GHC (WarningTxt(..), ModSummary, HscEnv(..))
import Language.Haskell.TH.LanguageExtensions (Extension(..))
import Name as GHC hiding (varName)
import Outputable as GHC (vcat, showSDocUnsafe, (<+>))
import RdrName as GHC
import RnEnv as GHC (mkUnboundNameRdr)
import RnExpr as GHC (rnLExpr)
import SrcLoc as GHC
import TcRnMonad as GHC

import Language.Haskell.Tools.AST (Ann(..), AnnMaybeG, AnnListG(..), Dom, RangeStage
                                  , sourceInfo, semantics, annotation, nodeSpan)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.FromGHC.Decls (trfDecls, trfDeclsGroup)
import Language.Haskell.Tools.AST.FromGHC.GHCUtils (HsHasName(..))
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Names (TransformName(..), trfName)
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.SemaInfoTypes as AST (nameInfo, implicitNames, importedNames)

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
        
        trfModuleRename' preludeImports hsMod@(HsModule name exports _ _ deprec _) = do
          transformedImports <- orderAnnList <$> (trfImports imports)
           
          addToScope (concat @[] (transformedImports ^? AST.annList&semantics&importedNames) ++ preludeImports)
            $ loadSplices hsMod gr $ setOriginalNames originalNames . setDeclsToInsert roleAnnots
              $ AST.UModule <$> trfFilePragmas
                            <*> trfModuleHead name (case (exports, exps) of (Just (L l _), Just ie) -> Just (L l ie)
                                                                            _                       -> Nothing) deprec
                            <*> return transformedImports
                            <*> trfDeclsGroup gr

loadSplices :: HsModule RdrName -> HsGroup Name -> Trf a -> Trf a
loadSplices hsMod group trf = do 
    let declSpls = map (\(SpliceDecl sp _) -> sp) $ hsMod ^? biplateRef :: [Located (HsSplice RdrName)]
        exprSpls = catMaybes $ map (\case HsSpliceE sp -> Just sp; _ -> Nothing) $ hsMod ^? biplateRef :: [HsSplice RdrName]
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
                                                <*> trfModulePragma (srcSpanEnd $ getLoc mn) modPrag
                                                <*> trfExportList (before AnnWhere) exports))
trfModuleHead _ Nothing _ = nothing "" "" moduleHeadPos
  where moduleHeadPos = after AnnClose >>= \case loc@(RealSrcLoc _) -> return loc
                                                 _ -> atTheStart
trfModuleHead Nothing (Just _) _ = error "trfModuleHead: no head but has exports"

trfFilePragmas :: Trf (AnnListG AST.UFilePragma (Dom r) RangeStage)
trfFilePragmas = do pragmas <- asks pragmaComms
                    languagePragmas <- mapM trfLanguagePragma (fromMaybe [] $ (Map.lookup "LANGUAGE") pragmas)
                    optionsPragmas <- mapM trfOptionsPragma (fromMaybe [] $ (Map.lookup "OPTIONS_GHC") pragmas)
                    makeList "" atTheStart $ pure $ orderDefs $ languagePragmas ++ optionsPragmas

trfLanguagePragma :: Located String -> Trf (Ann AST.UFilePragma (Dom r) RangeStage)
trfLanguagePragma lstr@(L l _) = annLocNoSema (pure l) (AST.ULanguagePragma <$> makeList ", " (pure $ srcSpanStart $ getLoc $ last pragmaElems) 
                                                                                              (mapM (trfLocNoSema (pure . AST.ULanguageExtension)) extensions))
  where pragmaElems = splitLocated lstr
        extensions = init $ drop 2 pragmaElems

trfOptionsPragma :: Located String -> Trf (Ann AST.UFilePragma (Dom r) RangeStage)
trfOptionsPragma (L l str) = annLocNoSema (pure l) (AST.UOptionsPragma <$> annContNoSema (pure $ AST.UStringNode str))

trfModulePragma :: SrcLoc -> Maybe (Located WarningTxt) -> Trf (AnnMaybeG AST.UModulePragma (Dom r) RangeStage)
trfModulePragma l = trfMaybeDefault " " "" (trfLocNoSema $ \case WarningTxt _ txts -> AST.UModuleWarningPragma <$> trfAnnList " " trfText' txts
                                                                 DeprecatedTxt _ txts -> AST.UModuleDeprecatedPragma <$> trfAnnList " " trfText' txts) 
                                    (pure l)

trfText' :: StringLiteral -> Trf (AST.UStringNode (Dom r) RangeStage)
trfText' = pure . AST.UStringNode . unpackFS . sl_fs



trfExportList :: TransformName n r => Trf SrcLoc -> Maybe (Located [LIE n]) -> Trf (AnnMaybeG AST.UExportSpecs (Dom r) RangeStage)
trfExportList loc = trfMaybeDefault "" " " (trfLocNoSema trfExportList') loc

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
trfImport (L l impDecl@(GHC.ImportDecl _ name pkg isSrc isSafe isQual _ declAs declHiding)) =
  do safeTok <- tokenLoc AnnSafe

     let -- default positions of optional parts of an import declaration
         annBeforeQual = if isSrc then AnnClose else AnnImport
         annBeforeSafe = if isQual then AnnQualified else annBeforeQual
         annBeforePkg = if isGoodSrcSpan safeTok then AnnSafe else annBeforeSafe

     annLoc (createImportData impDecl) (pure l) $ AST.UImportDecl 
       <$> (if isSrc then makeJust <$> annLocNoSema (tokensLoc [AnnOpen, AnnClose]) (pure AST.UImportSource)
                     else nothing " " "" (after AnnImport))
       <*> (if isQual then makeJust <$> (annLocNoSema (tokenLoc AnnQualified) (pure AST.UImportQualified)) 
                      else nothing " " "" (after annBeforeQual))
       <*> (if isGoodSrcSpan safeTok then makeJust <$> (annLocNoSema (pure safeTok) (pure AST.UImportSafe)) 
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
  = makeJust <$> trfLocNoSema (\specs -> AST.UImportSpecHiding <$> (makeList ", " (after AnnOpenP) (catMaybes <$> mapM trfIESpec (removeDuplicates specs)))) l
trfImportSpecs (Just (False, l)) 
  = makeJust <$> trfLocNoSema (\specs -> AST.UImportSpecList <$> (makeList ", " (after AnnOpenP) (catMaybes <$> mapM trfIESpec (removeDuplicates specs)))) l
trfImportSpecs Nothing = nothing " " "" atTheEnd
    
trfIESpec :: TransformName n r => LIE n -> Trf (Maybe (Ann AST.UIESpec (Dom r) RangeStage)) 
trfIESpec = trfMaybeLocNoSema trfIESpec'
  
trfIESpec' :: TransformName n r => IE n -> Trf (Maybe (AST.UIESpec (Dom r) RangeStage))
trfIESpec' (IEVar n) = Just <$> (AST.UIESpec <$> trfImportModifier <*> trfName n <*> (nothing "(" ")" atTheEnd))
trfIESpec' (IEThingAbs n) = Just <$> (AST.UIESpec <$> trfImportModifier <*> trfName n <*> (nothing "(" ")" atTheEnd))
trfIESpec' (IEThingAll n) 
  = Just <$> (AST.UIESpec <$> trfImportModifier <*> trfName n <*> (makeJust <$> subspec))
  where subspec = betweenIncluding AnnOpenP AnnCloseP $ annContNoSema (pure AST.USubSpecAll)
trfIESpec' (IEThingWith n _ ls _)
  = Just <$> (AST.UIESpec <$> trfImportModifier <*> trfName n <*> (makeJust <$> subspec))
  where subspec = betweenIncluding AnnOpenP AnnCloseP $ annContNoSema 
                    $ AST.USubSpecList <$> between AnnOpenP AnnCloseP (makeList ", " atTheStart (mapM trfName ls))
trfIESpec' _ = pure Nothing

trfImportModifier :: Trf (AnnMaybeG AST.UImportModifier (Dom r) RangeStage)
trfImportModifier = do
  patLoc <- tokenLoc AnnPattern
  if isGoodSrcSpan patLoc then makeJust <$> annLocNoSema (return patLoc) (return AST.UImportPattern)
                          else nothing " " "" atTheStart
  
trfModuleName :: Located ModuleName -> Trf (Ann AST.UModuleName (Dom r) RangeStage)
trfModuleName = trfLocNoSema trfModuleName'

trfModuleName' :: ModuleName -> Trf (AST.UModuleName (Dom r) RangeStage)
trfModuleName' = pure . AST.UModuleName . moduleNameString 
 