{-# LANGUAGE BangPatterns, FlexibleContexts, GADTs, LambdaCase, MultiWayIf, ScopedTypeVariables, TypeApplications, ViewPatterns #-}

-- | Functions that convert the module-related elements (modules, imports, exports) of the GHC AST to corresponding elements in the Haskell-tools AST representation
-- Also contains the entry point of the transformation that collects the information from different GHC AST representations.
module Language.Haskell.Tools.BackendGHC.Modules where

import Control.Monad.Reader
import Control.Monad.State
import Control.Reference hiding (element)
import Data.Char (isSpace)
import Data.Generics.Uniplate.Data ()
import Data.List as List
import Data.Map as Map (fromList, lookup)
import Data.Maybe

import BasicTypes as GHC (WarningTxt(..), StringLiteral(..))
import FastString as GHC (unpackFS)
import FieldLabel as GHC (FieldLbl(..))
import GHC
import SrcLoc as GHC
import TcRnMonad as GHC (Applicative(..), (<$>))

import Language.Haskell.Tools.AST (Ann(..), AnnMaybeG, AnnListG(..), Dom, RangeStage
                                  , sourceInfo, semantics, annotation, nodeSpan)
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.SemaInfoTypes as AST
import Language.Haskell.Tools.BackendGHC.Decls (trfDecls, trfDeclsGroup)
import Language.Haskell.Tools.BackendGHC.Exprs (trfText')
import Language.Haskell.Tools.BackendGHC.Monad
import Language.Haskell.Tools.BackendGHC.Names (TransformName, trfName)
import Language.Haskell.Tools.BackendGHC.Utils

-- Transformes a module in its renamed state. This will be performed to help the transformation of the actual typed module representation.
trfModule :: ModSummary -> Located (HsModule RdrName) -> Trf (Ann AST.UModule (Dom RdrName) RangeStage)
trfModule mod hsMod = do -- createModuleInfo involves reading the ghc compiler state, so it must be evaluated
                         -- or large parts of the representation will be kept
                         !modInfo <- createModuleInfo mod (maybe noSrcSpan getLoc $ hsmodName $ unLoc hsMod) (hsmodImports $ unLoc hsMod)
                         trfLocCorrect (pure modInfo)
                            (\sr -> combineSrcSpans sr <$> (uniqueTokenAnywhere AnnEofPos))
                            (\(HsModule name exports imports decls deprec _) ->
                               AST.UModule <$> trfFilePragmas
                                           <*> trfModuleHead name (srcSpanStart (foldLocs (map getLoc imports ++ map getLoc decls))) exports deprec
                                           <*> trfImports imports
                                           <*> trfDecls decls) $ hsMod

-- | Transformes the module in its typed state. Uses the results of 'trfModule' to extract program
-- elements (splices for example) that are not kept in the typed representation.
trfModuleRename :: ModSummary -> Ann AST.UModule (Dom RdrName) RangeStage
                              -> (HsGroup Name, [LImportDecl Name], Maybe [LIE Name], Maybe LHsDocString)
                              -> Located (HsModule RdrName)
                              -> Trf (Ann AST.UModule (Dom GHC.Name) RangeStage)
trfModuleRename mod rangeMod (gr,imports,exps,_) hsMod
    = do -- createModuleInfo involves reading the ghc compiler state, so it must be evaluated
         -- or large parts of the representation will be kept
         !info <- createModuleInfo mod (maybe noSrcSpan getLoc $ hsmodName $ unLoc hsMod) imports
         trfLocCorrect (pure info) (\sr -> combineSrcSpans sr <$> (uniqueTokenAnywhere AnnEofPos)) (trfModuleRename' (info ^. implicitNames)) hsMod
  where roleAnnots = rangeMod ^? AST.modDecl&AST.annList&filtered ((\case Ann _ (AST.URoleDecl {}) -> True; _ -> False))
        originalNames = Map.fromList $ mapMaybe getSourceAndInfo (rangeMod ^? biplateRef)
        getSourceAndInfo :: Ann AST.UQualifiedName (Dom RdrName) RangeStage -> Maybe (SrcSpan, RdrName)
        getSourceAndInfo n = (,) <$> (n ^? annotation&sourceInfo&nodeSpan) <*> (n ^? semantics&nameInfo)

        exportDecls = rangeMod ^? AST.modHead & AST.annJust & AST.mhExports & AST.annJust & AST.espExports & AST.annList
        exportSubspecs = map (\e -> e ^? AST.exportDecl & AST.ieSubspec & AST.annJust & AST.essList & AST.annList) exportDecls
        exportSubspecsRngs = map (map AST.getRange) exportSubspecs

        replaceSubspecLocs :: [LIE Name] -> [LIE Name]
        replaceSubspecLocs exps = zipWith (\ss ie -> case ie of (L l (IEThingWith n wc ls flds)) -> L l (IEThingWith n wc (replaceNames ss ls) (replaceFieldNames (drop (length ls) ss) flds))
                                                                _ -> ie) exportSubspecsRngs exps
          where replaceNames ss ls = zipWith (\(L _ iew) l -> case iew of IEName (L _ n) -> L l (IEName (L l n))
                                                                          _              -> L l iew) ls ss
                replaceFieldNames ss ls = zipWith (\(L _ iew) l -> L l iew) ls ss

        trfModuleRename' preludeImports hsMod@(HsModule name exports _ _ deprec _) = do
          transformedImports <- orderAnnList <$> (trfImports imports)

          let importNames impd = ( impd ^. AST.importModule & AST.moduleNameString
                                 , impd ^? AST.importAs & AST.annJust & AST.importRename & AST.moduleNameString
                                 , AST.isAnnJust (impd ^. AST.importQualified)
                                 , impd ^. semantics&importedNames )
              -- if there is a qualified form of the import Prelude, the names should be empty
              importPrelude names = ( "Prelude", Nothing, False, names)

          addToScopeImported (map importNames (transformedImports ^? AST.annList) ++ [importPrelude preludeImports])
            $ loadSplices hsMod
            $ setOriginalNames originalNames . setDeclsToInsert roleAnnots
            $ do filePrags <- trfFilePragmas
                 AST.UModule filePrags
                  <$> trfModuleHead name
                       (srcSpanEnd (AST.getRange filePrags))
                       (case (exports, exps) of (Just (L l _), Just ie) -> Just (L l (replaceSubspecLocs (orderLocated ie)))
                                                _                       -> Nothing)
                       deprec
                  <*> return transformedImports
                  <*> trfDeclsGroup gr

-- | Extract the template haskell splices from the representation and adds them to the transformation state.
loadSplices :: HsModule RdrName -> Trf a -> Trf a
loadSplices hsMod trf = do
    let declSpls = map (\(SpliceDecl sp _) -> sp) $ hsMod ^? biplateRef :: [Located (HsSplice RdrName)]
        exprSpls = mapMaybe (\case L l (HsSpliceE sp) -> Just (L l sp); _ -> Nothing) $ hsMod ^? biplateRef :: [Located (HsSplice RdrName)]
        typeSpls = mapMaybe (\case L l (HsSpliceTy sp _) -> Just (L l sp); _ -> Nothing) $ hsMod ^? biplateRef :: [Located (HsSplice RdrName)]
    setSplices declSpls typeSpls exprSpls trf

trfModuleHead :: TransformName n r => Maybe (Located ModuleName) -> SrcLoc -> Maybe (Located [LIE n]) -> Maybe (Located WarningTxt) -> Trf (AnnMaybeG AST.UModuleHead (Dom r) RangeStage)
trfModuleHead (Just mn) _ exports modPrag
  = makeJust <$> (annLocNoSema (tokensLoc [AnnModule, AnnWhere])
                               (AST.UModuleHead <$> trfModuleName mn
                                                <*> trfModulePragma (srcSpanEnd $ getLoc mn) modPrag
                                                <*> trfExportList (before AnnWhere) exports))
trfModuleHead _ rng Nothing _ = nothing "" "" (pure rng)
trfModuleHead Nothing _ (Just _) _ = convertionProblem "trfModuleHead: no head but has exports"

trfFilePragmas :: Trf (AnnListG AST.UFilePragma (Dom r) RangeStage)
trfFilePragmas = do pragmas <- asks pragmaComms
                    languagePragmas <- mapM trfLanguagePragma (fromMaybe [] $ (Map.lookup "LANGUAGE") pragmas)
                    optionsPragmas <- mapM trfOptionsPragma (fromMaybe [] $ (Map.lookup "OPTIONS_GHC") pragmas)
                    makeList "" atTheStart $ pure $ orderDefs $ languagePragmas ++ optionsPragmas

trfLanguagePragma :: Located String -> Trf (Ann AST.UFilePragma (Dom r) RangeStage)
trfLanguagePragma lstr@(L l _) = annLocNoSema (pure l) (AST.ULanguagePragma <$> makeList ", " (pure $ srcSpanStart $ getLoc $ last pragmaElems)
                                                                                              (mapM (trfLocNoSema (pure . AST.ULanguageExtension)) extensions))
  where pragmaElems = splitLocatedOn (\c -> isSpace c || c == ',') lstr
        extensions = filter ((\sp -> srcSpanStart sp /= srcSpanEnd sp) . getLoc)
                       $ map (removeEnd . removeLang . removeStart) pragmaElems
        removeStart pr@(L l txt) = if "{-#"      `isPrefixOf` txt then L (updateStart (updateCol (+3)) l) (drop 3 txt) else pr
        removeLang  pr@(L l txt) = if "LANGUAGE" `isPrefixOf` txt then L (updateStart (updateCol (+8)) l) (drop 8 txt) else pr
        removeEnd   pr@(L l txt) = if "#-}"      `isSuffixOf` txt then L (updateEnd   (updateCol (subtract 3)) l) (reverse $ drop 3 $ reverse $ txt) else pr

trfOptionsPragma :: Located String -> Trf (Ann AST.UFilePragma (Dom r) RangeStage)
trfOptionsPragma (L l str) = annLocNoSema (pure l) (AST.UOptionsPragma <$> annContNoSema (pure $ AST.UStringNode str))

trfModulePragma :: SrcLoc -> Maybe (Located WarningTxt) -> Trf (AnnMaybeG AST.UModulePragma (Dom r) RangeStage)
trfModulePragma l = trfMaybeDefault " " "" (trfLocNoSema $ \case WarningTxt _ txts -> AST.UModuleWarningPragma <$> trfAnnList " " trfText' txts
                                                                 DeprecatedTxt _ txts -> AST.UModuleDeprecatedPragma <$> trfAnnList " " trfText' txts)
                                    (pure l)

trfExportList :: TransformName n r => Trf SrcLoc -> Maybe (Located [LIE n]) -> Trf (AnnMaybeG AST.UExportSpecs (Dom r) RangeStage)
trfExportList loc = trfMaybeDefault "" " " (trfLocNoSema trfExportList') loc

trfExportList' :: TransformName n r => [LIE n] -> Trf (AST.UExportSpecs (Dom r) RangeStage)
trfExportList' exps = AST.UExportSpecs <$> (makeList ", " (after AnnOpenP) (orderDefs . catMaybes <$> (mapM trfExport exps)))

trfExport :: TransformName n r => LIE n -> Trf (Maybe (Ann AST.UExportSpec (Dom r) RangeStage))
trfExport = trfMaybeLocNoSema $ \case
  IEModuleContents n -> Just . AST.UModuleExport <$> (trfModuleName n)
  other -> do trf <- trfIESpec' other
              fmap AST.UDeclExport <$> (sequence $ fmap (annContNoSema . return) trf)

trfImports :: forall n r . TransformName n r => [LImportDecl n] -> Trf (AnnListG AST.UImportDecl (Dom r) RangeStage)
trfImports (filter (not . ideclImplicit . unLoc) -> imps)
  = do res <- AnnListG <$> importDefaultLoc <*> mapM trfImport imps
       -- the list of imported entities is added after the imports have been evaluated, to have all instances loaded
       !importData <- mapM (createImportData . unLoc) imps :: Trf [ImportInfo r]
       return $ flip evalState 0 $ AST.annList & AST.annotation & AST.semanticInfo
                                     !~ (\_ -> get >>= \i -> modify (+1) >> return (importData !! i)) $ res
  where importDefaultLoc = noSemaInfo . AST.ListPos (if List.null imps then "\n" else "") "" "\n" (Just []) . srcSpanEnd
                             <$> (combineSrcSpans <$> asks (srcLocSpan . srcSpanStart . contRange)
                                                  <*> (srcLocSpan . srcSpanEnd <$> tokenLoc AnnWhere))

trfImport :: TransformName n r => LImportDecl n -> Trf (Ann AST.UImportDecl (Dom r) RangeStage)
trfImport (L l (GHC.ImportDecl _ name pkg isSrc _ isQual _ declAs declHiding)) = focusOn l $
  do safeTok <- tokenLoc AnnSafe
     let -- default positions of optional parts of an import declaration
         annBeforeQual = if isSrc then AnnClose else AnnImport
         annBeforeSafe = if isQual then AnnQualified else annBeforeQual
         annBeforePkg = if isGoodSrcSpan safeTok then AnnSafe else annBeforeSafe
     -- the import semantic infos will be generated after all imports are processed,
     -- otherwise information on instances imported will be inconsistent
     annLoc (pure (error "Import's semantic data not initialized")) (pure l) $ AST.UImportDecl
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
  where trfRenaming mn = annLocNoSema (combineSrcSpans (getLoc mn) <$> tokenLoc AnnAs)
                                      (AST.UImportRenaming <$> (trfModuleName mn))

trfImportSpecs :: TransformName n r => Maybe (Bool, Located [LIE n]) -> Trf (AnnMaybeG AST.UImportSpec (Dom r) RangeStage)
trfImportSpecs (Just (True, l))
  = makeJust <$> trfLocNoSema (\specs -> AST.UImportSpecHiding <$> (makeList ", " (after AnnOpenP) (catMaybes <$> mapM trfIESpec (removeDuplicates specs)))) l
trfImportSpecs (Just (False, l))
  = makeJust <$> trfLocNoSema (\specs -> AST.UImportSpecList <$> (makeList ", " (after AnnOpenP) (catMaybes <$> mapM trfIESpec (removeDuplicates specs)))) l
trfImportSpecs Nothing = nothing " " "" atTheEnd

trfIESpec :: TransformName n r => LIE n -> Trf (Maybe (Ann AST.UIESpec (Dom r) RangeStage))
trfIESpec = trfMaybeLocNoSema trfIESpec'

trfIESpec' :: TransformName n r => IE n -> Trf (Maybe (AST.UIESpec (Dom r) RangeStage))
trfIESpec' (IEVar n) = Just <$> (AST.UIESpec <$> trfImportModifier <*> trfName (getWrappedName n) <*> (nothing "(" ")" atTheEnd))
trfIESpec' (IEThingAbs n) = Just <$> (AST.UIESpec <$> trfImportModifier <*> trfName (getWrappedName n) <*> (nothing "(" ")" atTheEnd))
trfIESpec' (IEThingAll n)
  = Just <$> (AST.UIESpec <$> trfImportModifier <*> trfName (getWrappedName n) <*> (makeJust <$> subspec))
  where subspec = annLocNoSema (combineSrcSpans <$> tokenLocBack AnnOpenP <*> tokenLocBack AnnCloseP) (pure AST.USubSpecAll)
trfIESpec' (IEThingWith n _ ls flds)
  = Just <$> (AST.UIESpec <$> trfImportModifier <*> trfName (getWrappedName n) <*> (makeJust <$> subspec))
  where subspec = annLocNoSema (combineSrcSpans <$> tokenLocBack AnnOpenP <*> tokenLocBack AnnCloseP)
                    $ AST.USubSpecList <$> between AnnOpenP AnnCloseP (makeList ", " atTheStart ((++) <$> mapM (trfName . getWrappedName) ls <*> mapM trfName (map (fmap flSelector) flds)))
trfIESpec' _ = pure Nothing

getWrappedName :: Located (IEWrappedName n) -> Located n
getWrappedName (L _ (IEName n)) = n
getWrappedName (L _ (IEPattern n)) = n
getWrappedName (L _ (IEType n)) = n

-- TODO: easier with wrapped names
trfImportModifier :: Trf (AnnMaybeG AST.UImportModifier (Dom r) RangeStage)
trfImportModifier = do
  patLoc <- tokenLoc AnnPattern
  typLoc <- tokenLoc AnnType
  if | (not . isGoodSrcSpan $ patLoc) && (not . isGoodSrcSpan $ typLoc) -> nothing " " "" atTheStart
     | isGoodSrcSpan patLoc -> makeJust <$> annLocNoSema (return patLoc) (return AST.UImportPattern)
     | isGoodSrcSpan typLoc -> makeJust <$> annLocNoSema (return typLoc) (return AST.UImportType)

trfModuleName :: Located ModuleName -> Trf (Ann AST.UModuleName (Dom r) RangeStage)
trfModuleName = trfLocNoSema trfModuleName'

trfModuleName' :: ModuleName -> Trf (AST.UModuleName (Dom r) RangeStage)
trfModuleName' = pure . AST.UModuleName . moduleNameString
