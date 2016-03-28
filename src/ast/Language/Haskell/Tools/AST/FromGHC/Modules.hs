{-# LANGUAGE LambdaCase
           , ViewPatterns
           , FlexibleContexts
           #-}
module Language.Haskell.Tools.AST.FromGHC.Modules where

import Control.Reference hiding (element)
import Data.Maybe
import Data.List
import Data.Map as Map hiding (map, filter)
import Data.IORef
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data
import Data.StructuralTraversal
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
import Outputable as GHC
import TyCon as GHC
import ConLike as GHC
import DataCon as GHC
import Bag as GHC
import Var as GHC

import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Base as AST
import qualified Language.Haskell.Tools.AST.Modules as AST

import Language.Haskell.Tools.AST.Helpers
import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.Decls
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

addTypeInfos :: LHsBinds Id -> Ann AST.Module RangeWithName -> Ghc (Ann AST.Module RangeWithType)
addTypeInfos bnds = traverseUp (return ()) (return ()) replaceNodeInfo
  where replaceNodeInfo :: RangeWithName -> Ghc RangeWithType
        replaceNodeInfo = semanticInfo !~ replaceSemanticInfo
        replaceSemanticInfo NoSemanticInfo = return NoSemanticInfo
        replaceSemanticInfo (NameInfo ni) = NameInfo <$> getType ni
        replaceSemanticInfo (ImportInfo mod access used) = ImportInfo mod <$> mapM getType access <*> mapM getType used
        getType name 
          = lookupName name >>= \case
              Just (AnId id) -> return $ mkVanillaGlobal name (varType id)
              Just (AConLike (RealDataCon dc)) -> return $ mkVanillaGlobal name (dataConUserType dc)
              Just (ATyCon tc) -> return $ mkVanillaGlobal name (tyConKind tc)
              Nothing -> case Map.lookup name mapping of 
                           Just x -> return x
                           Nothing -> error $ "Type of name " ++ showSDocUnsafe (ppr name) ++ " cannot be found"
        mapping = Map.fromList $ map (\id -> (getName id, id)) $ extractTypes bnds

extractTypes :: LHsBinds Id -> [Id]
extractTypes = concatMap universeBi . bagToList


trfModule :: Located (HsModule RdrName) -> Trf (Ann AST.Module RangeInfo)
trfModule = trfLocCorrect (\sr -> combineSrcSpans sr <$> (uniqueTokenAnywhere AnnEofPos)) $ 
  \(HsModule name exports imports decls deprec haddock) -> 
    AST.Module <$> trfPragmas deprec haddock
               <*> trfModuleHead name exports
               <*> trfImports imports
               <*> trfDecls decls
       
trfModuleRename :: (HsGroup Name, [LImportDecl Name], Maybe [LIE Name], Maybe LHsDocString) -> Located (HsModule RdrName) -> Trf (Ann AST.Module RangeWithName)
trfModuleRename (gr,imports,exps,_) 
  = trfLocCorrect (\sr -> combineSrcSpans sr <$> (uniqueTokenAnywhere AnnEofPos)) $ 
      \(HsModule name exports _ decls deprec haddock) -> 
        AST.Module <$> trfPragmas deprec haddock
                   <*> trfModuleHead name (case (exports, exps) of (Just (L l _), Just ie) -> Just (L l ie)
                                                                   _                       -> Nothing)
                   <*> (orderAnnList <$> (trfImports imports))
                   <*> trfDeclsGroup gr
       
trfModuleHead :: TransformName n r => Maybe (Located ModuleName) -> Maybe (Located [LIE n]) -> Trf (AnnMaybe AST.ModuleHead r) 
trfModuleHead (Just mn) exports 
  = makeJust <$> (annLoc (tokensLoc [AnnModule, AnnWhere])
                         (AST.ModuleHead <$> trfModuleName mn 
                                         <*> trfExportList exports))
trfModuleHead Nothing _ = nothing "" "" moduleHeadPos
  where moduleHeadPos = after AnnClose >>= \case loc@(RealSrcLoc _) -> return loc
                                                 _ -> atTheStart

trfPragmas :: RangeAnnot a => Maybe (Located WarningTxt) -> Maybe LHsDocString -> Trf (AnnList AST.ModulePragma a)
trfPragmas _ _ = makeList "\n" atTheStart (pure [])

trfExportList :: TransformName n r => Maybe (Located [LIE n]) -> Trf (AnnMaybe AST.ExportSpecList r)
trfExportList = trfMaybe " " "" $ trfLoc trfExportList'

trfExportList' :: TransformName n r => [LIE n] -> Trf (AST.ExportSpecList r)
trfExportList' exps = AST.ExportSpecList <$> (makeList ", " (after AnnOpenP) (orderDefs . catMaybes <$> (mapM trfExport exps)))
  
trfExport :: TransformName n r => LIE n -> Trf (Maybe (Ann AST.ExportSpec r))
trfExport = trfMaybeLoc $ \case 
  IEModuleContents n -> Just . AST.ModuleExport <$> (trfModuleName n)
  other -> fmap AST.DeclExport <$> trfIESpec' other
  
trfImports :: TransformName n r => [LImportDecl n] -> Trf (AnnList AST.ImportDecl r)
trfImports imps 
  = AnnList <$> importDefaultLoc <*> mapM trfImport (filter (not . ideclImplicit . unLoc) imps)
  where importDefaultLoc = toListAnnot "\n" . srcSpanEnd 
                             <$> (combineSrcSpans <$> asks (srcLocSpan . srcSpanStart . contRange) 
                                                  <*> tokenLoc AnnWhere)
trfImport :: TransformName n r => LImportDecl n -> Trf (Ann AST.ImportDecl r)
trfImport = (addImportData <=<) $ trfLoc $ \(GHC.ImportDecl src name pkg isSrc isSafe isQual isImpl declAs declHiding) ->
  let -- default positions of optional parts of an import declaration
      annBeforeQual = if isSrc then AnnClose else AnnImport
      annBeforeSafe = if isQual then AnnQualified else annBeforeQual
      annBeforePkg = if isSafe then AnnSafe else annBeforeSafe
      atAsPos = if isJust declHiding then before AnnOpenP else atTheEnd
  in AST.ImportDecl 
       <$> (if isSrc then makeJust <$> annLoc (tokensLoc [AnnOpen, AnnClose]) (pure AST.ImportSource)
                     else nothing " " "" (after AnnImport))
       <*> (if isQual then makeJust <$> (annLoc (tokenLoc AnnQualified) (pure AST.ImportQualified)) 
                      else nothing " " "" (after annBeforeQual))
       <*> (if isSafe then makeJust <$> (annLoc (tokenLoc AnnSafe) (pure AST.ImportSafe)) 
                      else nothing " " "" (after annBeforeSafe))
       <*> maybe (nothing " " "" (after annBeforePkg)) 
                 (\str -> makeJust <$> (annLoc (tokenLoc AnnPackageName) (pure (AST.StringNode (unpackFS str))))) pkg
       <*> trfModuleName name 
       <*> maybe (nothing " " "" atAsPos) (\mn -> makeJust <$> (trfRenaming mn)) declAs
       <*> trfImportSpecs declHiding
  where trfRenaming mn
          = annLoc (tokensLoc [AnnAs,AnnVal])
                   (AST.ImportRenaming <$> (annLoc (tokenLoc AnnVal) 
                                           (trfModuleName' mn)))  
  
trfImportSpecs :: TransformName n r => Maybe (Bool, Located [LIE n]) -> Trf (AnnMaybe AST.ImportSpec r)
trfImportSpecs (Just (True, l)) 
  = makeJust <$> trfLoc (\specs -> AST.ImportSpecHiding <$> (makeList ", " (after AnnOpenP) (catMaybes <$> mapM trfIESpec specs))) l
trfImportSpecs (Just (False, l)) 
  = makeJust <$> trfLoc (\specs -> AST.ImportSpecList <$> (makeList ", " (after AnnOpenP) (catMaybes <$> mapM trfIESpec specs))) l
trfImportSpecs Nothing = nothing " " "" atTheEnd
    
trfIESpec :: TransformName n r => LIE n -> Trf (Maybe (Ann AST.IESpec r)) 
trfIESpec = trfMaybeLoc trfIESpec'
  
trfIESpec' :: TransformName n r => IE n -> Trf (Maybe (AST.IESpec r))
trfIESpec' (IEVar n) = Just <$> (AST.IESpec <$> trfName n <*> (nothing "(" ")" atTheEnd))
trfIESpec' (IEThingAbs n) = Just <$> (AST.IESpec <$> trfName n <*> (nothing "(" ")" atTheEnd))
trfIESpec' (IEThingAll n) 
  = Just <$> (AST.IESpec <$> trfName n <*> (makeJust <$> (annLoc (tokenLoc AnnDotdot) (pure AST.SubSpecAll))))
trfIESpec' (IEThingWith n ls)
  = Just <$> (AST.IESpec <$> trfName n
                         <*> (makeJust <$> between AnnOpenP AnnCloseP 
                                                  (annCont $ AST.SubSpecList <$> makeList ", " (after AnnOpenP) (mapM trfName ls))))
trfIESpec' _ = pure Nothing
  
 