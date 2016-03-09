{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , StandaloneDeriving
           , DeriveDataTypeable
           , TypeSynonymInstances
           , LambdaCase
           , ViewPatterns
           #-}
module Language.Haskell.Tools.Refactor.OrganizeImports where

import SrcLoc
import Name hiding (Name)
import GHC (GhcMonad, lookupGlobalName, TyThing(..), moduleNameString, moduleName)
import qualified GHC
import TyCon

import Control.Lens hiding (element)
import Control.Lens.Action
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.String
import Data.Maybe
import Data.Data
import Data.List
import Data.Generics.Uniplate.Data
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor.DebugGhcAST
import Debug.Trace

type STWithNames = NodeInfo SemanticInfo SourceTemplate

organizeImports :: Ann Module STWithNames -> Ann Module STWithNames
organizeImports mod
  = mod & element.modImports.annListElems %~ narrowImports usedNames . sortImports
  where usedNames :: [GHC.Name]
        usedNames = catMaybes $ map (preview (annotation.semanticInfo.nameInfo)) $ (universeBi (mod ^. element.modHead) ++ universeBi (mod ^. element.modDecl) :: [Ann Name STWithNames])
        
sortImports :: [Ann ImportDecl STWithNames] -> [Ann ImportDecl STWithNames]
sortImports = sortBy (ordByOccurrence `on` view (element.importModule.element))

narrowImports :: [GHC.Name] -> [Ann ImportDecl STWithNames] -> [Ann ImportDecl STWithNames]
narrowImports usedNames imps 
  = map (\(imp,rest) -> narrowImport usedNames (map semantics rest) imp) 
        (listViews imps)
        
narrowImport :: [GHC.Name] -> [SemanticInfo] -> Ann ImportDecl STWithNames 
                           -> Ann ImportDecl STWithNames
narrowImport usedNames otherModules imp
  | importIsExact (imp ^. element) 
  = imp & element.importSpec.annJust.element.importSpecList %~ replaceList (map (flip mkIeSpec noth . mkUnqualName . occNameString . occName) actuallyImported)
  | otherwise 
  = if null actuallyImported
      then imp & element.importSpec .~ just (mkImportSpecList [])
      else imp
  where actuallyImported = (imp ^. annotation.semanticInfo.importedNames) `intersect` usedNames
    
-- * General utilities

class TemplateAnnot annot where
  fromTemplate :: SourceTemplate -> annot
  getTemplate :: annot -> SourceTemplate
  
instance TemplateAnnot (NodeInfo SemanticInfo SourceTemplate) where
  fromTemplate = NodeInfo NoSemanticInfo
  getTemplate = view sourceInfo
  
instance TemplateAnnot (NodeInfo () SourceTemplate) where
  fromTemplate = NodeInfo ()
  getTemplate = view sourceInfo
    
listViews :: Eq a => [a] -> [(a,[a])]
listViews ls = map (\e -> (e, delete e ls)) ls  
  
semantics :: Ann a STWithNames -> SemanticInfo
semantics = view (annotation.semanticInfo)
    
-- * AST creation
    
mkImportSpecList :: TemplateAnnot a => [Ann IESpec a] -> Ann ImportSpec a
mkImportSpecList specs = Ann (fromTemplate $ "(" <> child <> ")") 
                             (ImportSpecList (AnnList (fromTemplate list) specs))

mkIeSpec :: TemplateAnnot a => Ann Name a -> AnnMaybe SubSpec a -> Ann IESpec a
mkIeSpec name ss = Ann (fromTemplate $ child <> child) (IESpec name ss)
                             
replaceList :: TemplateAnnot a => [Ann e a] -> AnnList e a -> AnnList e a
replaceList elems (AnnList a _)
  = AnnList (fromTemplate (listSep mostCommonSeparator)) elems
  where mostCommonSeparator  
          = case getTemplate a ^. sourceTemplateElems of 
              [ChildListElem seps] -> head $ maximumBy (compare `on` length) $ group $ sort seps
              
mkUnqualName :: TemplateAnnot a => String -> Ann Name a
mkUnqualName n = Ann (fromTemplate $ child <> child) 
                     (Name emptyList (Ann (fromTemplate (fromString n)) 
                                          (SimpleName n)))
              
emptyList :: TemplateAnnot a => AnnList e a
emptyList = AnnList (fromTemplate list) []
              
just :: TemplateAnnot a => Ann e a -> AnnMaybe e a            
just e = AnnMaybe (fromTemplate opt) (Just e)

noth :: TemplateAnnot a => AnnMaybe e a
noth = AnnMaybe (fromTemplate opt) Nothing