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

type STWithNames = NodeInfo SemanticInfo SourceTemplate

organizeImports :: GhcMonad m => Ann Module STWithNames -> m (Ann Module STWithNames)
organizeImports mod
  = return $ mod & element.modImports.annList %~ narrowImports usedNames . sortImports
  where usedNames :: [GHC.Name]
        usedNames = catMaybes $ map (preview (annotation.semanticInfo.nameInfo)) (universeBi mod :: [Ann Name STWithNames])

sortImports :: [Ann ImportDecl STWithNames] -> [Ann ImportDecl STWithNames]
sortImports = sortBy (ordByOccurrence `on` view (element.importModule.element))

narrowImports :: [GHC.Name] -> [Ann ImportDecl STWithNames] -> [Ann ImportDecl STWithNames]
narrowImports usedNames imps 
  = catMaybes $ map (\(imp,rest) -> narrowImport usedNames (map semantics rest) imp) 
                    (listViews imps)
        
narrowImport :: [GHC.Name] -> [SemanticInfo] -> Ann ImportDecl STWithNames 
                           -> Maybe (Ann ImportDecl STWithNames)
narrowImport usedNames otherModules imp
  = if null actuallyImported
      then if canBeRemoved then Nothing 
                           else Just $ imp & element.importSpec .~ annJust (mkImportSpecList [])
      else Just imp
  where actuallyImported = (imp ^. annotation.semanticInfo.importedNames) `intersect` usedNames
        canBeRemoved = any (== (imp ^?! annotation.semanticInfo.importedModule)) 
                           (otherModules ^.. traverse.importedModule)
    
-- * General utilities

class TemplateAnnot annot where
  fromTemplate :: SourceTemplate -> annot
  
instance TemplateAnnot (NodeInfo SemanticInfo SourceTemplate) where
  fromTemplate = NodeInfo NoSemanticInfo
  
instance TemplateAnnot (NodeInfo () SourceTemplate) where
  fromTemplate = NodeInfo ()
    
listViews :: Eq a => [a] -> [(a,[a])]
listViews ls = map (\e -> (e, delete e ls)) ls  
  
semantics :: Ann a STWithNames -> SemanticInfo
semantics = view (annotation.semanticInfo)
    
-- * AST creation
    
mkImportSpecList :: TemplateAnnot a => [Ann IESpec a] -> Ann ImportSpec a
mkImportSpecList specs = Ann (fromTemplate "()") (ImportSpecList (AnnList specs))

