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
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor.DebugGhcAST

type STWithNames = NodeInfo SemanticInfo SourceTemplate

deriving instance Data SemanticInfo

organizeImports :: GhcMonad m => Ann Module STWithNames -> m ()
organizeImports mod
  = do -- res <- mod & modImports.annList.act ^! (narrowImports usedNames . groupImports . sortImports)
       mapM_ (\imp -> do names <- filterM (isActuallyImported (imp ^. element)) usedNames
                         liftIO . putStrLn $ prettyPrint imp ++ " -> " ++ show names) 
             (mod ^. element.modImports.annList)
  where usedNames :: [GHC.Name]
        usedNames = catMaybes $ map (preview (annotation.semanticInfo.nameInfo)) (universeBi mod :: [Ann Name STWithNames])

sortImports :: [Ann ImportDecl STWithNames] -> [Ann ImportDecl STWithNames]
sortImports = sortBy (ordByOccurrence `on` view (element.importModule.element))

isActuallyImported :: GhcMonad m => ImportDecl STWithNames -> GHC.Name -> m Bool
isActuallyImported imp name 
  = do liftIO $ putStrLn (nameString $ imp ^. importModule.element)
       ms <- GHC.getModSummary (GHC.mkModuleName $ nameString $ imp ^. importModule.element)
       Just mi <- GHC.getModuleInfo (GHC.ms_mod ms)
       (GHC.modInfoIsExportedName mi name &&) <$> checkImportVisible imp name
  
checkImportVisible :: GhcMonad m => ImportDecl STWithNames -> GHC.Name -> m Bool
checkImportVisible imp name
  | importIsExact imp 
  = or <$> mapM (`ieSpecMatches` name) (imp ^.. importExacts)
  | importIsHiding imp 
  = not . or <$> mapM (`ieSpecMatches` name) (imp ^.. importHidings)
  | otherwise = return True

ieSpecMatches :: GhcMonad m => IESpec STWithNames -> GHC.Name -> m Bool
ieSpecMatches (IESpec ((^?! annotation.semanticInfo.nameInfo) -> n) ss) name
  | n == name = return True
  | isTyConName n
  = (\case Just (ATyCon tc) -> name `elem` map getName (tyConDataCons tc)) 
             <$> lookupGlobalName n
  | otherwise = return False
        
narrowImports :: [GHC.Name] -> [[Ann ImportDecl STWithNames]] -> [Ann ImportDecl STWithNames]
narrowImports names = undefined
        
narrowImport :: [GHC.Name] -> Ann ImportDecl STWithNames -> Ann ImportDecl STWithNames
narrowImport names (Ann l imp) 
  = undefined -- imp & importSpec.importSpecList
    
-- * General utilities

class TemplateAnnot annot where
  fromTemplate :: SourceTemplate -> annot
  
instance TemplateAnnot (NodeInfo (Maybe a) SourceTemplate) where
  fromTemplate = NodeInfo Nothing
  
instance TemplateAnnot (NodeInfo () SourceTemplate) where
  fromTemplate = NodeInfo ()
    
-- * AST creation
    


