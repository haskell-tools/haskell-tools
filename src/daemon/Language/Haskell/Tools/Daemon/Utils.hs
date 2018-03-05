{-# LANGUAGE FlexibleContexts, RankNTypes #-}

-- | Utility operations for the reprsentation of module collections.
module Language.Haskell.Tools.Daemon.Utils where

import Control.Applicative (Alternative(..))
import Control.Reference
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import GHC (ModSummary(..))

import Language.Haskell.Tools.Daemon.Representation
import Language.Haskell.Tools.Refactor

-- | Get the name of a module collection from its Id.
moduleCollectionPkgId :: ModuleCollectionId -> Maybe String
moduleCollectionPkgId (DirectoryMC _) = Nothing
moduleCollectionPkgId (LibraryMC id) = Just id
moduleCollectionPkgId (ExecutableMC id _) = Just id
moduleCollectionPkgId (TestSuiteMC id _) = Just id
moduleCollectionPkgId (BenchmarkMC id _) = Just id

-- TODO: check that these are all needed and they are used right.

-- | Find the module collection where the given module is contained. Based on module name.
lookupModuleColl :: String -> [ModuleCollection SourceFileKey] -> Maybe (ModuleCollection SourceFileKey)
lookupModuleColl moduleName = find (any ((moduleName ==) . (^. sfkModuleName)) . Map.keys . (^. mcModules))

-- | Find the module collection where the given module is contained. Based on source file name.
lookupSourceFileColl :: FilePath -> [ModuleCollection SourceFileKey] -> Maybe (ModuleCollection SourceFileKey)
lookupSourceFileColl fp = find (any ((fp ==) . (^. sfkFileName)) . Map.keys . (^. mcModules))

-- | Find the module collection where the given module is contained. Based on source file name and module name.
lookupModuleCollection :: ModSummary -> [ModuleCollection SourceFileKey] -> Maybe (ModuleCollection SourceFileKey)
lookupModuleCollection ms mcs = lookupSourceFileColl (getModSumOrig ms) mcs <|> lookupModuleColl (getModSumName ms) mcs

-- | Find the module with the given name. Based on module name.
lookupModuleInSCs :: String -> [ModuleCollection SourceFileKey] -> Maybe (SourceFileKey, ModuleRecord)
lookupModuleInSCs moduleName = find ((moduleName ==) . (^. sfkModuleName) . fst) . concatMap (Map.assocs . (^. mcModules))

-- | Find the module with the given name. Based on source file name.
lookupModInSCs :: SourceFileKey -> [ModuleCollection SourceFileKey] -> Maybe (SourceFileKey, ModuleRecord)
lookupModInSCs key = find (((key ^. sfkFileName) ==) . (^. sfkFileName) . fst) . concatMap (Map.assocs . (^. mcModules))

-- | Get the module with the given name and source file key.
lookupSFKInSCs :: SourceFileKey -> [ModuleCollection SourceFileKey] -> Maybe ModuleRecord
lookupSFKInSCs key = listToMaybe . catMaybes . map (Map.lookup key . (^. mcModules))

-- | Remove a module with the given name. Based on module name.
removeModule :: String -> [ModuleCollection SourceFileKey] -> [ModuleCollection SourceFileKey]
removeModule moduleName = map (mcModules .- Map.filterWithKey (\k _ -> moduleName /= (k ^. sfkModuleName)))

-- | Remove a module with the given name from a module collection. Based on module name and file path.
removeModuleMS :: ModSummary -> Map.Map SourceFileKey ModuleRecord -> Map.Map SourceFileKey ModuleRecord
removeModuleMS ms = Map.filterWithKey (\k _ -> stay k)
  where stay k = getModSumName ms /= (k ^. sfkModuleName) || (fn /= getModSumOrig ms && fn /= "")
          where fn = k ^. sfkFileName

-- | Check if the given module needs code generation. Finds the module if no source file name is
-- present and module names check or if both module names and source file names check.
needsGeneratedCode :: SourceFileKey -> [ModuleCollection SourceFileKey] -> CodeGenPolicy
needsGeneratedCode key mcs = fromMaybe NoCodeGen 
                               $ (^? modRecCodeGen)
                                   =<< (lookupSFKInSCs key mcs <|> lookupSFKInSCs (sfkFileName .= "" $ key) mcs)

-- | Marks the given module for code generation. Finds the module if no source file name is
-- present and module names check or if both module names and source file names check.
-- Idempotent operation.
codeGeneratedFor :: SourceFileKey -> CodeGenPolicy -> [ModuleCollection SourceFileKey] -> [ModuleCollection SourceFileKey]
codeGeneratedFor key codeGen = map (mcModules .- Map.adjust (modRecCodeGen .= codeGen) (sfkFileName .= "" $ key) 
                                 . Map.adjust (modRecCodeGen .= codeGen) key)

-- | Check if the given module has been already loaded. Based on both module name and source
-- file name.
isAlreadyLoaded :: SourceFileKey -> CodeGenPolicy -> [ModuleCollection SourceFileKey] -> Bool
isAlreadyLoaded key codeGen = maybe False (\(_, mc) -> isLoaded mc && (mc ^? modRecCodeGen) < Just codeGen)
                                . find ((key ==) . fst) . concatMap (Map.assocs . (^. mcModules))

-- | Insert a module with a source file key to our database if it wasn't there already
insertIfMissing :: SourceFileKey -> [ModuleCollection SourceFileKey] -> [ModuleCollection SourceFileKey]
insertIfMissing sfk mods
  = case lookupSFKInSCs sfk mods of
      Just _ -> mods
      Nothing -> element 0 & mcModules .- Map.insert sfk (ModuleNotLoaded NoCodeGen False) $ mods
