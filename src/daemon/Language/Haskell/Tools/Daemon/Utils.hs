{-# LANGUAGE LambdaCase
           , RankNTypes
           , FlexibleContexts
           #-}
-- | Utility operations for the reprsentation of module collections.
module Language.Haskell.Tools.Daemon.Utils where

import Control.Applicative (Alternative(..))
import Control.Reference ((^.), (.=), (.-))
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Language.Haskell.Tools.Daemon.Representation
import Language.Haskell.Tools.Refactor (SourceFileKey(..), sfkFileName, sfkModuleName)

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

-- | Check if we already generated code for the given module. Based on both module name and source
-- file name.
hasGeneratedCode :: SourceFileKey -> [ModuleCollection SourceFileKey] -> Bool
hasGeneratedCode key = maybe False (\case ModuleCodeGenerated {} -> True; _ -> False)
                         . lookupSFKInSCs key

-- | Check if the given module needs code generation. Finds the module if no source file name is
-- present and module names check or if both module names and source file names check.
needsGeneratedCode :: SourceFileKey -> [ModuleCollection SourceFileKey] -> Bool
needsGeneratedCode key mcs = maybe False (\case ModuleCodeGenerated {} -> True; ModuleNotLoaded True _ -> True; _ -> False)
                              $ lookupSFKInSCs key mcs <|> lookupSFKInSCs (sfkFileName .= "" $ key) mcs

-- | Marks the given module for code generation. Finds the module if no source file name is
-- present and module names check or if both module names and source file names check.
codeGeneratedFor :: SourceFileKey -> [ModuleCollection SourceFileKey] -> [ModuleCollection SourceFileKey]
codeGeneratedFor key = map (mcModules .- Map.adjust setCodeGen (sfkFileName .= "" $ key) . Map.adjust setCodeGen key)
  where setCodeGen (ModuleTypeChecked mod ms) = ModuleCodeGenerated mod ms
        setCodeGen (ModuleNotLoaded _ exp) = ModuleNotLoaded True exp
        setCodeGen m = m

-- | Check if the given module has been already loaded. Based on both module name and source
-- file name.
isAlreadyLoaded :: SourceFileKey -> [ModuleCollection SourceFileKey] -> Bool
isAlreadyLoaded key = maybe False (\case (_, ModuleNotLoaded {}) -> False; _ -> True)
                         . find ((key ==) . fst) . concatMap (Map.assocs . (^. mcModules))
