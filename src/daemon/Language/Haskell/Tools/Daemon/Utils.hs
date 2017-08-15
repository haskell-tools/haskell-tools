{-# LANGUAGE LambdaCase
           , RankNTypes
           , FlexibleContexts
           #-}
-- | Utility operations for the reprsentation of module collections.
module Language.Haskell.Tools.Daemon.Utils where

import Control.Applicative (Alternative(..))
import Control.Reference
import Data.List
import Data.Maybe
import qualified Data.Map as Map

import Language.Haskell.Tools.Daemon.Representation
import Language.Haskell.Tools.Refactor (SourceFileKey(..), sfkFileName, sfkModuleName)

containingMC :: FilePath -> Simple Traversal [ModuleCollection k] (ModuleCollection k)
containingMC fp = traversal & filtered (\mc -> _mcRoot mc `isPrefixOf` fp)

moduleCollectionIdString :: ModuleCollectionId -> String
moduleCollectionIdString (DirectoryMC fp) = fp
moduleCollectionIdString (LibraryMC id) = id
moduleCollectionIdString (ExecutableMC _ id) = id
moduleCollectionIdString (TestSuiteMC _ id) = id
moduleCollectionIdString (BenchmarkMC _ id) = id

moduleCollectionPkgId :: ModuleCollectionId -> Maybe String
moduleCollectionPkgId (DirectoryMC _) = Nothing
moduleCollectionPkgId (LibraryMC id) = Just id
moduleCollectionPkgId (ExecutableMC id _) = Just id
moduleCollectionPkgId (TestSuiteMC id _) = Just id
moduleCollectionPkgId (BenchmarkMC id _) = Just id

modRecLoaded :: ModuleRecord -> Bool
modRecLoaded ModuleTypeChecked{} = True
modRecLoaded ModuleCodeGenerated{} = True
modRecLoaded _ = False

-- | Find the module collection where the given module is.
lookupModuleColl :: String -> [ModuleCollection SourceFileKey] -> Maybe (ModuleCollection SourceFileKey)
lookupModuleColl moduleName = find (any ((moduleName ==) . (^. sfkModuleName)) . Map.keys . (^. mcModules))

lookupModuleInSCs :: String -> [ModuleCollection SourceFileKey] -> Maybe (SourceFileKey, ModuleRecord)
lookupModuleInSCs moduleName = find ((moduleName ==) . (^. sfkModuleName) . fst) . concatMap (Map.assocs . (^. mcModules))

lookupSourceFileColl :: FilePath -> [ModuleCollection SourceFileKey] -> Maybe (ModuleCollection SourceFileKey)
lookupSourceFileColl fp = find (any ((fp ==) . (^. sfkFileName)) . Map.keys . (^. mcModules))

lookupModInSCs :: SourceFileKey -> [ModuleCollection SourceFileKey] -> Maybe (SourceFileKey, ModuleRecord)
lookupModInSCs moduleName = find (((moduleName ^. sfkFileName) ==) . (^. sfkFileName) . fst) . concatMap (Map.assocs . (^. mcModules))

lookupSFKInSCs :: SourceFileKey -> [ModuleCollection SourceFileKey] -> Maybe ModuleRecord
lookupSFKInSCs key = listToMaybe . catMaybes . map (Map.lookup key . (^. mcModules))

lookupSourceFileInSCs :: String -> [ModuleCollection SourceFileKey] -> Maybe (SourceFileKey, ModuleRecord)
lookupSourceFileInSCs fileName = find ((fileName ==) . (^. sfkFileName) . fst) . concatMap (Map.assocs . (^. mcModules))

removeModule :: String -> [ModuleCollection SourceFileKey] -> [ModuleCollection SourceFileKey]
removeModule moduleName = map (mcModules .- Map.filterWithKey (\k _ -> moduleName /= (k ^. sfkModuleName)))

hasGeneratedCode :: SourceFileKey -> [ModuleCollection SourceFileKey] -> Bool
hasGeneratedCode key = maybe False (\case ModuleCodeGenerated {} -> True; _ -> False)
                         . lookupSFKInSCs key

needsGeneratedCode :: SourceFileKey -> [ModuleCollection SourceFileKey] -> Bool
needsGeneratedCode key mcs = maybe False (\case ModuleCodeGenerated {} -> True; ModuleNotLoaded True _ -> True; _ -> False)
                              $ lookupSFKInSCs key mcs <|> lookupSFKInSCs (sfkFileName .= "" $ key) mcs

codeGeneratedFor :: SourceFileKey -> [ModuleCollection SourceFileKey] -> [ModuleCollection SourceFileKey]
codeGeneratedFor key = map (mcModules .- Map.adjust setCodeGen (sfkFileName .= "" $ key) . Map.adjust setCodeGen key)
  where setCodeGen (ModuleTypeChecked mod ms) = ModuleCodeGenerated mod ms
        setCodeGen (ModuleNotLoaded _ exp) = ModuleNotLoaded True exp
        setCodeGen m = m

isAlreadyLoaded :: SourceFileKey -> [ModuleCollection SourceFileKey] -> Bool
isAlreadyLoaded key = maybe False (\case (_, ModuleNotLoaded {}) -> False; _ -> True)
                         . find ((key ==) . fst) . concatMap (Map.assocs . (^. mcModules))
