{-# LANGUAGE TemplateHaskell
           , RecordWildCards
           , FlexibleContexts
           #-}
-- | Representation of the modules and packages in the daemon session.
module Language.Haskell.Tools.Daemon.Representation where

import Control.Reference
import Data.Function (on)
import Data.Map.Strict as Map
import Data.Maybe

import DynFlags
import GHC

import Language.Haskell.Tools.Refactor

-- | The modules of a library, executable, test or benchmark. A package contains one or more module collection.
data ModuleCollection k
  = ModuleCollection { _mcId :: ModuleCollectionId
                     , _mcRoot :: FilePath
                     , _mcSourceDirs :: [FilePath]
                     , _mcModuleFiles :: [(ModuleNameStr, FilePath)]
                     , _mcModules :: (Map.Map k ModuleRecord)
                     , _mcFlagSetup :: (DynFlags -> IO DynFlags) -- ^ Sets up the ghc environment for compiling the modules of this collection
                     , _mcLoadFlagSetup :: (DynFlags -> IO DynFlags) -- ^ Sets up the ghc environment for dependency analysis
                     , _mcDependencies :: [ModuleCollectionId]
                     }

modCollToSfk :: ModuleCollection ModuleNameStr -> ModuleCollection SourceFileKey
modCollToSfk ModuleCollection{..} = ModuleCollection{ _mcModules = Map.mapKeys (SourceFileKey "") _mcModules, ..}

-- | The state of a module.
data ModuleRecord
      = ModuleNotLoaded { _recModuleWillNeedCode :: Bool
                        , _recModuleExposed :: Bool
                        }
      | ModuleParsed { _parsedRecModule :: UnnamedModule (Dom RdrName)
                     , _modRecMS :: ModSummary
                     }
      | ModuleRenamed { _renamedRecModule :: UnnamedModule (Dom GHC.Name)
                      , _modRecMS :: ModSummary
                      }
      | ModuleTypeChecked { _typedRecModule :: UnnamedModule IdDom
                          , _modRecMS :: ModSummary
                          }
      | ModuleCodeGenerated { _typedRecModule :: UnnamedModule IdDom
                            , _modRecMS :: ModSummary
                            }

-- | An alias for module names
type ModuleNameStr = String

-- | This data structure identifies a module collection.
data ModuleCollectionId = DirectoryMC FilePath
                        | LibraryMC String
                        | ExecutableMC String String
                        | TestSuiteMC String String
                        | BenchmarkMC String String
 deriving (Eq, Ord, Show)

instance Eq (ModuleCollection k) where
  (==) = (==) `on` _mcId

instance Show k => Show (ModuleCollection k) where
  show (ModuleCollection id root srcDirs mapping mods _ _ deps)
    = "ModuleCollection (" ++ show id ++ ") " ++ root ++ " " ++ show srcDirs ++ " " ++ show mapping
        ++ " (" ++ show mods ++ ") " ++ show deps

makeReferences ''ModuleCollection
makeReferences ''ModuleRecord

instance Show ModuleRecord where
  show (ModuleNotLoaded code exposed) = "ModuleNotLoaded " ++ show code ++ " " ++ show exposed
  show mr@(ModuleParsed {}) = "ModuleParsed (" ++ (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod $ fromJust $ mr ^? modRecMS) ++ ")"
  show mr@(ModuleRenamed {}) = "ModuleRenamed (" ++ (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod $ fromJust $ mr ^? modRecMS) ++ ")"
  show mr@(ModuleTypeChecked {}) = "ModuleTypeChecked (" ++ (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod $ fromJust $ mr ^? modRecMS) ++ ")"
  show mr@(ModuleCodeGenerated {}) = "ModuleCodeGenerated (" ++ (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod $ fromJust $ mr ^? modRecMS) ++ ")"
