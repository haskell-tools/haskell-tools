module Main where

import Criterion.Main

import GHC (runGhc, Ghc)
import GHC.Paths (libdir)

import Control.Reference
import Data.Maybe (fromMaybe)
import System.IO.Silently (silence)

import Language.Haskell.Tools.Rewrite (mkLanguagePragma)
import Language.Haskell.Tools.Refactor hiding (ModuleName)
import Language.Haskell.Tools.Refactor.Builtin.OrganizeExtensions
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.SupportedExtensions

import qualified ExtensionOrganizerBenchmark.ManualTraversal   as Manual
import qualified ExtensionOrganizerBenchmark.UniPlateTraversal as UniPlate
import qualified Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.TraverseAST as ClassyPlate


-- Our benchmark harness.
main :: IO ()
main = do
  mods <- runGhc (Just libdir) . mapM loadModulesWithExtensions $ testModules
  defaultMain [ bgroup "Traversal"
                  [ mkClassyPlateBenchmarks mods
                  , mkManualBenchmarks mods
                  , mkUniPlateBenchmarks mods
                  ]
              ]

completeModules = "examples/CompleteModules"

type ModuleName = String

loadModulesWithExtensions :: ModuleName -> Ghc TypedModule
loadModulesWithExtensions moduleName = do
  modAST <- loadModuleAST completeModules moduleName
  let modAST' = addPragmas modAST
  liftIO . silence . print $ modAST'
  return modAST'
  where pragmas    = mkLanguagePragma . map show $ fullyHandledExtensions
        addPragmas = filePragmas & annListElems .- (:) pragmas

mkBenchmarkWith :: CheckNode TypedModule -> TypedModule -> Benchmark
mkBenchmarkWith trv m = bench modName $ nfIO (runGhc (Just libdir) . collectExtensionsWith trv $ m)
  where modName' = m ^? modHead & annJust & mhName & moduleNameString
        modName  = fromMaybe "UnnamedModule" modName'

mkClassyPlateBenchmark :: TypedModule -> Benchmark
mkClassyPlateBenchmark = mkBenchmarkWith ClassyPlate.traverseModule

mkClassyPlateBenchmarks :: [TypedModule] -> Benchmark
mkClassyPlateBenchmarks = bgroup "ClassyPlate" . map mkClassyPlateBenchmark

mkManualBenchmark :: TypedModule -> Benchmark
mkManualBenchmark = mkBenchmarkWith Manual.traverseModule

mkManualBenchmarks :: [TypedModule] -> Benchmark
mkManualBenchmarks = bgroup "Manual" . map mkManualBenchmark

mkUniPlateBenchmark :: TypedModule -> Benchmark
mkUniPlateBenchmark = mkBenchmarkWith UniPlate.traverseModule

mkUniPlateBenchmarks :: [TypedModule] -> Benchmark
mkUniPlateBenchmarks = bgroup "UniPlate" . map mkUniPlateBenchmark

testModules :: [ModuleName]
testModules = [ "Griddler", "Hitori" ]
