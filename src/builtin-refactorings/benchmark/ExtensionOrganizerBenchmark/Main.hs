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

-- Our benchmark harness.
main :: IO ()
main = do
  mods <- runGhc (Just libdir) . mapM loadModulesWithExtensions $ testModules
  defaultMain [ bgroup "ExtensionOrganizerBenchmark"
                  (map mkBenchmark mods)
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

mkBenchmark :: TypedModule -> Benchmark
mkBenchmark m = bench modName $ nfIO (runGhc (Just libdir) . collectExtensions $ m)
  where modName' = m ^? modHead & annJust & mhName & moduleNameString
        modName  = fromMaybe "UnnamedModule" modName'

testModules :: [ModuleName]
testModules = [ "Hitori" ]
