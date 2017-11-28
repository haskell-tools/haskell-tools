{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (sort, isSuffixOf, isInfixOf)
import Data.Maybe
import Network.Socket hiding (KeepAlive, send, recv)
import Network.Socket.ByteString.Lazy as Sock (sendAll, recv)
import System.Directory
import System.Environment (unsetEnv)
import System.Exit (ExitCode(..))
import System.FilePath (FilePath(..), (</>))
import System.FilePath.Glob (glob)
import System.IO
import System.IO.Error (catchIOError)
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

import FastString (mkFastString)
import SrcLoc (SrcSpan(..), mkSrcSpan, mkSrcLoc)

import Language.Haskell.Tools.Daemon (runDaemon')
import Language.Haskell.Tools.Daemon.Options as Options (SharedDaemonOptions(..), DaemonOptions(..))
import Language.Haskell.Tools.Daemon.PackageDB (PackageDB(..))
import Language.Haskell.Tools.Daemon.Protocol
import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings)

pORT_NUM_START = 4100
pORT_NUM_END = 4200

main :: IO ()
main = do unsetEnv "GHC_PACKAGE_PATH"
          portCounter <- newMVar pORT_NUM_START
          tr <- canonicalizePath testRoot
          hasStack <- isJust <$> findExecutable "stack"
          hasCabal <- isJust <$> findExecutable "cabal"
          defaultMain (allTests (hasStack && hasCabal) tr portCounter)

allTests :: Bool -> FilePath -> MVar Int -> TestTree
allTests isSource testRoot portCounter
  = localOption (mkTimeout ({- 30s -} 1000 * 1000 * 30))
      $ testGroup "daemon-tests"
          [ testGroup "simple-tests"
              $ map (makeDaemonTest portCounter) simpleTests
          , testGroup "loading-tests"
              $ map (makeDaemonTest portCounter) (loadingTests testRoot)
          , testGroup "complex-loading-tests"
              $ map (makeComplexLoadTest portCounter) (complexLoadingTests testRoot)
          , testGroup "refactor-tests"
              $ map (makeRefactorTest portCounter) (refactorTests testRoot)
          , testGroup "reload-tests"
              $ map (makeReloadTest portCounter) (reloadingTests testRoot)
          , testGroup "undo-tests"
              $ map (makeUndoTest portCounter) (undoTests testRoot)
          , testGroup "compilation-problem-tests"
              $ map (makeCompProblemTest portCounter) (compProblemTests isSource testRoot)
          , testGroup "special-tests"
              $ map (makeSpecialTest portCounter) (specialTests testRoot)
          -- if not a stack build, we cannot guarantee that stack is on the path
          , if isSource
             then testGroup "pkg-db-tests" $ map (makePkgDbTest portCounter) (pkgDbTests testRoot)
             else testCase "IGNORED pkg-db-tests" (return ())
          -- cannot execute this when the source is not present
          -- , if isSource then selfLoadingTest portCounter else testCase "IGNORED self-load" (return ())
          ]

testSuffix = "_test"

simpleTests :: [(String, [ClientMessage], [ResponseMsg])]
simpleTests =
  [ ( "empty-test", [], [] )
  , ( "keep-alive", [KeepAlive], [KeepAliveResponse] )
  ]

loadingTests :: FilePath -> [(String, [ClientMessage], [ResponseMsg])]
loadingTests testRoot =
  [ ( "load-package"
    , [AddPackages [testRoot </> "has-cabal"]]
    , [ LoadingModules [testRoot </> "has-cabal" </> "A.hs"]
      , LoadedModule (testRoot </> "has-cabal" </> "A.hs") "A" ] )
  , ( "same-module-in-two-mod-coll"
    , [AddPackages [testRoot </> "same-module-in-two-mod-coll"]]
    , [ LoadingModules [testRoot </> "same-module-in-two-mod-coll" </> "A.hs"
      , testRoot </> "same-module-in-two-mod-coll" </> "Main.hs"]
      , LoadedModule (testRoot </> "same-module-in-two-mod-coll" </> "A.hs") "A"
      , LoadedModule (testRoot </> "same-module-in-two-mod-coll" </> "Main.hs") "Main" ] )
  , ( "exposed-mod-as-main"
    , [AddPackages [testRoot </> "exposed-mod-as-main"]]
    , [ LoadingModules [testRoot </> "exposed-mod-as-main" </> "A.hs"]
      , LoadedModule (testRoot </> "exposed-mod-as-main" </> "A.hs") "A" ] )
  , ( "no-cabal"
    , [AddPackages [testRoot </> "no-cabal"]]
    , [ LoadingModules [testRoot </> "no-cabal" </> "A.hs"]
      , LoadedModule (testRoot </> "no-cabal" </> "A.hs") "A"] )
  , ( "code-gen"
    , [AddPackages [testRoot </> "code-gen"]]
    , [ LoadingModules [testRoot </> "code-gen" </> "B.hs", testRoot </> "code-gen" </> "A.hs"]
      , LoadedModule (testRoot </> "code-gen" </> "B.hs") "B"
      , LoadedModule (testRoot </> "code-gen" </> "A.hs") "A" ] )
  ,( "th-typecheck"
    , [AddPackages [testRoot </> "th-typecheck"]]
    , [ LoadingModules [testRoot </> "th-typecheck" </> "A.hs"]
      , LoadedModule (testRoot </> "th-typecheck" </> "A.hs") "A"] )
  , ( "source-dir"
    , [AddPackages [testRoot </> "source-dir"]]
    , [ LoadingModules [testRoot </> "source-dir" </> "src" </> "A.hs"]
      , LoadedModule (testRoot </> "source-dir" </> "src" </> "A.hs") "A"] )
  , ( "source-dir-outside"
    , [AddPackages [testRoot </> "source-dir-outside"]]
    , [ LoadingModules [testRoot </> "source-dir-outside" </> ".." </> "src" </> "A.hs"]
      , LoadedModule (testRoot </> "source-dir-outside" </> ".." </> "src" </> "A.hs") "A"] )
  , ( "multi-packages"
    , [ AddPackages [ testRoot </> "multi-packages" </> "package1"
                    , testRoot </> "multi-packages" </> "package2" ]]
    , [ LoadingModules [ testRoot </> "multi-packages" </> "package1" </> "A.hs"
                       , testRoot </> "multi-packages" </> "package2" </> "B.hs" ]
      , LoadedModule (testRoot </> "multi-packages" </> "package1" </> "A.hs") "A"
      , LoadedModule (testRoot </> "multi-packages" </> "package2" </> "B.hs") "B" ] )
  , ( "multi-packages-flags"
    , [ AddPackages [ testRoot </> "multi-packages-flags" </> "package1"
                    , testRoot </> "multi-packages-flags" </> "package2" ]]
    , [ LoadingModules [ testRoot </> "multi-packages-flags" </> "package1" </> "A.hs"
                       , testRoot </> "multi-packages-flags" </> "package2" </> "B.hs" ]
      , LoadedModule (testRoot </> "multi-packages-flags" </> "package1" </> "A.hs") "A"
      , LoadedModule (testRoot </> "multi-packages-flags" </> "package2" </> "B.hs") "B" ] )
  , ( "multi-packages-dependent"
    , [ AddPackages [ testRoot </> "multi-packages-dependent" </> "package1"
                    , testRoot </> "multi-packages-dependent" </> "package2" ]]
    , [ LoadingModules [ testRoot </> "multi-packages-dependent" </> "package1" </> "A.hs"
                       , testRoot </> "multi-packages-dependent" </> "package2" </> "B.hs" ]
      , LoadedModule (testRoot </> "multi-packages-dependent" </> "package1" </> "A.hs") "A"
      , LoadedModule (testRoot </> "multi-packages-dependent" </> "package2" </> "B.hs") "B" ] )
  , ( "has-th"
    , [AddPackages [testRoot </> "has-th"]]
    , [ LoadingModules [ testRoot </> "has-th" </> "TH.hs", testRoot </> "has-th" </> "A.hs" ]
      , LoadedModule (testRoot </> "has-th" </> "TH.hs") "TH"
      , LoadedModule (testRoot </> "has-th" </> "A.hs") "A" ] )
  , ( "th-added-later"
    , [ AddPackages [testRoot </> "th-added-later" </> "package1"]
      , AddPackages [testRoot </> "th-added-later" </> "package2"]
      ]
    , [ LoadingModules [ testRoot </> "th-added-later" </> "package1" </> "A.hs" ]
      , LoadedModule (testRoot </> "th-added-later" </> "package1" </> "A.hs") "A"
      , LoadingModules [ testRoot </> "th-added-later" </> "package2" </> "B.hs" ]
      , LoadedModule (testRoot </> "th-added-later" </> "package2" </> "B.hs") "B" ] )
  , ( "unused-module"
    , [ AddPackages [testRoot </> "unused-mod"] ]
    , [ LoadingModules [ testRoot </> "unused-mod" </> "Main.hs" ]
      , LoadedModule (testRoot </> "unused-mod" </> "Main.hs") "Main" ] )
  , ( "additional-files"
    , [ SetPackageDB DefaultDB, AddPackages [testRoot </> "additional-files"] ]
    , [ LoadingModules [ testRoot </> "additional-files" </> "B.hs"
                       , testRoot </> "additional-files" </> "A.hs" ]
      , LoadedModule (testRoot </> "additional-files" </> "B.hs") "B"
      , LoadedModule (testRoot </> "additional-files" </> "A.hs") "A"
      ])
  ]

complexLoadingTests :: FilePath -> [(String, FilePath, [ClientMessage], [ResponseMsg] -> Bool)]
complexLoadingTests testRoot =
  [ ( "paths-module", testRoot </> "paths-module"
    , [AddPackages [testRoot </> "paths-module"]]
    , \case [ LoadingModules [paths, a], LoadedModule{}, LoadedModule{}
              ] -> "Paths_some_test_package.hs" `isSuffixOf` paths
                      && "A.hs" `isSuffixOf` a
              ; _ -> False)
  , ( "paths-codegen", testRoot </> "paths-codegen"
    , [AddPackages [testRoot </> "paths-codegen"]]
    , \case [ LoadingModules [paths, a], LoadedModule{}, LoadedModule{}
              ] -> "Paths_some_test_package.hs" `isSuffixOf` paths
                      && "A.hs" `isSuffixOf` a
              ; _ -> False)
  ]

compProblemTests :: Bool -> FilePath -> [(String, [Either (IO ()) ClientMessage], [ResponseMsg] -> Bool)]
compProblemTests isSource testRoot =
  [ ( "load-error"
    , [ Right $ SetPackageDB DefaultDB, Right $ AddPackages [testRoot </> "load-error"] ]
    , \case [LoadingModules{}, CompilationProblem {}] -> True; _ -> False)
  , ( "source-error"
    , [ Right $ SetPackageDB DefaultDB, Right $ AddPackages [testRoot </> "source-error"] ]
    , \case [LoadingModules{}, CompilationProblem {}] -> True; _ -> False)
  , ( "reload-error"
    , [ Right $ SetPackageDB DefaultDB, Right $ AddPackages [testRoot </> "empty"]
      , Left $ appendFile (testRoot </> "empty" </> "A.hs") "\n\nimport No.Such.Module"
      , Right $ ReLoad [] [testRoot </> "empty" </> "A.hs"] []
      , Left $ writeFile (testRoot </> "empty" </> "A.hs") "module A where"]
    , \case [LoadingModules {}, LoadedModule {}, LoadingModules {}, CompilationProblem {}] -> True; _ -> False)
  , ( "reload-source-error"
    , [ Right $ SetPackageDB DefaultDB, Right $ AddPackages [testRoot </> "empty"]
      , Left $ appendFile (testRoot </> "empty" </> "A.hs") "\n\naa = 3 + ()"
      , Right $ ReLoad [] [testRoot </> "empty" </> "A.hs"] []
      , Left $ writeFile (testRoot </> "empty" </> "A.hs") "module A where"]
    , \case [LoadingModules {}, LoadedModule {}, LoadingModules {}, CompilationProblem {}] -> True; _ -> False)
  , ( "warning"
    , [ Right $ SetPackageDB DefaultDB, Right $ AddPackages [testRoot </> "warning"] ]
    , \case [LoadingModules{}, LoadedModule {}, CompilationProblem [Marker _ Warning _] []] -> True; _ -> False)
  , ( "no-such-file"
    , [ Right $ SetPackageDB DefaultDB
      , Right $ PerformRefactoring "RenameDefinition" (testRoot </> "simple-refactor" ++ testSuffix </> "A.hs") "3:1-3:2" ["y"] False False]
    , \case [ ErrorMessage _ ] -> True; _ -> False )
  ] ++ if isSource then
         [ ( "cabal-sandbox-defaulted"
           , [ Left $ withCurrentDirectory (testRoot </> "cabal-sandbox") initCabalSandbox
             , Right $ SetPackageDB DefaultDB
             , Right $ AddPackages [testRoot </> "cabal-sandbox"] ]
           , \case [ErrorMessage{}] -> True; _ -> False )
         , ( "package-db-conflict"
           , [ Left $ withCurrentDirectory (testRoot </> "cabal-sandbox") initCabalSandbox
             , Right $ AddPackages [testRoot </> "cabal-sandbox", testRoot </> "has-cabal"] ]
           , \case [ErrorMessage{}] -> True; _ -> False )
         , ( "package-db-conflict-2"
           , [ Left $ withCurrentDirectory (testRoot </> "cabal-sandbox") initCabalSandbox
             , Right $ AddPackages [testRoot </> "cabal-sandbox"]
             , Right $ AddPackages [testRoot </> "has-cabal"] ]
           , \case [LoadingModules{}, LoadedModule{}, ErrorMessage{}] -> True; _ -> False )
         ] else []

refactorTests :: FilePath -> [(String, FilePath, [ClientMessage], [ResponseMsg] -> Bool)]
refactorTests testRoot =
  [ ( "simple-refactor", testRoot </> "simple-refactor"
    , [ AddPackages [ testRoot </> "simple-refactor" ++ testSuffix ]
      , PerformRefactoring "RenameDefinition" "A" "3:1-3:2" ["y"] False False
      ]
    , \case [ LoadingModules{}, LoadedModule aPath _, LoadingModules{}, LoadedModule aPath' _ ]
              -> aPath == testRoot </> "simple-refactor" ++ testSuffix </> "A.hs" && aPath == aPath'; _ -> False )
  , ( "refactor-file", testRoot </> "simple-refactor"
    , [ AddPackages [ testRoot </> "simple-refactor" ++ testSuffix ]
      , PerformRefactoring "RenameDefinition" "A.hs" "3:1-3:2" ["y"] False False
      ]
    , \case [ LoadingModules{}, LoadedModule aPath _, LoadingModules{}, LoadedModule aPath' _]
              -> aPath == testRoot </> "simple-refactor" ++ testSuffix </> "A.hs" && aPath == aPath'; _ -> False )
  , ( "refactor-with-th", testRoot </> "th-imports-normal"
      , [ AddPackages [ testRoot </> "th-imports-normal" ++ testSuffix ]
        , PerformRefactoring "RenameDefinition" "A" "3:6" ["A'"] False False
        ]
      , \case [ LoadingModules{}, LoadedModule a _, LoadedModule c _, LoadedModule b _
                , LoadingModules{}, LoadedModule a' _, LoadedModule c' _, LoadedModule b' _
                ] -> [a,b,c] == map ((testRoot </> "th-imports-normal" ++ testSuffix) </>) ["A.hs","B.hs","C.hs"] && [a',b',c'] == [a,b,c]
              _ -> False )
  , ( "dry-refactor", testRoot </> "reloading"
    , [ AddPackages [ testRoot </> "reloading" ++ testSuffix ]
      , PerformRefactoring "RenameDefinition" "C" "3:1-3:2" ["cc"] False True
      ]
    , \case [ LoadingModules{}, LoadedModule{}, LoadedModule{}, LoadedModule{}
              , DiffInfo diff'
              ] -> let [cFile,bFile] = map ((testRoot </> "reloading" ++ testSuffix) </>) ["C.hs","B.hs"]
                       diff = "--- " ++ cFile ++ "\n+++ " ++ cFile ++ "\n@@\n module C where\r\n \r\n-c = ()\n+cc = ()\n--- "
                                ++ bFile ++ "\n+++ " ++ bFile ++ "\n@@\n module B where\r\n \r\n import C\r\n \r\n-b = c\n+b = cc\n"
                    in diff' == diff; _ -> False )
  , ( "hs-boots", testRoot </> "hs-boots"
    , [ AddPackages [ testRoot </> "hs-boots" ++ testSuffix ]
      , PerformRefactoring "RenameDefinition" "A" "5:1-5:2" ["aa"] False False
      ]
    , \case [ LoadingModules{}, LoadedModule{}, LoadedModule{}, LoadedModule{}, LoadedModule{}
              , LoadingModules{}, LoadedModule path1 _, LoadedModule path2 _
              , LoadedModule path3 _, LoadedModule path4 _
              ] -> let allPathes = map ((testRoot </> "hs-boots" ++ testSuffix) </>) ["A.hs","B.hs","A.hs-boot","B.hs-boot"]
                    in sort [path1,path2,path3,path4] == sort allPathes
            _ -> False )
  , ( "refactor-boot", testRoot </> "hs-boots"
    , [ AddPackages [ testRoot </> "hs-boots" ++ testSuffix ]
      , PerformRefactoring "RenameDefinition" (testRoot </> "hs-boots" ++ testSuffix </> "A.hs-boot") "3:1-3:2" ["aa"] False False
      ]
    , \case [ LoadingModules{}, LoadedModule{}, LoadedModule{}, LoadedModule{}, LoadedModule{}
              , LoadingModules{}, LoadedModule path1 _, LoadedModule path2 _
              , LoadedModule path3 _, LoadedModule path4 _
              ] -> let allPathes = map ((testRoot </> "hs-boots" ++ testSuffix) </>) ["A.hs","B.hs","A.hs-boot","B.hs-boot"]
                    in sort [path1,path2,path3,path4] == sort allPathes
            _ -> False )
  , ( "remove-module", testRoot </> "simple-refactor"
    , [ AddPackages [ testRoot </> "simple-refactor" ++ testSuffix ]
      , PerformRefactoring "RenameDefinition" "A" "1:8-1:9" ["AA"] False False
      ]
    , \case [ LoadingModules{}, LoadedModule aPath _, LoadingModules{}, LoadedModule aaPath _]
              -> aPath == testRoot </> "simple-refactor" ++ testSuffix </> "A.hs"
                   && aaPath == testRoot </> "simple-refactor" ++ testSuffix </> "AA.hs"
            _ -> False )
  ]

reloadingTests :: FilePath -> [(String, FilePath, [ClientMessage], IO (), [ClientMessage], [ResponseMsg] -> Bool)]
reloadingTests testRoot =
  [ ( "reloading-module", testRoot </> "reloading", [ AddPackages [ testRoot </> "reloading" ++ testSuffix ]]
    , writeFile (testRoot </> "reloading" ++ testSuffix </> "C.hs") "module C where\nc = ()"
    , [ ReLoad [] [testRoot </> "reloading" ++ testSuffix </> "C.hs"] []
      , PerformRefactoring "RenameDefinition" "C" "2:1-2:2" ["d"] False False
      ]
    , \case [ LoadingModules{}, LoadedModule pathC'' _, LoadedModule pathB'' _, LoadedModule pathA'' _
              , LoadingModules{}, LoadedModule pathC _, LoadedModule pathB _, LoadedModule pathA _
              , LoadingModules{},LoadedModule pathC' _, LoadedModule pathB' _, LoadedModule pathA' _
              ] -> let allPathes = map ((testRoot </> "reloading" ++ testSuffix) </>) ["C.hs","B.hs","A.hs"]
                    in [pathC,pathB,pathA] == allPathes
                         && [pathC',pathB',pathA'] == allPathes
                         && [pathC'',pathB'',pathA''] == allPathes
            _ -> False )
  , ( "reloading-only-one-module", testRoot </> "reloading"
    , [ AddPackages [ testRoot </> "reloading" ++ testSuffix ]] , return ()
    , [ ReLoad [] [testRoot </> "reloading" ++ testSuffix </> "A.hs"] [] ]
    , \case [ LoadingModules{}, LoadedModule pathC _, LoadedModule pathB _, LoadedModule pathA _
              , LoadingModules{}, LoadedModule pathA' _
              ] -> [pathC,pathB,pathA] == map ((testRoot </> "reloading" ++ testSuffix) </>) ["C.hs","B.hs","A.hs"]
                     && pathA' == pathA
            _ -> False )
  , ( "reloading-package", testRoot </> "changing-cabal"
    , [ AddPackages [ testRoot </> "changing-cabal" ++ testSuffix ]]
    , appendFile (testRoot </> "changing-cabal" ++ testSuffix </> "some-test-package.cabal") ", B"
    , [ AddPackages [testRoot </> "changing-cabal" ++ testSuffix]
      , PerformRefactoring "RenameDefinition" "A" "3:1-3:2" ["z"] False False
      ]
    , \case [ LoadingModules{}, LoadedModule pathA _, LoadingModules{}, LoadedModule pathA' _
              , LoadedModule pathB' _
              , LoadingModules{}, LoadedModule pathA'' _, LoadedModule pathB'' _
              ] -> let [pA,pB] = map ((testRoot </> "changing-cabal" ++ testSuffix) </>) ["A.hs","B.hs"]
                    in pA == pathA && pA == pathA' && pA == pathA'' && pB == pathB' && pB == pathB''
            _ -> False )
  , ( "reloading-th", testRoot </> "has-th", [], return ()
    , [ AddPackages [testRoot </> "has-th" ++ testSuffix]
      , ReLoad [] [testRoot </> "has-th" ++ testSuffix </> "TH.hs"] []
      ]
    , \case [ LoadingModules{}, LoadedModule th _, LoadedModule a _
              , LoadingModules{}, LoadedModule th' _, LoadedModule a' _
              ] -> let [pth,pa] = map ((testRoot </> "has-th" ++ testSuffix) </>) ["TH.hs","A.hs"]
                    in pa == a && pa == a' && pth == th && pth == th'
            _ -> False )
  -- TODO: This test runs correctly in itself. Probably a testing bug?
  -- , ( "reset-th", testRoot </> "has-th", [], return ()
  --   , [ AddPackages [testRoot </> "has-th" ++ testSuffix]
  --     , Reset
  --     ]
  --   , \case [ LoadingModules{}, LoadedModule th _, LoadedModule a _
  --             , LoadingModules{}, LoadedModule th' _, LoadedModule a' _
  --             ] -> let [pth,pa] = map ((testRoot </> "has-th" ++ testSuffix) </>) ["TH.hs","A.hs"]
  --                   in pa == a && pa == a' && pth == th && pth == th'
  --           _ -> False )
  , ( "reloading-ghc", testRoot </> "has-ghc", [], return ()
    , [ AddPackages [testRoot </> "has-ghc" ++ testSuffix]
      , ReLoad [] [testRoot </> "has-ghc" ++ testSuffix </> "A.hs"] []
      ]
    , \case [ LoadingModules{}, LoadedModule a _
              , LoadingModules{}, LoadedModule a' _
              ] -> a == testRoot </> "has-ghc" ++ testSuffix </> "A.hs" && a' == a
            _ -> False )
  , ( "reloading-unloadable-project", testRoot </> "load-error"
    , [ AddPackages [testRoot </> "load-error" ++ testSuffix] ]
    , writeFile (testRoot </> "load-error" ++ testSuffix </> "A.hs") "module A where\n\na = ()"
    , [ ReLoad [] [testRoot </> "load-error" ++ testSuffix </> "A.hs"] [] ]
    , \case [ LoadingModules{}, CompilationProblem{}
              , LoadingModules{}, LoadedModule a _
              ] -> a == testRoot </> "load-error" ++ testSuffix </> "A.hs"
            _ -> False )
  , ( "reloading-unloadable-project-multi-modules", testRoot </> "load-error-multi"
    , [ AddPackages [testRoot </> "load-error-multi" ++ testSuffix] ]
    , writeFile (testRoot </> "load-error-multi" ++ testSuffix </> "B.hs") "module B where\n\nimport A\n\nb = a"
    , [ ReLoad [] [testRoot </> "load-error-multi" ++ testSuffix </> "B.hs"] [] ]
    , \case [ LoadingModules{}, LoadedModule a _, LoadedModule d _, CompilationProblem{}
              , LoadingModules{}, LoadedModule b _, LoadedModule c _
              ] -> [a,b,c,d] == map ((testRoot </> "load-error-multi" ++ testSuffix) </>) ["A.hs", "B.hs","C.hs","D.hs"]
            _ -> False )
  , ( "change-cabal", testRoot </> "two-modules", [], return ()
    , [ AddPackages [testRoot </> "two-modules" ++ testSuffix]
      , ReLoad [] [testRoot </> "two-modules" ++ testSuffix </> "some-test-package.cabal"] []
      ]
    , \case [ LoadingModules{}, LoadedModule a _, LoadedModule b _
              , LoadingModules{}, LoadedModule a' _, LoadedModule b' _
              ] -> [a,b] == map ((testRoot </> "two-modules" ++ testSuffix) </>) ["A.hs","B.hs"] && [a',b'] == [a,b]
            _ -> False )
  , ( "change-incomplete-cabal", testRoot </> "incomplete-cabal"
    , [ AddPackages [testRoot </> "incomplete-cabal" ++ testSuffix] ]
    , appendFile (testRoot </> "incomplete-cabal" ++ testSuffix </> "some-test-package.cabal") ", B"
    , [ ReLoad [] [testRoot </> "incomplete-cabal" ++ testSuffix </> "some-test-package.cabal"] [] ]
    , \case [ LoadingModules{}, LoadedModule a _
              , LoadingModules{}, LoadedModule a' _, LoadedModule b' _
              ] -> [a',b'] == map ((testRoot </> "incomplete-cabal" ++ testSuffix) </>) ["A.hs","B.hs"] && a == a'
            _ -> False )
  , ( "adding-module", testRoot </> "reloading", [AddPackages [ testRoot </> "reloading" ++ testSuffix ]]
    , writeFile (testRoot </> "reloading" ++ testSuffix </> "D.hs") "module D where\nd = ()"
    , [ ReLoad [testRoot </> "reloading" ++ testSuffix </> "D.hs"] [] [] ]
    , \case [ LoadingModules {}, LoadedModule {}, LoadedModule {}, LoadedModule {}, LoadingModules {}
              , LoadingModules {}, LoadedModule {}, LoadedModule {}, LoadedModule {}, LoadedModule {}] -> True
            _ -> False )
  , ( "reloading-remove", testRoot </> "reloading", [ AddPackages [ testRoot </> "reloading" ++ testSuffix ]]
    , do removeFile (testRoot </> "reloading" ++ testSuffix </> "A.hs")
         removeFile (testRoot </> "reloading" ++ testSuffix </> "B.hs")
    , [ ReLoad [] [testRoot </> "reloading" ++ testSuffix </> "C.hs"]
                  [testRoot </> "reloading" ++ testSuffix </> "A.hs", testRoot </> "reloading" ++ testSuffix </> "B.hs"]
      , PerformRefactoring "RenameDefinition" "C" "3:1-3:2" ["d"] False False
      ]
    , \case [ LoadingModules{}, LoadedModule pathC _, LoadedModule pathB _, LoadedModule pathA _
              , LoadingModules{}, LoadedModule pathC' _, LoadingModules{}, LoadedModule pathC'' _ ]
              -> let [pC,pB,pA] = map ((testRoot </> "reloading" ++ testSuffix) </>) ["C.hs","B.hs","A.hs"]
                  in pA == pathA && pB == pathB && pC == pathC && pC == pathC' && pC == pathC''
            _ -> False )
  , ( "remove-package", testRoot </> "multi-packages-dependent"
    , [ AddPackages [ testRoot </> "multi-packages-dependent" ++ testSuffix </> "package1"
                    , testRoot </> "multi-packages-dependent" ++ testSuffix </> "package2" ]]
    , removeDirectoryRecursive (testRoot </> "multi-packages-dependent" ++ testSuffix </> "package2")
    , [ RemovePackages [testRoot </> "multi-packages-dependent" ++ testSuffix </> "package2"]
      , PerformRefactoring "RenameDefinition" "A"
                                              "3:1-3:2" ["d"] False False
      ]
    , \case [ LoadingModules{}, LoadedModule pathA' _, LoadedModule pathB' _
              , LoadingModules{}, LoadedModule pathA _ ]
              -> let [pA,pB] = map ((testRoot </> "multi-packages-dependent" ++ testSuffix) </>) [ "package1" </> "A.hs", "package2" </> "B.hs"]
                  in pA == pathA && pA == pathA' && pB == pathB'
            _ -> False )
  ]

undoTests :: FilePath -> [(String, FilePath, [Either (IO ()) ClientMessage], [ResponseMsg] -> Bool)]
undoTests testRoot
  = [ ( "nothing-to-undo", testRoot </> "simple-refactor"
      , [ Right $ AddPackages [ testRoot </> "simple-refactor" ++ testSuffix ]
        , Right UndoLast
        ] , \case [ LoadingModules{}, LoadedModule{}, ErrorMessage{}] -> True; _ -> False )
    , ( "simple-undo", testRoot </> "simple-refactor"
      , [ Right $ AddPackages [ testRoot </> "simple-refactor" ++ testSuffix ]
        , Right $ PerformRefactoring "RenameDefinition" "A" "3:1-3:2" ["y"] False False
        , Left $ do -- if the test does not succeed, insert a delay here
                    res <- readFile (testRoot </> "simple-refactor" ++ testSuffix </> "A.hs")
                    when (filter (`notElem` ['\r','\n']) res /= "module A wherey = ()")
                      $ assertFailure ("Module content after refactoring is not the expected:\n" ++ res)
        , Right UndoLast
        , Left $ do -- if the test does not succeed, insert a delay here
                    res <- readFile (testRoot </> "simple-refactor" ++ testSuffix </> "A.hs")
                    when (filter (`notElem` ['\r','\n']) res /= "module A wherex = ()")
                      $ assertFailure ("Module content after undoing is not the expected:\n" ++ res)
        ]
      , \case [ LoadingModules{}, LoadedModule{}, LoadingModules{}, LoadedModule{}, LoadingModules{}, LoadedModule{}]
                -> True; _ -> False )
    , ( "undo-invalidated", testRoot </> "simple-refactor"
      , [ Right $ AddPackages [ testRoot </> "simple-refactor" ++ testSuffix ]
        , Right $ PerformRefactoring "RenameDefinition" "A" "3:1-3:2" ["y"] False False
        , Right (ReLoad [] [testRoot </> "simple-refactor" ++ testSuffix </> "A.hs"] [])
        , Right UndoLast
        ]
      , \case [ LoadingModules{}, LoadedModule{}, LoadingModules{}, LoadedModule{}
                , LoadingModules{}, LoadedModule{}, ErrorMessage{} ] -> True; _ -> False )
    , ( "undo-invalidated-other-module", testRoot </> "two-modules"
      , [ Right $ AddPackages [ testRoot </> "two-modules" ++ testSuffix ]
        , Right $ PerformRefactoring "RenameDefinition" "A" "3:1-3:2" ["y"] False False
        , Right (ReLoad [] [testRoot </> "two-modules" ++ testSuffix </> "B.hs"] [])
        , Right UndoLast
        ]
      , \case [ LoadingModules{}, LoadedModule{}, LoadedModule{}, LoadingModules{}, LoadedModule{}
                , LoadingModules{}, LoadedModule{}, ErrorMessage{} ] -> True; _ -> False )
    , ( "undo-invalidated-revalidate", testRoot </> "simple-refactor"
      , [ Right $ AddPackages [ testRoot </> "simple-refactor" ++ testSuffix ]
        , Right $ PerformRefactoring "RenameDefinition" "A" "3:1-3:2" ["y"] False False
        , Right (ReLoad [] [testRoot </> "simple-refactor" ++ testSuffix </> "A.hs"] [])
        , Right $ PerformRefactoring "RenameDefinition" "A" "3:1-3:2" ["x"] False False
        , Right UndoLast
        ]
      , \case [ LoadingModules{}, LoadedModule{}, LoadingModules{}, LoadedModule{}
                , LoadingModules{}, LoadedModule{}, LoadingModules{}, LoadedModule{}
                , LoadingModules{}, LoadedModule{} ] -> True; _ -> False )
    , ( "multi-module-undo", testRoot </> "reloading"
        , [ Right $ AddPackages [ testRoot </> "reloading" ++ testSuffix ]
          , Right $ PerformRefactoring "RenameDefinition" "C" "3:1-3:2" ["d"] False False
          , Right UndoLast
          , Left $ do -- if the test does not succeed, insert a delay here
                      resC <- readFile (testRoot </> "reloading" ++ testSuffix </> "C.hs")
                      resB <- readFile (testRoot </> "reloading" ++ testSuffix </> "B.hs")
                      when (filter (`notElem` ['\r','\n']) resC /= "module C wherec = ()")
                        $ assertFailure ("C.hs content after undoing is not the expected:\n" ++ resC)
                      when (filter (`notElem` ['\r','\n']) resB /= "module B whereimport Cb = c")
                        $ assertFailure ("B.hs content after undoing is not the expected:\n" ++ resC)
          ]
        , \case [ LoadingModules{}, LoadedModule{}, LoadedModule{}, LoadedModule{}
                  , LoadingModules{}, LoadedModule{}, LoadedModule{}, LoadedModule{}
                  , LoadingModules{}, LoadedModule{}, LoadedModule{}, LoadedModule{} ]
                  -> True; _ -> False )
    , ( "multiple-undo", testRoot </> "simple-refactor"
      , [ Right $ AddPackages [ testRoot </> "simple-refactor" ++ testSuffix ]
        , Right $ PerformRefactoring "RenameDefinition" "A" "3:1-3:2" ["y"] False False
        , Right $ PerformRefactoring "RenameDefinition" "A" "3:1-3:2" ["z"] False False
        , Right UndoLast
        , Right UndoLast
        , Left $ do -- if the test does not succeed, insert a delay here
                    res <- readFile (testRoot </> "simple-refactor" ++ testSuffix </> "A.hs")
                    when (filter (`notElem` ['\r','\n']) res /= "module A wherex = ()")
                      $ assertFailure ("Module content after undoing is not the expected:\n" ++ res)
        ]
      , \case [ LoadingModules{}, LoadedModule{} -- initial load
                , LoadingModules{}, LoadedModule{} -- re-load after first refactor
                , LoadingModules{}, LoadedModule{} -- re-load after second refactor
                , LoadingModules{}, LoadedModule{} -- re-load after first undo
                , LoadingModules{}, LoadedModule{} -- re-load after second undo
                ]
                -> True; _ -> False )
    ]

pkgDbTests :: FilePath -> [(String, IO (), [ClientMessage], [ResponseMsg])]
pkgDbTests testRoot
  = [ ( "cabal-sandbox"
      , withCurrentDirectory (testRoot </> "cabal-sandbox") initCabalSandbox
      , [ SetPackageDB CabalSandboxDB, AddPackages [testRoot </> "cabal-sandbox"] ]
      , [ LoadingModules [testRoot </> "cabal-sandbox" </> "UseGroups.hs"]
        , LoadedModule (testRoot </> "cabal-sandbox" </> "UseGroups.hs") "UseGroups"] )
    , ( "cabal-sandbox-auto"
      , withCurrentDirectory (testRoot </> "cabal-sandbox") initCabalSandbox
      , [ AddPackages [testRoot </> "cabal-sandbox"] ]
      , [ LoadingModules [testRoot </> "cabal-sandbox" </> "UseGroups.hs"]
        , LoadedModule (testRoot </> "cabal-sandbox" </> "UseGroups.hs") "UseGroups"] )
    , ( "pkg-db-reload"
      , withCurrentDirectory (testRoot </> "cabal-sandbox") initCabalSandbox
      , [ AddPackages [testRoot </> "cabal-sandbox"]
        , ReLoad [] [testRoot </> "cabal-sandbox" </> "UseGroups.hs"] []]
      , [ LoadingModules [testRoot </> "cabal-sandbox" </> "UseGroups.hs"]
        , LoadedModule (testRoot </> "cabal-sandbox" </> "UseGroups.hs") "UseGroups"
        , LoadingModules [testRoot </> "cabal-sandbox" </> "UseGroups.hs"]
        , LoadedModule (testRoot </> "cabal-sandbox" </> "UseGroups.hs") "UseGroups" ])
    ]

initCabalSandbox = do
  sandboxExists <- doesDirectoryExist ".cabal-sandbox"
  when sandboxExists $ tryToExecute "cabal" ["sandbox", "delete"]
  execute "cabal" ["sandbox", "init"]
  withCurrentDirectory ("groups-0.4.0.0") $ do
    execute "cabal" ["sandbox", "init", "--sandbox", ".." </> ".cabal-sandbox"]
    execute "cabal" ["install"]

specialTests :: FilePath -> [(String, FilePath, Int -> Maybe FilePath -> DaemonOptions, [ClientMessage], [ResponseMsg] -> Bool)]
specialTests testRoot =
  [ ( "force-code-gen-with-th", testRoot </> "th-imports-normal", codeGenOpts
      , [ AddPackages [ testRoot </> "th-imports-normal" ++ testSuffix ]
        , PerformRefactoring "RenameDefinition" "A" "3:6" ["A'"] False False
        ]
      , \case [ LoadingModules{}, LoadedModule a _, LoadedModule c _, LoadedModule b _
                , LoadingModules{}, LoadedModule a' _, LoadedModule c' _, LoadedModule b' _
                ] -> [a,b,c] == map ((testRoot </> "th-imports-normal" ++ testSuffix) </>) ["A.hs","B.hs","C.hs"] && [a',b',c'] == [a,b,c]
              _ -> False )
  ]
  where codeGenOpts p fs = addCodeGen (daemonOpts p fs)
        addCodeGen opts = opts { sharedOptions = (sharedOptions opts) { generateCode = True } }

-------------------------------------------------------
-- * Helper functions for test code -------------------
-------------------------------------------------------

sourceRoot = ".." </> ".." </> "src"

execute :: String -> [String] -> IO ()
execute cmd args
  = do let command = (cmd ++ concat (map (" " ++) args))
       (_, Just stdOut, Just stdErr, handle) <- createProcess ((shell command) { std_out = CreatePipe, std_err = CreatePipe })
       exitCode <- waitForProcess handle
       when (exitCode /= ExitSuccess) $ do
         output <- hGetContents stdOut
         errors <- hGetContents stdErr
         error ("Command exited with nonzero: " ++ command ++ " output:\n" ++ output ++ "\nerrors:\n" ++ errors)

tryToExecute :: String -> [String] -> IO ()
tryToExecute cmd args
  = do let command = (cmd ++ concat (map (" " ++) args))
       (_, _, _, handle) <- createProcess ((shell command) { std_out = NoStream, std_err = NoStream })
       void $ waitForProcess handle

makeDaemonTest :: MVar Int -> (String, [ClientMessage], [ResponseMsg]) -> TestTree
makeDaemonTest port (label, input, expected) = testCase label $ restartOnNetworkError $ do
    actual <- communicateWithDaemon False port daemonOpts (map Right (SetPackageDB DefaultDB : input))
    assertEqual "" expected actual

makeComplexLoadTest :: MVar Int -> (String, FilePath, [ClientMessage], [ResponseMsg] -> Bool) -> TestTree
makeComplexLoadTest port (label, dir, inputs, validator) = testCase label $ restartOnNetworkError $ do
    exists <- doesDirectoryExist (dir ++ testSuffix)
    -- clear the target directory from possible earlier test runs
    when exists $ removeDirectoryRecursive (dir ++ testSuffix)
    copyDir dir (dir ++ testSuffix)
    actual <- communicateWithDaemon False port daemonOpts (map Right (SetPackageDB DefaultDB : inputs))
    assertBool ("The responses are not the expected: " ++ show actual) (validator actual)
  `finally` removeDirectoryRecursive (dir ++ testSuffix)

makeRefactorTest :: MVar Int -> (String, FilePath, [ClientMessage], [ResponseMsg] -> Bool) -> TestTree
makeRefactorTest port (label, dir, input, validator) = testCase label $ restartOnNetworkError $ do
    exists <- doesDirectoryExist (dir ++ testSuffix)
    -- clear the target directory from possible earlier test runs
    when exists $ removeDirectoryRecursive (dir ++ testSuffix)
    copyDir dir (dir ++ testSuffix)
    actual <- communicateWithDaemon False port daemonOpts (map Right (SetPackageDB DefaultDB : input))
    assertBool ("The responses are not the expected: " ++ show actual) (validator actual)
  `finally` removeDirectoryRecursive (dir ++ testSuffix)

makeReloadTest :: MVar Int -> (String, FilePath, [ClientMessage], IO (), [ClientMessage], [ResponseMsg] -> Bool) -> TestTree
makeReloadTest port (label, dir, input1, io, input2, validator) = testCase label $ restartOnNetworkError $ do
    exists <- doesDirectoryExist (dir ++ testSuffix)
    -- clear the target directory from possible earlier test runs
    when exists $ removeDirectoryRecursive (dir ++ testSuffix)
    copyDir dir (dir ++ testSuffix)
    actual <- communicateWithDaemon False port daemonOpts (map Right (SetPackageDB DefaultDB : input1) ++ [Left io] ++ map Right input2)
    assertBool ("The responses are not the expected: " ++ show actual) (validator actual)
  `finally` removeDirectoryRecursive (dir ++ testSuffix)

makeUndoTest :: MVar Int -> (String, FilePath, [Either (IO ()) ClientMessage], [ResponseMsg] -> Bool) -> TestTree
makeUndoTest port (label, dir, inputs, validator) = testCase label $ restartOnNetworkError $ do
    exists <- doesDirectoryExist (dir ++ testSuffix)
    -- clear the target directory from possible earlier test runs
    when exists $ removeDirectoryRecursive (dir ++ testSuffix)
    copyDir dir (dir ++ testSuffix)
    actual <- communicateWithDaemon False port daemonOpts (Right (SetPackageDB DefaultDB) : inputs)
    assertBool ("The responses are not the expected: " ++ show actual) (validator actual)
  `finally` removeDirectoryRecursive (dir ++ testSuffix)

makePkgDbTest :: MVar Int -> (String, IO (), [ClientMessage], [ResponseMsg]) -> TestTree
makePkgDbTest port (label, prepare, inputs, expected)
  = testCase label $ restartOnNetworkError $ do
      actual <- communicateWithDaemon False port daemonOpts ([Left prepare] ++ map Right inputs)
      assertEqual "" expected actual

makeCompProblemTest :: MVar Int -> (String, [Either (IO ()) ClientMessage], [ResponseMsg] -> Bool) -> TestTree
makeCompProblemTest port (label, actions, validator) = testCase label $ restartOnNetworkError $ do
  actual <- communicateWithDaemon False port daemonOpts actions
  assertBool ("The responses are not the expected: " ++ show actual) (validator actual)

makeSpecialTest :: MVar Int -> (String, FilePath, Int -> Maybe FilePath -> DaemonOptions, [ClientMessage], [ResponseMsg] -> Bool) -> TestTree
makeSpecialTest port (label, dir, opts, input, validator) = testCase label $ restartOnNetworkError $ do
    exists <- doesDirectoryExist (dir ++ testSuffix)
    -- clear the target directory from possible earlier test runs
    when exists $ removeDirectoryRecursive (dir ++ testSuffix)
    copyDir dir (dir ++ testSuffix)
    actual <- communicateWithDaemon False port opts (map Right (SetPackageDB DefaultDB : input))
    assertBool ("The responses are not the expected: " ++ show actual) (validator actual)
  `finally` removeDirectoryRecursive (dir ++ testSuffix)

communicateWithDaemon :: Bool -> MVar Int -> (Int -> Maybe FilePath -> DaemonOptions) -> [Either (IO ()) ClientMessage] -> IO [ResponseMsg]
communicateWithDaemon watch port opts msgs = withSocketsDo $ do
    portNum <- retryConnect port
    addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just (show portNum))
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    waitToConnect sock (addrAddress serverAddr)
    intermedRes <- sequence (map (either (\io -> do sendAll sock (encode KeepAlive)
                                                    r <- readSockResponsesUntil sock KeepAliveResponse BS.empty
                                                    io
                                                    return r)
                                 ((>> return []) . sendAll sock . (`BS.snoc` '\n') . encode)) msgs)
    sendAll sock (encode Disconnect)
      `catchIOError` \e -> hPutStrLn stderr $ "Cannot send Disconnect: " ++ show e
    resps <- readSockResponsesUntil sock Disconnected BS.empty
    close sock
    return (concat intermedRes ++ resps)
  where waitToConnect sock addr
          = connect sock addr `catch` \(_ :: SomeException) -> threadDelay 10000 >> waitToConnect sock addr
        retryConnect port = do portNum <- readMVar port
                               watchDir <- (++) <$> glob watchPath <*> glob linuxWatchPath
                               case (watch, watchDir) of
                                 (True, []) -> error "The watch executable is not found."
                                 (True, w:_) -> forkIO $ runDaemon' builtinRefactorings (opts portNum (Just w))
                                 (False, _) -> forkIO $ runDaemon' builtinRefactorings (opts portNum Nothing)
                               return portNum
          `catch` \(e :: SomeException) -> do putStrLn ("exception caught: `" ++ show e ++ "` trying with a new port")
                                              modifyMVar_ port (\i -> if i < pORT_NUM_END
                                                                        then return (i+1)
                                                                        else error "The port number reached the maximum")
                                              retryConnect port

restartOnNetworkError = tryNTimes 5

tryNTimes :: Int -> IO () -> IO ()
tryNTimes 0 action = action
tryNTimes n action = action `catch` \e -> if "Network.Socket.sendBuf: failed" `isInfixOf` show e
                                            then do putStrLn (show e)
                                                    threadDelay 1000000
                                                    tryNTimes (n-1) action
                                            else throwIO (e :: SomeException)


daemonOpts :: Int -> Maybe FilePath -> DaemonOptions
daemonOpts n we = DaemonOptions { daemonVersion = False
                                , portNumber = n
                                , silentMode = True
                                , sharedOptions = SharedDaemonOptions { noWatch = isNothing we
                                                                      , watchExe = we
                                                                      , generateCode = False
                                                                      , disableHistory = False
                                                                      , Options.ghcFlags = Nothing
                                                                      , projectType = Nothing
                                                                      }
                                }

-- this must be changed once watch is in stackage
watchPath = "../../.stack-work/downloaded/*/watch-master/.stack-work/dist/*/build/watch"
linuxWatchPath = "../../.stack-work/downloaded/*/watch-master/.stack-work/dist/*/*/build/watch"

readSockResponsesUntil :: Socket -> ResponseMsg -> BS.ByteString -> IO [ResponseMsg]
readSockResponsesUntil sock rsp bs
  = do resp <- recv sock 2048
       let fullBS = bs `BS.append` resp
       if BS.null resp
         then return []
         else
           let splitted = BS.split '\n' fullBS
               recognized = catMaybes $ map decode splitted
            in if rsp `elem` recognized
                 then return $ takeWhile (/= rsp) recognized
                 else readSockResponsesUntil sock rsp fullBS

testRoot = "examples" </> "Project"

deriving instance Eq UndoRefactor
deriving instance Eq ResponseMsg
instance FromJSON UndoRefactor
instance FromJSON ResponseMsg
instance FromJSON Marker
instance FromJSON Severity
instance ToJSON ClientMessage
instance ToJSON PackageDB

copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  createDirectory dst
  content <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) content
  forM_ xs $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyDir srcPath dstPath
      else copyFile srcPath dstPath

commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []

longestCommonPrefix :: (Eq a) => [[a]] -> [a]
longestCommonPrefix = foldl1 commonPrefix

instance FromJSON SrcSpan where
    parseJSON (Object v) = mkSrcSpanReal <$> v .: "file"
                                         <*> v .: "startRow"
                                         <*> v .: "startCol"
                                         <*> v .: "endRow"
                                         <*> v .: "endCol"
    parseJSON _          = fail "not an object"


mkSrcSpanReal :: String -> Int -> Int -> Int -> Int -> SrcSpan
mkSrcSpanReal file startRow startCol endRow endCol
  = mkSrcSpan (mkSrcLoc (mkFastString file) startRow startCol)
              (mkSrcLoc (mkFastString file) endRow endCol)
