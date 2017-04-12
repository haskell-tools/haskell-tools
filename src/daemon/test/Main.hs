{-# LANGUAGE StandaloneDeriving, LambdaCase, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import System.Exit
import System.Directory
import System.FilePath
import System.Process
import System.Environment
import System.Exit
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Network.Socket hiding (KeepAlive, send, recv)
import Network.Socket.ByteString.Lazy as Sock
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List as List
import Data.List (sort)
import Data.Aeson
import Data.Maybe
import System.IO

import SrcLoc
import FastString

import Language.Haskell.Tools.Refactor.Daemon
import Language.Haskell.Tools.Refactor.Daemon.PackageDB

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
  = localOption (mkTimeout ({- 10s -} 1000 * 1000 * 20))
      $ testGroup "daemon-tests"
          [ testGroup "simple-tests"
              $ map (makeDaemonTest portCounter) simpleTests
          , testGroup "loading-tests"
              $ map (makeDaemonTest portCounter) loadingTests
          , testGroup "refactor-tests"
              $ map (makeRefactorTest portCounter) (refactorTests testRoot)
          , testGroup "reload-tests"
              $ map (makeReloadTest portCounter) reloadingTests
          , testGroup "compilation-problem-tests"
              $ map (makeCompProblemTest portCounter) compProblemTests
          -- if not a stack build, we cannot guarantee that stack is on the path
          , if isSource
             then testGroup "pkg-db-tests" $ map (makePkgDbTest portCounter) pkgDbTests
             else testCase "IGNORED pkg-db-tests" (return ())
          -- cannot execute this when the source is not present
          , if isSource then selfLoadingTest portCounter else testCase "IGNORED self-load" (return ())
          ]

testSuffix = "_test"

simpleTests :: [(String, [ClientMessage], [ResponseMsg])]
simpleTests =
  [ ( "empty-test", [], [] )
  , ( "keep-alive", [KeepAlive], [KeepAliveResponse] )
  ]

loadingTests :: [(String, [ClientMessage], [ResponseMsg])]
loadingTests =
  [ ( "load-package"
    , [AddPackages [testRoot </> "has-cabal"]]
    , [ LoadingModules [testRoot </> "has-cabal" </> "A.hs"]
      , LoadedModules [(testRoot </> "has-cabal" </> "A.hs", "A")]] )
  , ( "no-cabal"
    , [AddPackages [testRoot </> "no-cabal"]]
    , [ LoadingModules [testRoot </> "no-cabal" </> "A.hs"]
      , LoadedModules [(testRoot </> "no-cabal" </> "A.hs", "A")]] )
  , ( "source-dir"
    , [AddPackages [testRoot </> "source-dir"]]
    , [ LoadingModules [testRoot </> "source-dir" </> "src" </> "A.hs"]
      , LoadedModules [(testRoot </> "source-dir" </> "src" </> "A.hs", "A")]] )
  , ( "source-dir-outside"
    , [AddPackages [testRoot </> "source-dir-outside"]]
    , [ LoadingModules [testRoot </> "source-dir-outside" </> ".." </> "src" </> "A.hs"]
      , LoadedModules [(testRoot </> "source-dir-outside" </> ".." </> "src" </> "A.hs", "A")]] )
  , ( "multi-packages"
    , [ AddPackages [ testRoot </> "multi-packages" </> "package1"
                    , testRoot </> "multi-packages" </> "package2" ]]
    , [ LoadingModules [ testRoot </> "multi-packages" </> "package2" </> "B.hs"
                       , testRoot </> "multi-packages" </> "package1" </> "A.hs" ]
      , LoadedModules [ (testRoot </> "multi-packages" </> "package2" </> "B.hs", "B") ]
      , LoadedModules [ (testRoot </> "multi-packages" </> "package1" </> "A.hs", "A") ] ] )
  , ( "multi-packages-flags"
    , [ AddPackages [ testRoot </> "multi-packages-flags" </> "package1"
                    , testRoot </> "multi-packages-flags" </> "package2" ]]
    , [ LoadingModules [ testRoot </> "multi-packages-flags" </> "package2" </> "B.hs"
                       , testRoot </> "multi-packages-flags" </> "package1" </> "A.hs" ]
      , LoadedModules [ (testRoot </> "multi-packages-flags" </> "package2" </> "B.hs", "B") ]
      , LoadedModules [ (testRoot </> "multi-packages-flags" </> "package1" </> "A.hs", "A") ] ] )
  , ( "multi-packages-dependent"
    , [ AddPackages [ testRoot </> "multi-packages-dependent" </> "package1"
                    , testRoot </> "multi-packages-dependent" </> "package2" ]]
    , [ LoadingModules [ testRoot </> "multi-packages-dependent" </> "package1" </> "A.hs"
                       , testRoot </> "multi-packages-dependent" </> "package2" </> "B.hs" ]
      , LoadedModules [ (testRoot </> "multi-packages-dependent" </> "package1" </> "A.hs", "A") ]
      , LoadedModules [ (testRoot </> "multi-packages-dependent" </> "package2" </> "B.hs", "B") ] ] )
  , ( "has-th"
    , [AddPackages [testRoot </> "has-th"]]
    , [ LoadingModules [ testRoot </> "has-th" </> "TH.hs", testRoot </> "has-th" </> "A.hs" ]
      , LoadedModules [ (testRoot </> "has-th" </> "TH.hs", "TH") ]
      , LoadedModules [ (testRoot </> "has-th" </> "A.hs", "A") ] ] )
  , ( "th-added-later"
    , [ AddPackages [testRoot </> "th-added-later" </> "package1"]
      , AddPackages [testRoot </> "th-added-later" </> "package2"]
      ]
    , [ LoadingModules [ testRoot </> "th-added-later" </> "package1" </> "A.hs" ]
      , LoadedModules [(testRoot </> "th-added-later" </> "package1" </> "A.hs", "A")]
      , LoadingModules [ testRoot </> "th-added-later" </> "package2" </> "B.hs" ]
      , LoadedModules [(testRoot </> "th-added-later" </> "package2" </> "B.hs", "B")] ] )
  ]

compProblemTests :: [(String, [Either (IO ()) ClientMessage], [ResponseMsg] -> Bool)]
compProblemTests =
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
    , \case [LoadingModules {}, LoadedModules {}, LoadingModules {}, CompilationProblem {}] -> True; _ -> False)
  , ( "reload-source-error"
    , [ Right $ SetPackageDB DefaultDB, Right $ AddPackages [testRoot </> "empty"]
      , Left $ appendFile (testRoot </> "empty" </> "A.hs") "\n\naa = 3 + ()"
      , Right $ ReLoad [] [testRoot </> "empty" </> "A.hs"] []
      , Left $ writeFile (testRoot </> "empty" </> "A.hs") "module A where"]
    , \case [LoadingModules {}, LoadedModules {}, LoadingModules {}, CompilationProblem {}] -> True; _ -> False)
  , ( "no-such-file"
    , [ Right $ PerformRefactoring "RenameDefinition" (testRoot </> "simple-refactor" ++ testSuffix </> "A.hs") "3:1-3:2" ["y"] ]
    , \case [ ErrorMessage _ ] -> True; _ -> False )
  , ( "additional-files"
    , [ Right $ SetPackageDB DefaultDB, Right $ AddPackages [testRoot </> "additional-files"] ]
    , \case [ LoadingModules {}, ErrorMessage _ ] -> True; _ -> False )
  ]

sourceRoot = ".." </> ".." </> "src"

selfLoadingTest :: MVar Int -> TestTree
selfLoadingTest port = localOption (mkTimeout ({- 5 min -} 1000 * 1000 * 60 * 5)) $ testCase "self-load" $ do
    actual <- communicateWithDaemon port
                [ Right $ AddPackages (map (sourceRoot </>) ["ast", "backend-ghc", "prettyprint", "rewrite", "refactor", "daemon"]) ]
    assertBool ("The expected result is a nonempty response message list that does not contain errors. Actual result: " ++ show actual)
               (not (null actual) && all (\case ErrorMessage {} -> False; _ -> True) actual)


refactorTests :: FilePath -> [(String, FilePath, [ClientMessage], [ResponseMsg] -> Bool)]
refactorTests testRoot =
  [ ( "simple-refactor", testRoot </> "simple-refactor"
    , [ AddPackages [ testRoot </> "simple-refactor" ++ testSuffix ]
      , PerformRefactoring "RenameDefinition" (testRoot </> "simple-refactor" ++ testSuffix </> "A.hs") "3:1-3:2" ["y"]
      ]
    , \case [ LoadingModules{}, LoadedModules [ (aPath, _) ], ModulesChanged _, LoadingModules{}, LoadedModules [ (aPath', _) ]]
              -> aPath == testRoot </> "simple-refactor" ++ testSuffix </> "A.hs" && aPath == aPath'; _ -> False )
  , ( "hs-boots", testRoot </> "hs-boots"
    , [ AddPackages [ testRoot </> "hs-boots" ++ testSuffix ]
      , PerformRefactoring "RenameDefinition" (testRoot </> "hs-boots" ++ testSuffix </> "A.hs") "5:1-5:2" ["aa"]
      ]
    , \case [ LoadingModules{}, LoadedModules _, LoadedModules _, LoadedModules _, LoadedModules _, ModulesChanged _
              , LoadingModules{}, LoadedModules [ (path1, _) ], LoadedModules [ (path2, _) ]
              , LoadedModules [ (path3, _) ], LoadedModules [ (path4, _) ]
              ] -> let allPathes = map ((testRoot </> "hs-boots" ++ testSuffix) </>) ["A.hs","B.hs","A.hs-boot","B.hs-boot"]
                    in sort [path1,path2,path3,path4] == sort allPathes
            _ -> False )
  , ( "remove-module", testRoot </> "simple-refactor"
    , [ AddPackages [ testRoot </> "simple-refactor" ++ testSuffix ]
      , PerformRefactoring "RenameDefinition" (testRoot </> "simple-refactor" ++ testSuffix </> "A.hs") "1:8-1:9" ["AA"]
      ]
    , \case [ LoadingModules{},LoadedModules [ (aPath, _) ], ModulesChanged _, LoadingModules{},LoadedModules [ (aaPath, _) ]]
              -> aPath == testRoot </> "simple-refactor" ++ testSuffix </> "A.hs"
                   && aaPath == testRoot </> "simple-refactor" ++ testSuffix </> "AA.hs"
            _ -> False )
  ]

reloadingTests :: [(String, FilePath, [ClientMessage], IO (), [ClientMessage], [ResponseMsg] -> Bool)]
reloadingTests =
  [ ( "reloading-module", testRoot </> "reloading", [ AddPackages [ testRoot </> "reloading" ++ testSuffix ]]
    , writeFile (testRoot </> "reloading" ++ testSuffix </> "C.hs") "module C where\nc = ()"
    , [ ReLoad [] [testRoot </> "reloading" ++ testSuffix </> "C.hs"] []
      , PerformRefactoring "RenameDefinition" (testRoot </> "reloading" ++ testSuffix </> "C.hs") "2:1-2:2" ["d"]
      ]
    , \case [ LoadingModules{}, LoadedModules [(pathC'',_)], LoadedModules [(pathB'',_)], LoadedModules [(pathA'',_)]
              , LoadingModules{}, LoadedModules [(pathC,_)], LoadedModules [(pathB,_)], LoadedModules [(pathA,_)]
              , ModulesChanged _, LoadingModules{},LoadedModules [(pathC',_)], LoadedModules [(pathB',_)], LoadedModules [(pathA',_)]
              ] -> let allPathes = map ((testRoot </> "reloading" ++ testSuffix) </>) ["C.hs","B.hs","A.hs"]
                    in [pathC,pathB,pathA] == allPathes
                         && [pathC',pathB',pathA'] == allPathes
                         && [pathC'',pathB'',pathA''] == allPathes
            _ -> False )
  , ( "reloading-package", testRoot </> "changing-cabal"
    , [ AddPackages [ testRoot </> "changing-cabal" ++ testSuffix ]]
    , appendFile (testRoot </> "changing-cabal" ++ testSuffix </> "some-test-package.cabal") ", B"
    , [ AddPackages [testRoot </> "changing-cabal" ++ testSuffix]
      , PerformRefactoring "RenameDefinition" (testRoot </> "changing-cabal" ++ testSuffix </> "A.hs") "3:1-3:2" ["z"]
      ]
    , \case [ LoadingModules{}, LoadedModules [(pathA,_)], LoadingModules{}, LoadedModules [(pathA',_)]
              , LoadedModules [(pathB',_)], ModulesChanged _
              , LoadingModules{}, LoadedModules [(pathA'',_)], LoadedModules [(pathB'',_)]
              ] -> let [pA,pB] = map ((testRoot </> "changing-cabal" ++ testSuffix) </>) ["A.hs","B.hs"]
                    in pA == pathA && pA == pathA' && pA == pathA'' && pB == pathB' && pB == pathB''
            _ -> False )
  , ( "adding-module", testRoot </> "reloading", [AddPackages [ testRoot </> "reloading" ++ testSuffix ]]
    , writeFile (testRoot </> "reloading" ++ testSuffix </> "D.hs") "module D where\nd = ()"
    , [ ReLoad [testRoot </> "reloading" ++ testSuffix </> "D.hs"] [] [] ]
    , \case [ LoadingModules {}, LoadedModules {}, LoadedModules {}, LoadedModules {}, LoadingModules {}
              , LoadingModules {}, LoadedModules {}, LoadedModules {}, LoadedModules {}, LoadedModules {}] -> True
            _ -> False )
  , ( "reloading-remove", testRoot </> "reloading", [ AddPackages [ testRoot </> "reloading" ++ testSuffix ]]
    , do removeFile (testRoot </> "reloading" ++ testSuffix </> "A.hs")
         removeFile (testRoot </> "reloading" ++ testSuffix </> "B.hs")
    , [ ReLoad [] [testRoot </> "reloading" ++ testSuffix </> "C.hs"]
                  [testRoot </> "reloading" ++ testSuffix </> "A.hs", testRoot </> "reloading" ++ testSuffix </> "B.hs"]
      , PerformRefactoring "RenameDefinition" (testRoot </> "reloading" ++ testSuffix </> "C.hs") "3:1-3:2" ["d"]
      ]
    , \case [ LoadingModules{}, LoadedModules [(pathC,_)], LoadedModules [(pathB,_)], LoadedModules [(pathA,_)]
              , LoadingModules{}, LoadedModules [(pathC',_)], ModulesChanged _, LoadingModules{}, LoadedModules [(pathC'',_)] ]
              -> let [pC,pB,pA] = map ((testRoot </> "reloading" ++ testSuffix) </>) ["C.hs","B.hs","A.hs"]
                  in pA == pathA && pB == pathB && pC == pathC && pC == pathC' && pC == pathC''
            _ -> False )
  , ( "remove-package", testRoot </> "multi-packages-dependent"
    , [ AddPackages [ testRoot </> "multi-packages-dependent" ++ testSuffix </> "package1"
                    , testRoot </> "multi-packages-dependent" ++ testSuffix </> "package2" ]]
    , removeDirectoryRecursive (testRoot </> "multi-packages-dependent" ++ testSuffix </> "package2")
    , [ RemovePackages [testRoot </> "multi-packages-dependent" ++ testSuffix </> "package2"]
      , PerformRefactoring "RenameDefinition" (testRoot </> "multi-packages-dependent" ++ testSuffix </> "package1" </> "A.hs")
                                              "3:1-3:2" ["d"]
      ]
    , \case [ LoadingModules{}, LoadedModules [(pathA',_)], LoadedModules [(pathB',_)], ModulesChanged _, LoadingModules{}, LoadedModules [(pathA,_)] ]
              -> let [pA,pB] = map ((testRoot </> "multi-packages-dependent" ++ testSuffix) </>) [ "package1" </> "A.hs", "package2" </> "B.hs"]
                  in pA == pathA && pA == pathA' && pB == pathB'
            _ -> False )
  ]

pkgDbTests :: [(String, IO (), [ClientMessage], [ResponseMsg])]
pkgDbTests
  = [ ( "cabal-sandbox"
      , withCurrentDirectory (testRoot </> "cabal-sandbox") initCabalSandbox
      , [SetPackageDB CabalSandboxDB, AddPackages [testRoot </> "cabal-sandbox"]]
      , [ LoadingModules [testRoot </> "cabal-sandbox" </> "UseGroups.hs"]
        , LoadedModules [(testRoot </> "cabal-sandbox" </> "UseGroups.hs", "UseGroups")]] )
    , ( "cabal-sandbox-auto"
      , withCurrentDirectory (testRoot </> "cabal-sandbox") initCabalSandbox
      , [SetPackageDB AutoDB, AddPackages [testRoot </> "cabal-sandbox"]]
      , [ LoadingModules [testRoot </> "cabal-sandbox" </> "UseGroups.hs"]
        , LoadedModules [(testRoot </> "cabal-sandbox" </> "UseGroups.hs", "UseGroups")]] )
    , ( "pkg-db-reload"
      , withCurrentDirectory (testRoot </> "cabal-sandbox") initCabalSandbox
      , [ SetPackageDB AutoDB
        , AddPackages [testRoot </> "cabal-sandbox"]
        , ReLoad [] [testRoot </> "cabal-sandbox" </> "UseGroups.hs"] []]
      , [ LoadingModules [testRoot </> "cabal-sandbox" </> "UseGroups.hs"]
        , LoadedModules [(testRoot </> "cabal-sandbox" </> "UseGroups.hs", "UseGroups")]
        , LoadingModules [testRoot </> "cabal-sandbox" </> "UseGroups.hs"]
        , LoadedModules [(testRoot </> "cabal-sandbox" </> "UseGroups.hs", "UseGroups")] ])
    ]
  where initCabalSandbox = do
          sandboxExists <- doesDirectoryExist ".cabal-sandbox"
          when sandboxExists $ tryToExecute "cabal" ["sandbox", "delete"]
          execute "cabal" ["sandbox", "init"]
          withCurrentDirectory ("groups-0.4.0.0") $ do
            execute "cabal" ["sandbox", "init", "--sandbox", ".." </> ".cabal-sandbox"]
            execute "cabal" ["install"]
        initStack = do
          execute "stack" ["clean"]
          execute "stack" ["build"]


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
makeDaemonTest port (label, input, expected) = testCase label $ do
    actual <- communicateWithDaemon port (map Right (SetPackageDB DefaultDB : input))
    assertEqual "" expected actual

makeRefactorTest :: MVar Int -> (String, FilePath, [ClientMessage], [ResponseMsg] -> Bool) -> TestTree
makeRefactorTest port (label, dir, input, validator) = testCase label $ do
    exists <- doesDirectoryExist (dir ++ testSuffix)
    -- clear the target directory from possible earlier test runs
    when exists $ removeDirectoryRecursive (dir ++ testSuffix)
    copyDir dir (dir ++ testSuffix)
    actual <- communicateWithDaemon port (map Right (SetPackageDB DefaultDB : input))
    assertBool ("The responses are not the expected: " ++ show actual) (validator actual)
  `finally` removeDirectoryRecursive (dir ++ testSuffix)

makeReloadTest :: MVar Int -> (String, FilePath, [ClientMessage], IO (), [ClientMessage], [ResponseMsg] -> Bool) -> TestTree
makeReloadTest port (label, dir, input1, io, input2, validator) = testCase label $ do
    exists <- doesDirectoryExist (dir ++ testSuffix)
    -- clear the target directory from possible earlier test runs
    when exists $ removeDirectoryRecursive (dir ++ testSuffix)
    copyDir dir (dir ++ testSuffix)
    actual <- communicateWithDaemon port (map Right (SetPackageDB DefaultDB : input1) ++ [Left io] ++ map Right input2)
    assertBool ("The responses are not the expected: " ++ show actual) (validator actual)
  `finally` removeDirectoryRecursive (dir ++ testSuffix)

makePkgDbTest :: MVar Int -> (String, IO (), [ClientMessage], [ResponseMsg]) -> TestTree
makePkgDbTest port (label, prepare, inputs, expected)
  = localOption (mkTimeout ({- 30s -} 1000 * 1000 * 30))
      $ testCase label $ do
          actual <- communicateWithDaemon port ([Left prepare] ++ map Right inputs)
          assertEqual "" expected actual

makeCompProblemTest :: MVar Int -> (String, [Either (IO ()) ClientMessage], [ResponseMsg] -> Bool) -> TestTree
makeCompProblemTest port (label, actions, validator) = testCase label $ do
  actual <- communicateWithDaemon port actions
  assertBool ("The responses are not the expected: " ++ show actual) (validator actual)

communicateWithDaemon :: MVar Int -> [Either (IO ()) ClientMessage] -> IO [ResponseMsg]
communicateWithDaemon port msgs = withSocketsDo $ do
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
    sendAll sock $ encode Disconnect
    resps <- readSockResponsesUntil sock Disconnected BS.empty
    sendAll sock $ encode Stop
    close sock
    return (concat intermedRes ++ resps)
  where waitToConnect sock addr
          = connect sock addr `catch` \(e :: SomeException) -> waitToConnect sock addr
        retryConnect port = do portNum <- readMVar port
                               forkIO $ runDaemon [show portNum, "True"]
                               return portNum
          `catch` \(e :: SomeException) -> do putStrLn ("exception caught: `" ++ show e ++ "` trying with a new port")
                                              modifyMVar_ port (\i -> if i < pORT_NUM_END
                                                                        then return (i+1)
                                                                        else error "The port number reached the maximum")
                                              retryConnect port


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
                 then return $ List.delete rsp recognized
                 else readSockResponsesUntil sock rsp fullBS

testRoot = "examples" </> "Project"

deriving instance Eq UndoRefactor
deriving instance Eq ResponseMsg
instance FromJSON UndoRefactor
instance FromJSON ResponseMsg
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
