{-# LANGUAGE StandaloneDeriving, LambdaCase #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import System.Exit
import System.Directory
import System.FilePath
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Network.Socket hiding (KeepAlive, send, recv)
import Network.Socket.ByteString.Lazy as Sock
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List as List
import Data.Aeson
import Data.Maybe
import System.IO
import System.Directory

import Debug.Trace

import Language.Haskell.Tools.Refactor.Daemon

main :: IO ()
main = do -- create one daemon process for the whole testing session
          -- with separate processes it is not a problem
          forkIO $ runDaemon ["4123", "True"]
          tr <- canonicalizePath testRoot
          defaultMain (allTests tr)
          stopDaemon

allTests :: FilePath -> TestTree
allTests testRoot
  = localOption (mkTimeout ({- 10s -} 1000 * 1000 * 10)) 
      $ testGroup "daemon-tests" 
          [ testGroup "simple-tests" 
              $ map (makeDaemonTest . (\(label, input, output) -> (Nothing, label, input, output))) simpleTests
          , testGroup "loading-tests" 
              $ map (makeDaemonTest . (\(label, input, output) -> (Nothing, label, input, output))) loadingTests
          , testGroup "refactor-tests" 
              $ map (makeDaemonTest . (\(label, dir, input, output) -> (Just (testRoot </> dir), label, input, output))) (refactorTests testRoot)
          , testGroup "reload-tests" 
              $ map makeReloadTest reloadingTests
          , selfLoadingTest
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
    , [LoadedModules [testRoot </> "has-cabal" </> "A.hs"]] )
  , ( "no-cabal"
    , [AddPackages [testRoot </> "no-cabal"]]
    , [LoadedModules [testRoot </> "no-cabal" </> "A.hs"]] )
  , ( "source-dir"
    , [AddPackages [testRoot </> "source-dir"]]
    , [LoadedModules [testRoot </> "source-dir" </> "src" </> "A.hs"]] )
  , ( "source-dir-outside"
    , [AddPackages [testRoot </> "source-dir-outside"]]
    , [LoadedModules [testRoot </> "source-dir-outside" </> ".." </> "src" </> "A.hs"]] )
  , ( "multi-packages"
    , [ AddPackages [ testRoot </> "multi-packages" </> "package1"
                    , testRoot </> "multi-packages" </> "package2" ]]
    , [ LoadedModules [ testRoot </> "multi-packages" </> "package2" </> "B.hs"
                      , testRoot </> "multi-packages" </> "package1" </> "A.hs"]] )
  , ( "multi-packages-flags"
    , [ AddPackages [ testRoot </> "multi-packages-flags" </> "package1"
                    , testRoot </> "multi-packages-flags" </> "package2" ]]
    , [ LoadedModules [ testRoot </> "multi-packages-flags" </> "package2" </> "B.hs"
                      , testRoot </> "multi-packages-flags" </> "package1" </> "A.hs"]] )
  , ( "multi-packages-dependent"
    , [ AddPackages [ testRoot </> "multi-packages-dependent" </> "package1"
                    , testRoot </> "multi-packages-dependent" </> "package2" ]]
    , [ LoadedModules [ testRoot </> "multi-packages-dependent" </> "package1" </> "A.hs"
                      , testRoot </> "multi-packages-dependent" </> "package2" </> "B.hs"]] )
  , ( "has-th"
    , [AddPackages [testRoot </> "has-th"]]
    , [LoadedModules [testRoot </> "has-th" </> "TH.hs", testRoot </> "has-th" </> "A.hs"]] )
  , ( "th-added-later"
    , [ AddPackages [testRoot </> "th-added-later" </> "package1"]
      , AddPackages [testRoot </> "th-added-later" </> "package2"] 
      ]
    , [ LoadedModules [testRoot </> "th-added-later" </> "package1" </> "A.hs"] 
      , LoadedModules [testRoot </> "th-added-later" </> "package2" </> "B.hs"]] )
  ]

sourceRoot = ".." </> ".." </> "src"

selfLoadingTest :: TestTree
selfLoadingTest = localOption (mkTimeout ({- 2 min -} 1000 * 1000 * 60 * 2)) $ testCase "self-load" $ do  
    actual <- communicateWithDaemon 
                [ Right $ AddPackages (map (sourceRoot </>) ["ast" {-, "backend-ghc", "prettyprint", "rewrite", "refactor", "cli", "daemon" -} ] ) ]
    assertBool ("The expected result is a nonempty response message list that does not contain errors. Actual result: " ++ show actual) 
               (not (null actual) && all (\case ErrorMessage {} -> False; _ -> True) actual)


refactorTests :: FilePath -> [(String, FilePath, [ClientMessage], [ResponseMsg])]
refactorTests testRoot =
  [ ( "simple-refactor", "simple-refactor"
    , [ AddPackages [ testRoot </> "simple-refactor" ++ testSuffix ]
      , PerformRefactoring "RenameDefinition" (testRoot </> "simple-refactor" ++ testSuffix </> "A.hs") "3:1-3:2" ["y"]
      ]
    , [ LoadedModules [ testRoot </> "simple-refactor" ++ testSuffix </> "A.hs" ]
      , ModulesChanged [ testRoot </> "simple-refactor" ++ testSuffix </> "A.hs" ] 
      , LoadedModules [ testRoot </> "simple-refactor" ++ testSuffix </> "A.hs" ]
      ] )
  , ( "hs-boots", "hs-boots"
    , [ AddPackages [ testRoot </> "hs-boots" ++ testSuffix ]
      , PerformRefactoring "RenameDefinition" (testRoot </> "hs-boots" ++ testSuffix </> "A.hs") "5:1-5:2" ["aa"]
      ]
    , [ LoadedModules [ testRoot </> "hs-boots" ++ testSuffix </> "B.hs-boot", testRoot </> "hs-boots" ++ testSuffix </> "A.hs-boot"
                      , testRoot </> "hs-boots" ++ testSuffix </> "A.hs", testRoot </> "hs-boots" ++ testSuffix </> "B.hs" ]
      , ModulesChanged [ testRoot </> "hs-boots" ++ testSuffix </> "A.hs", testRoot </> "hs-boots" ++ testSuffix </> "B.hs"
                       , testRoot </> "hs-boots" ++ testSuffix </> "A.hs-boot" ] 
      , LoadedModules [ testRoot </> "hs-boots" ++ testSuffix </> "A.hs-boot" ]
      , LoadedModules [ testRoot </> "hs-boots" ++ testSuffix </> "B.hs-boot" ]
      , LoadedModules [ testRoot </> "hs-boots" ++ testSuffix </> "A.hs" ]
      , LoadedModules [ testRoot </> "hs-boots" ++ testSuffix </> "B.hs" ]
      ] )
  , ( "remove-module", "simple-refactor"
    , [ AddPackages [ testRoot </> "simple-refactor" ++ testSuffix ]
      , PerformRefactoring "RenameDefinition" (testRoot </> "simple-refactor" ++ testSuffix </> "A.hs") "1:8-1:9" ["AA"]
      ]
    , [ LoadedModules [ testRoot </> "simple-refactor" ++ testSuffix </> "A.hs" ]
      , ModulesChanged [ testRoot </> "simple-refactor" ++ testSuffix </> "AA.hs" ] 
      , LoadedModules [ testRoot </> "simple-refactor" ++ testSuffix </> "AA.hs" ]
      ] )
  ]

reloadingTests :: [(String, FilePath, [ClientMessage], IO (), [ClientMessage], [ResponseMsg])]
reloadingTests =
  [ ( "reloading-module", testRoot </> "reloading", [ AddPackages [ testRoot </> "reloading" ++ testSuffix ] ]
    , writeFile (testRoot </> "reloading" ++ testSuffix </> "C.hs") "module C where\nc = ()" 
    , [ ReLoad [testRoot </> "reloading" ++ testSuffix </> "C.hs"] []
      , PerformRefactoring "RenameDefinition" (testRoot </> "reloading" ++ testSuffix </> "C.hs") "2:1-2:2" ["d"] 
      ]
    , [ LoadedModules [ testRoot </> "reloading" ++ testSuffix </> "C.hs"
                      , testRoot </> "reloading" ++ testSuffix </> "B.hs"
                      , testRoot </> "reloading" ++ testSuffix </> "A.hs" ]
      , LoadedModules [ testRoot </> "reloading" ++ testSuffix </> "C.hs" ]
      , LoadedModules [ testRoot </> "reloading" ++ testSuffix </> "B.hs" ]
      , LoadedModules [ testRoot </> "reloading" ++ testSuffix </> "A.hs" ]
      , ModulesChanged [ testRoot </> "reloading" ++ testSuffix </> "C.hs"
                       , testRoot </> "reloading" ++ testSuffix </> "B.hs" ]
      , LoadedModules [ testRoot </> "reloading" ++ testSuffix </> "C.hs" ]
      , LoadedModules [ testRoot </> "reloading" ++ testSuffix </> "B.hs" ]
      , LoadedModules [ testRoot </> "reloading" ++ testSuffix </> "A.hs" ]
      ]
    )
  , ( "reloading-package", testRoot </> "changing-cabal"
    , [ AddPackages [ testRoot </> "changing-cabal" ++ testSuffix ] ]
    , appendFile (testRoot </> "changing-cabal" ++ testSuffix </> "some-test-package.cabal") ", B" 
    , [ AddPackages [testRoot </> "changing-cabal" ++ testSuffix]
      , PerformRefactoring "RenameDefinition" (testRoot </> "changing-cabal" ++ testSuffix </> "A.hs") "3:1-3:2" ["z"] 
      ]
    , [ LoadedModules [ testRoot </> "changing-cabal" ++ testSuffix </> "A.hs" ]
      , LoadedModules [ testRoot </> "changing-cabal" ++ testSuffix </> "A.hs"
                      , testRoot </> "changing-cabal" ++ testSuffix </> "B.hs" ]
      , ModulesChanged [ testRoot </> "changing-cabal" ++ testSuffix </> "A.hs"
                       , testRoot </> "changing-cabal" ++ testSuffix </> "B.hs" ]
      , LoadedModules [ testRoot </> "changing-cabal" ++ testSuffix </> "A.hs" ]
      , LoadedModules [ testRoot </> "changing-cabal" ++ testSuffix </> "B.hs" ]
      ]
    )
  , ( "reloading-remove", testRoot </> "reloading", [ AddPackages [ testRoot </> "reloading" ++ testSuffix ] ]
    , do removeFile (testRoot </> "reloading" ++ testSuffix </> "A.hs")
         removeFile (testRoot </> "reloading" ++ testSuffix </> "B.hs")
    , [ ReLoad [testRoot </> "reloading" ++ testSuffix </> "C.hs"] 
               [testRoot </> "reloading" ++ testSuffix </> "A.hs", testRoot </> "reloading" ++ testSuffix </> "B.hs"]
      , PerformRefactoring "RenameDefinition" (testRoot </> "reloading" ++ testSuffix </> "C.hs") "3:1-3:2" ["d"] 
      ]
    , [ LoadedModules [ testRoot </> "reloading" ++ testSuffix </> "C.hs"
                      , testRoot </> "reloading" ++ testSuffix </> "B.hs"
                      , testRoot </> "reloading" ++ testSuffix </> "A.hs" ]
      , LoadedModules [ testRoot </> "reloading" ++ testSuffix </> "C.hs" ]
      , ModulesChanged [ testRoot </> "reloading" ++ testSuffix </> "C.hs" ]
      , LoadedModules [ testRoot </> "reloading" ++ testSuffix </> "C.hs" ]
      ]
    )
  , ( "remove-package", testRoot </> "multi-packages-dependent"
    , [ AddPackages [ testRoot </> "multi-packages-dependent" ++ testSuffix </> "package1"
                    , testRoot </> "multi-packages-dependent" ++ testSuffix </> "package2" ]]
    , removeDirectoryRecursive (testRoot </> "multi-packages-dependent" ++ testSuffix </> "package2")
    , [ RemovePackages [testRoot </> "multi-packages-dependent" ++ testSuffix </> "package2"] 
      , PerformRefactoring "RenameDefinition" (testRoot </> "multi-packages-dependent" ++ testSuffix </> "package1" </> "A.hs") 
                                              "3:1-3:2" ["d"] 
      ]
    , [ LoadedModules [ testRoot </> "multi-packages-dependent" ++ testSuffix </> "package1" </> "A.hs"
                      , testRoot </> "multi-packages-dependent" ++ testSuffix </> "package2" </> "B.hs" ]
      , ModulesChanged [ testRoot </> "multi-packages-dependent" ++ testSuffix </> "package1" </> "A.hs" ]
      , LoadedModules [ testRoot </> "multi-packages-dependent" ++ testSuffix </> "package1" </> "A.hs" ]
      ]
    )
  ]

makeDaemonTest :: (Maybe FilePath, String, [ClientMessage], [ResponseMsg]) -> TestTree
makeDaemonTest (Nothing, label, input, expected) = testCase label $ do  
    actual <- communicateWithDaemon (map Right input)
    assertEqual "" expected actual
makeDaemonTest (Just dir, label, input, expected) = testCase label $ do 
    exists <- doesDirectoryExist (dir ++ testSuffix)
    -- clear the target directory from possible earlier test runs
    when exists $ removeDirectoryRecursive (dir ++ testSuffix)
    copyDir dir (dir ++ testSuffix)
    actual <- communicateWithDaemon (map Right input)
    assertEqual "" expected actual
  `finally` removeDirectoryRecursive (dir ++ testSuffix)

makeReloadTest :: (String, FilePath, [ClientMessage], IO (), [ClientMessage], [ResponseMsg]) -> TestTree
makeReloadTest (label, dir, input1, io, input2, expected) = testCase label $ do  
    exists <- doesDirectoryExist (dir ++ testSuffix)
    -- clear the target directory from possible earlier test runs
    when exists $ removeDirectoryRecursive (dir ++ testSuffix)
    copyDir dir (dir ++ testSuffix)
    actual <- communicateWithDaemon (map Right input1 ++ [Left io] ++ map Right input2)
    assertEqual "" expected actual
  `finally` removeDirectoryRecursive (dir ++ testSuffix)

communicateWithDaemon :: [Either (IO ()) ClientMessage] -> IO [ResponseMsg]
communicateWithDaemon msgs = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4123")
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  intermedRes <- sequence (map (either (\io -> do sendAll sock (encode KeepAlive)
                                                  r <- readSockResponsesUntil sock KeepAliveResponse BS.empty
                                                  io
                                                  return r)
                               ((>> return []) . sendAll sock . (`BS.snoc` '\n') . encode)) msgs)
  sendAll sock $ encode Disconnect
  resps <- readSockResponsesUntil sock Disconnected BS.empty
  close sock
  return (concat intermedRes ++ resps)

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

stopDaemon :: IO ()
stopDaemon = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4123")
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)

  sendAll sock $ encode Stop
  close sock

testRoot = ".." </> ".." </> "examples" </> "Project"

deriving instance Eq ResponseMsg
instance FromJSON ResponseMsg
instance ToJSON ClientMessage

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
