{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Control.Concurrent (killThread, forkIO)
import Control.Monad (Monad(..), mapM)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS (unpack)
import Data.Maybe (Maybe(..))
import Network.WebSockets
import System.IO (IO(..))
import Test.Tasty
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)

import Language.Haskell.Tools.Demo (ResponseMsg(..), ClientMessage(..), runDemo)

main :: IO ()
main = do -- create one daemon process for the whole testing session
          -- with separate processes it is not a problem
          threadId <- forkIO $ runDemo []
          defaultMain allTests
          killThread threadId

allTests :: TestTree
allTests
  = localOption (mkTimeout ({- 30s -} 1000 * 1000 * 30))
      $ testGroup "demo-tests"
          [ testGroup "simple-tests" $ map makeDemoTest simpleTests
          , testGroup "loading-tests" $ map makeDemoTest loadingTests
          , testGroup "refactor-tests" $ map makeDemoTest refactorTests
          , astViewTest
          ]

simpleTests :: [(String, [ClientMessage], [ResponseMsg])]
simpleTests =
  [ ( "empty-test", [], [] )
  , ( "keep-alive", [KeepAlive], [] )
  ]

loadingTests :: [(String, [ClientMessage], [ResponseMsg])]
loadingTests =
  [ ( "one-module"
    , [InitialProject [("A", "module A where\n\na = ()")]]
    , [] )
  , ( "multi-modules"
    , [InitialProject [("A", "module A where\n\na = ()"), ("B", "module B where\n\nimport A\n\nb = a")]]
    , [] )
  , ( "multi-modules-wrong-order"
    , [InitialProject [("B", "module B where\n\nimport A\n\nb = a"), ("A", "module A where\n\na = ()")]]
    , [] )
  , ( "multi-modules-added-later"
    , [ InitialProject [("A", "module A where\n\na = ()")]
      , ModuleChanged "B" "module B where\n\nimport A\n\nb = a" ]
    , [] )
  , ( "comp-problem"
    , [InitialProject [("A", "module A where\n\na = noSuchVar")]]
    , [CompilationProblem "A.hs 3:5\nVariable not in scope: noSuchVar\n\n"] )
  ]

refactorTests :: [(String, [ClientMessage], [ResponseMsg])]
refactorTests =
  [ ( "simple-refactor"
    , [ InitialProject [("A", "module A where\n\na = ()")]
      , PerformRefactoring "RenameDefinition" "A" "3:1-3:2" ["x"] ]
    , [ RefactorChanges [("A", Just "module A where\n\nx = ()")] ] )
  , ( "multi-module-refactor"
    , [ InitialProject [("A", "module A where\n\na = ()"), ("B", "module B where\n\nimport A\n\nb = a")]
      , PerformRefactoring "RenameDefinition" "A" "3:1-3:2" ["x"] ]
    , [ RefactorChanges [("A", Just "module A where\n\nx = ()"), ("B", Just "module B where\n\nimport A\n\nb = x")] ] )
  , ( "multi-module-refactor-user-added-later"
    , [ InitialProject [("A", "module A where\n\na = ()")]
      , ModuleChanged "B" "module B where\n\nimport A\n\nb = a"
      , PerformRefactoring "RenameDefinition" "A" "3:1-3:2" ["x"] ]
    , [ RefactorChanges [("A", Just "module A where\n\nx = ()"), ("B", Just "module B where\n\nimport A\n\nb = x")] ] )
  , ( "multi-module-refactor-both-added-later"
    , [ InitialProject [("B", "module B where")]
      , ModuleChanged "A" "module A where\n\na = ()"
      , ModuleChanged "B" "module B where\n\nimport A\n\nb = a"
      , PerformRefactoring "RenameDefinition" "A" "3:1-3:2" ["x"] ]
    , [ RefactorChanges [("A", Just "module A where\n\nx = ()"), ("B", Just "module B where\n\nimport A\n\nb = x")] ] )
  , ( "rename-module"
    , [ InitialProject [("A", "module A where\n\na = ()")]
      , PerformRefactoring "RenameDefinition" "A" "1:8-1:9" ["AA"] ]
    , [ RefactorChanges [("AA", Just "module AA where\n\na = ()"), ("A", Nothing)] ] )
  , ( "change-module"
    , [ InitialProject [("A", "module A where\n\na = ()")]
      , ModuleChanged "A" "module A where\n\n\na = ()"
      , PerformRefactoring "RenameDefinition" "A" "4:1-4:2" ["x"] ]
    , [ RefactorChanges [("A", Just "module A where\n\n\nx = ()")] ] )
  , ( "removed-module"
    , [ InitialProject [("A", "module A where\n\na = ()"), ("B", "module B where\n\nimport A\n\nb = a")]
      , ModuleDeleted "B"
      , PerformRefactoring "RenameDefinition" "A" "3:1-3:2" ["x"] ]
    , [ RefactorChanges [("A", Just "module A where\n\nx = ()")] ] )
  ]

makeDemoTest :: (String, [ClientMessage], [ResponseMsg]) -> TestTree
makeDemoTest (label, input, expected) = testCase label $ do
    actual <- communicateWithDemo input
    assertEqual "" expected actual

astViewTest :: TestTree
astViewTest = testCase "ast-view-test" $ do
    actual <- communicateWithDemo [ InitialProject [("A", "module A where\n\na = ()")]
                                  , PerformRefactoring "UpdateAST" "A" "1:1" [] ]
    assertBool "The response must be a simple ASTViewContent message"
      $ case actual of [ ASTViewContent _ ] -> True
                       _                    -> False

communicateWithDemo :: [ClientMessage] -> IO [ResponseMsg]
communicateWithDemo msgs = runClient "127.0.0.1" 8206 "/" $ \conn -> do
  mapM (sendTextData conn . encode) (msgs ++ [Disconnect])
  receiveAllResponses conn

receiveAllResponses :: Connection -> IO [ResponseMsg]
receiveAllResponses conn = do
  Text mess _ <- receiveDataMessage conn
  let decoded = decode mess
  case decoded of Just Disconnected -> return []
                  Just other -> (other :) <$> receiveAllResponses conn
                  _ -> error ("Unrecognized response: " ++ BS.unpack mess)

deriving instance Eq ResponseMsg
instance FromJSON ResponseMsg
instance ToJSON ClientMessage
