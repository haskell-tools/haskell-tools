module Main where

import Test.HUnit
import System.Exit
import System.FilePath

import Language.Haskell.Tools.Refactor.CLI

main :: IO ()
main = run allTests

run :: [Test] -> IO ()
run tests = do results <- runTestTT $ TestList tests
               if errors results + failures results > 0 
                  then exitFailure
                  else exitSuccess

allTests :: [Test]
allTests = map makeCliTest cliTests

makeCliTest :: (String, [String], String) -> Test
makeCliTest (dir, args, expected) = TestLabel dir $ TestCase $ do 
    res <- refactorSession (args ++ [dir])
    assertEqual "" (filter (/= '\r') expected) (filter (/= '\r') res)

cliTests :: [(String, [String], String)]
cliTests 
  = [ ( ".." </> ".." </> "examples" </> "Project" </> "source-dir"
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""] 
      , "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()")
    , ( ".." </> ".." </> "examples" </> "Project" </> "source-dir-outside"
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""] 
      , "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()")
    , ( ".." </> ".." </> "examples" </> "Project" </> "no-cabal"
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""] 
      , "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()")
    , ( ".." </> ".." </> "examples" </> "Project" </> "has-cabal"
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""] 
      , "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()")
    ]