module Main where

import Test.HUnit
import System.Exit
import System.Directory
import System.FilePath
import Control.Monad (forM_)
import Control.Exception
import qualified Data.List as List
import Data.Knob
import Data.ByteString.Char8 (pack, unpack)
import System.IO

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

makeCliTest :: ([FilePath], [String], String, String) -> Test
makeCliTest (dirs, args, input, output) = let dir = joinPath $ longestCommonPrefix $ map splitDirectories dirs
  in TestLabel dir $ TestCase $ do   
    copyDir dir (dir ++ "_orig")
    inKnob <- newKnob (pack input)
    inHandle <- newFileHandle inKnob "<input>" ReadMode
    outKnob <- newKnob (pack [])
    outHandle <- newFileHandle outKnob "<output>" WriteMode
    res <- refactorSession inHandle outHandle (args ++ dirs)
    actualOut <- Data.Knob.getContents outKnob
    assertEqual "" (filter (/= '\r') output) (filter (/= '\r') $ unpack actualOut)
  `finally` do removeDirectoryRecursive dir
               renameDirectory (dir ++ "_orig") dir

cliTests :: [([FilePath], [String], String, String)]
cliTests 
  = [ ( [testRoot </> "source-dir"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""] 
      , "", prefixText ["A"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()\n")
    , ( [testRoot </> "source-dir-outside"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""] 
      , "", prefixText ["A"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()\n")
    , ( [testRoot </> "no-cabal"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""] 
      , "", prefixText ["A"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()\n")
    , ( [testRoot </> "has-cabal"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""] 
      , "", prefixText ["A"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()\n")
    , ( [testRoot </> "selection"], [] 
      , "SelectModule C\nSelectModule B\nRenameDefinition 5:1-5:2 bb\nSelectModule C\nRenameDefinition 3:1-3:2 cc\nExit"
      , prefixText ["C","B"] ++ "no-module-selected> C> B> " 
          ++ reloads ["B"] ++ "B> C> "
          ++ reloads ["C", "B"] ++ "C> "
          )
    , ( [testRoot </> "reloading"], [] 
      , "SelectModule C\nRenameDefinition 3:1-3:2 cc\nSelectModule B\nRenameDefinition 5:1-5:2 bb\nExit"
      , prefixText ["C","B","A"] ++ "no-module-selected> C> " 
          ++ reloads ["C", "B", "A"] ++ "C> B> "
          ++ reloads ["B", "A"] ++ "B> ")
    , ( map ((testRoot </> "multi-packages") </>) ["package1", "package2"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"RenameDefinition 3:1-3:2 xx\""], ""
      , prefixText ["A", "B"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nxx = ()\n" 
      )
    , ( map ((testRoot </> "multi-packages-flags") </>) ["package1", "package2"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"RenameDefinition 3:1-3:2 xx\""], ""
      , prefixText ["A", "B"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nxx = \\case () -> ()\n"
      )
    , ( map ((testRoot </> "multi-packages-same-module") </>) ["package1", "package2"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"RenameDefinition 3:1-3:2 xx\""], ""
      , "Compiling modules. This may take some time. Please wait.\nLoaded module: A\n" 
          ++ "The following modules are ignored: A (from package2). Multiple modules with the same qualified name are not supported.\n"
          ++ "All modules loaded. Use 'SelectModule module-name' to select a module\n" 
          ++ "### Module changed: A\n### new content:\nmodule A where\n\nxx = ()\n"
      )
    ]

testRoot = ".." </> ".." </> "examples" </> "Project"

prefixText :: [String] -> String
prefixText mods 
  = "Compiling modules. This may take some time. Please wait.\n" 
      ++ concatMap (\m -> "Loaded module: " ++ m ++ "\n") mods 
      ++ "All modules loaded. Use 'SelectModule module-name' to select a module\n"

reloads :: [String] -> String
reloads mods = concatMap (\m -> "Re-loaded module: " ++ m ++ "\n") mods 

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