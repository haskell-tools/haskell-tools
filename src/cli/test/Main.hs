module Main where

import Test.Tasty
import Test.Tasty.HUnit

import System.Exit
import System.Directory
import System.FilePath
import Control.Monad
import Control.Exception
import qualified Data.List as List
import Data.Knob
import Data.ByteString.Char8 (pack, unpack)
import System.IO

import Language.Haskell.Tools.Refactor.CLI

main :: IO ()
main = do nightlyTests <- benchTests
          defaultMain $ testGroup "cli tests" (allTests ++ nightlyTests)

allTests :: [TestTree]
allTests = map makeCliTest cliTests

makeCliTest :: ([FilePath], [String], String, String) -> TestTree
makeCliTest (dirs, args, input, output) = let dir = joinPath $ longestCommonPrefix $ map splitDirectories dirs
                                              testdirs = map (((dir ++ "_test") </>) . makeRelative dir) dirs
  in testCase dir $ do
    exists <- doesDirectoryExist (dir ++ "_test")
    when exists $ removeDirectoryRecursive (dir ++ "_test")
    copyDir dir (dir ++ "_test")
    inKnob <- newKnob (pack input)
    inHandle <- newFileHandle inKnob "<input>" ReadMode
    outKnob <- newKnob (pack [])
    outHandle <- newFileHandle outKnob "<output>" WriteMode
    res <- refactorSession inHandle outHandle (args ++ testdirs)
    actualOut <- Data.Knob.getContents outKnob
    assertEqual "" (filter (/= '\r') output) (filter (/= '\r') $ unpack actualOut)
  `finally` removeDirectoryRecursive (dir ++ "_test")

cliTests :: [([FilePath], [String], String, String)]
cliTests
  = [ ( [testRoot </> "Project" </> "cpp-opt"]
      , ["-dry-run", "-one-shot", "-module-name=A"]
      , "", oneShotPrefix ["A"] ++ "-module-name or -refactoring flag not specified correctly. Not doing any refactoring.\n")
    , ( [testRoot </> "Project" </> "source-dir"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""]
      , "", oneShotPrefix ["A"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()\n")
    , ( [testRoot </> "Project" </> "working-dir"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"OrganizeImports\""]
      , "", oneShotPrefix ["A"] ++ "### Module changed: A\n### new content:\n{-# LANGUAGE TemplateHaskell #-}\nmodule A where\n\nimport Language.Haskell.TH\n\n$(runIO (readFile \"data.txt\") >> return [])\n")
    , ( [testRoot </> "Project" </> "source-dir-outside"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""]
      , "", oneShotPrefix ["A"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()\n")
    , ( [testRoot </> "Project" </> "no-cabal"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""]
      , "", oneShotPrefix ["A"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()\n")
    , ( [testRoot </> "Project" </> "has-cabal"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""]
      , "", oneShotPrefix ["A"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()\n")
    , ( [testRoot </> "Project" </> "selection"], []
      , "SelectModule C\nSelectModule B\nRenameDefinition 5:1-5:2 bb\nSelectModule C\nRenameDefinition 3:1-3:2 cc\nExit"
      , prefixText ["C","B"] ++ "no-module-selected> C> B> "
          ++ reloads ["B"] ++ "B> C> "
          ++ reloads ["C", "B"] ++ "C> "
          )
    , ( [testRoot </> "Project" </> "reloading"], []
      , "SelectModule C\nRenameDefinition 3:1-3:2 cc\nSelectModule B\nRenameDefinition 5:1-5:2 bb\nExit"
      , prefixText ["C","B","A"] ++ "no-module-selected> C> "
          ++ reloads ["C", "B", "A"] ++ "C> B> "
          ++ reloads ["B", "A"] ++ "B> ")
    , ( map ((testRoot </> "Project" </> "multi-packages") </>) ["package1", "package2"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"RenameDefinition 3:1-3:2 xx\""], ""
      , oneShotPrefix ["B", "A"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nxx = ()\n"
      )
    , ( map ((testRoot </> "Project" </> "multi-packages-flags") </>) ["package1", "package2"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"RenameDefinition 3:1-3:2 xx\""], ""
      , oneShotPrefix ["B", "A"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nxx = \\case () -> ()\n"
      )
    , ( map ((testRoot </> "Project" </> "multi-packages-same-module") </>) ["package1", "package2"]
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"RenameDefinition 3:1-3:2 xx\""], ""
      , "Compiling modules. This may take some time. Please wait.\nLoaded module: A\n"
          ++ "The following modules are ignored: A. Multiple modules with the same qualified name are not supported.\n"
          ++ "All modules loaded.\n"
          ++ "### Module changed: A\n### new content:\nmodule A where\n\nxx = ()\n"
      )
    ]

benchTests :: IO [TestTree]
benchTests
  = forM ["full-1", "full-2", "full-3"] $ \id -> do
      commands <- readFile ("bench-tests" </> id <.> "txt")
      return $ makeCliTest (["examples" </> "CppHs"], [], filter (/='\r') commands, expectedOut id)

expectedOut "full-1"
  = prefixText cppHsMods ++ "no-module-selected> Language.Preprocessor.Cpphs.CppIfdef> "
      ++ concat (replicate 8 (reloads cppIfDefReloads ++ "Language.Preprocessor.Cpphs.CppIfdef> "))
expectedOut "full-2"
  = prefixText cppHsMods ++ "no-module-selected> Language.Preprocessor.Cpphs.MacroPass> "
      ++ concat (replicate 3 (reloads macroPassReloads ++ "Language.Preprocessor.Cpphs.MacroPass> "))
expectedOut "full-3"
  = prefixText cppHsMods ++ "no-module-selected> Language.Preprocessor.Cpphs.CppIfdef> "
      ++ concat (replicate 2 (reloads cppIfDefReloads ++ "Language.Preprocessor.Cpphs.CppIfdef> "))
      ++ "Language.Preprocessor.Cpphs.MacroPass> "
      ++ reloads macroPassReloads ++ "Language.Preprocessor.Cpphs.MacroPass> "
      ++ "Language.Preprocessor.Cpphs.CppIfdef> "
      ++ concat (replicate 3 (reloads cppIfDefReloads ++ "Language.Preprocessor.Cpphs.CppIfdef> "))
      ++ "Language.Preprocessor.Cpphs.MacroPass> "
      ++ reloads macroPassReloads ++ "Language.Preprocessor.Cpphs.MacroPass> "
      ++ "Language.Preprocessor.Cpphs.CppIfdef> "
      ++ concat (replicate 3 (reloads cppIfDefReloads ++ "Language.Preprocessor.Cpphs.CppIfdef> "))

cppIfDefReloads = [ "Language.Preprocessor.Cpphs.CppIfdef"
                  , "Language.Preprocessor.Cpphs.RunCpphs"
                  , "Language.Preprocessor.Cpphs" ]
macroPassReloads = "Language.Preprocessor.Cpphs.MacroPass" : cppIfDefReloads

cppHsMods = [ "Language.Preprocessor.Unlit"
            , "Language.Preprocessor.Cpphs.SymTab"
            , "Language.Preprocessor.Cpphs.Position"
            , "Language.Preprocessor.Cpphs.ReadFirst"
            , "Language.Preprocessor.Cpphs.Options"
            , "Language.Preprocessor.Cpphs.HashDefine"
            , "Language.Preprocessor.Cpphs.Tokenise"
            , "Language.Preprocessor.Cpphs.MacroPass"
            , "Language.Preprocessor.Cpphs.CppIfdef"
            , "Language.Preprocessor.Cpphs.RunCpphs"
            , "Language.Preprocessor.Cpphs" ]

testRoot = "examples"

prefixText :: [String] -> String
prefixText mods
  = "Compiling modules. This may take some time. Please wait.\n"
      ++ concatMap (\m -> "Loaded module: " ++ m ++ "\n") mods
      ++ "All modules loaded. Use 'SelectModule module-name' to select a module.\n"

oneShotPrefix :: [String] -> String
oneShotPrefix mods
  = "Compiling modules. This may take some time. Please wait.\n"
      ++ concatMap (\m -> "Loaded module: " ++ m ++ "\n") mods
      ++ "All modules loaded.\n"


reloads :: [String] -> String
reloads mods = concatMap (\m -> "Re-loaded module: " ++ m ++ "\n") mods

copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  exists <- doesDirectoryExist dst
  -- clear the target directory from possible earlier test runs
  when exists $ removeDirectoryRecursive dst
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
