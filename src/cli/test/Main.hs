module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (assertBool, testCase)

import Control.Monad ((=<<), when, forM_)
import Data.ByteString.Char8 (pack, unpack)
import Data.Knob (newKnob, newFileHandle, getContents)
import qualified Data.List as List
import System.Directory
import System.FilePath
import System.IO

import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings)
import Language.Haskell.Tools.Refactor.CLI (SharedDaemonOptions(..), CLIOptions(..), normalRefactorSession)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests
  = testGroup "cli-tests" [
      makeCliTest ( "batch", ["examples"</>"example-project"]
                  , \s -> CLIOptions False False (Just $ "RenameDefinition " ++ "examples"</>("example-project"++s)</>"Demo.hs" ++ " 3:1 b")
                             (SharedDaemonOptions True Nothing False False Nothing)
                  , \_ -> ""
                  , \s _ -> checkFileContent ("examples"</>("example-project"++s)</>"Demo.hs")
                                             ("b = ()" `List.isInfixOf`))
    , makeCliTest ( "session", ["examples"</>"example-project"]
                  , \_ -> CLIOptions False False Nothing (SharedDaemonOptions True Nothing False False Nothing)
                  , \s -> "RenameDefinition " ++ "examples"</>("example-project"++s)</>"Demo.hs" ++ " 3:1 b\nExit\n"
                  , \s _ -> checkFileContent ("examples"</>("example-project"++s)</>"Demo.hs")
                                             ("b = ()" `List.isInfixOf`))
    ]

makeCliTest :: (String, [FilePath], String -> [FilePath] -> CLIOptions, String -> String, String -> String -> IO Bool) -> TestTree
makeCliTest (name, dirs, args, input, outputCheck)
  = let dir = joinPath $ longestCommonPrefix $ map splitDirectories dirs
        testdirs = map (\d -> if d == dir then dir ++ suffix else (dir ++ suffix </> makeRelative dir d)) dirs
    in testCase name $ do
      copyDir dir (dir ++ suffix)
      inKnob <- newKnob (pack (input suffix))
      inHandle <- newFileHandle inKnob "<input>" ReadMode
      outKnob <- newKnob (pack [])
      outHandle <- newFileHandle outKnob "<output>" WriteMode
      res <- normalRefactorSession builtinRefactorings inHandle outHandle (args suffix testdirs)
      actualOut <- Data.Knob.getContents outKnob
      assertBool ("The result is not what is expected. Output: " ++ (unpack actualOut))
        =<< outputCheck suffix (unpack actualOut)
  where suffix = "_test_" ++ name

checkFileContent :: FilePath -> (String -> Bool) -> IO Bool
checkFileContent fp check = check <$> readFile fp

copyDir :: FilePath -> FilePath -> IO ()
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

longestCommonPrefix :: (Eq a) => [[a]] -> [a]
longestCommonPrefix = foldl1 commonPrefix

commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []
