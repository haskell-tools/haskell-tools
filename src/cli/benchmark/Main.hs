
{-# LANGUAGE DeriveGeneric, MonoLocalBinds, RecordWildCards #-}

module Main where

import Criterion
import Criterion.Main

import Control.Exception (finally)
import Control.Monad
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Knob (newKnob, newFileHandle)
import GHC.Generics (Generic)
import System.Directory
import System.FilePath (FilePath, (</>))
import System.IO

import Language.Haskell.Tools.Daemon.Options (SharedDaemonOptions(..))
import Language.Haskell.Tools.Daemon.PackageDB (PackageDB(..))
import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings, builtinQueries)
import Language.Haskell.Tools.Refactor.CLI (CLIOptions(..), normalRefactorSession)

rootDir = "examples"

main :: IO ()
main = defaultMain (map createBench bms)

data BM = BM { bmId       :: String
             , workingDir :: FilePath
             , refactors  :: [String]
             }
  deriving (Eq, Show, Generic)

instance FromJSON BM
instance ToJSON BM

bms :: [BM]
bms = [ BM { bmId = "full-1", workingDir = rootDir </> "CppHs", refactors = [
        "ExtractBinding Language.Preprocessor.Cpphs.CppIfdef 182:8-182:36 parseResult"
        , "RenameDefinition Language.Preprocessor.Cpphs.CppIfdef 181:1 gDefined"
        , "GenerateSignature Language.Preprocessor.Cpphs.CppIfdef 51:5-51:5"
        , "GenerateSignature Language.Preprocessor.Cpphs.CppIfdef 50:5-50:5"
        , "GenerateSignature Language.Preprocessor.Cpphs.CppIfdef 49:5-49:5"
        , "ExtractBinding Language.Preprocessor.Cpphs.CppIfdef 47:46-47:64 linesFixed"
        , "RenameDefinition Language.Preprocessor.Cpphs.CppIfdef 46:1 cppIfDef"
        , "OrganizeImports Language.Preprocessor.Cpphs.CppIfdef"
        , "Exit"
        ]  }
      , BM { bmId = "full-2", workingDir = rootDir </> "CppHs", refactors = [
        "ExtractBinding Language.Preprocessor.Cpphs.MacroPass 96:29-96:47 tokenizeTT"
        , "ExtractBinding Language.Preprocessor.Cpphs.MacroPass 90:11-90:67 fun"
        , "OrganizeImports Language.Preprocessor.Cpphs.MacroPass"
        , "Exit"
        ]  }
      , BM { bmId = "full-3", workingDir = rootDir </> "CppHs", refactors = [
        "ExtractBinding Language.Preprocessor.Cpphs.CppIfdef 182:8-182:36 parseResult"
        , "RenameDefinition Language.Preprocessor.Cpphs.CppIfdef 181:1 gDefined"
        , "ExtractBinding Language.Preprocessor.Cpphs.MacroPass 96:29-96:47 tokenizeTT"
        , "GenerateSignature Language.Preprocessor.Cpphs.CppIfdef 51:5-51:5"
        , "GenerateSignature Language.Preprocessor.Cpphs.CppIfdef 50:5-50:5"
        , "GenerateSignature Language.Preprocessor.Cpphs.CppIfdef 49:5-49:5"
        , "ExtractBinding Language.Preprocessor.Cpphs.MacroPass 90:11-90:67 fun"
        , "ExtractBinding Language.Preprocessor.Cpphs.CppIfdef 47:46-47:64 linesFixed"
        , "RenameDefinition Language.Preprocessor.Cpphs.CppIfdef 46:1 cppIfDef"
        , "OrganizeImports Language.Preprocessor.Cpphs.CppIfdef"
        , "Exit"
        ]  }
      , BM { bmId = "3xGenerateTypeSignature", workingDir = rootDir </> "CppHs", refactors = [
        "GenerateSignature Language.Preprocessor.Cpphs.CppIfdef 51:5-51:5"
        , "GenerateSignature Language.Preprocessor.Cpphs.CppIfdef 50:5-50:5"
        , "GenerateSignature Language.Preprocessor.Cpphs.CppIfdef 49:5-49:5"
        , "Exit"
        ]  }
      , BM { bmId ="empty", workingDir = rootDir </> "CppHs", refactors = [ "Exit" ] }
      ]

createBench :: BM -> Benchmark
createBench (BM { .. }) = bench bmId $ nfIO $ makeCliTest workingDir refactors

makeCliTest :: String -> [String] -> IO ()
makeCliTest wd rfs = do
    copyDir wd (wd ++ "_orig")
    inKnob <- newKnob (BS.pack $ unlines rfs)
    inHandle <- newFileHandle inKnob "<input>" ReadMode
    outKnob <- newKnob (BS.pack [])
    outHandle <- newFileHandle outKnob "<output>" WriteMode
    void $ normalRefactorSession builtinRefactorings builtinQueries inHandle outHandle
             (CLIOptions False False Nothing (SharedDaemonOptions True Nothing False False Nothing (Just DefaultDB)) [wd])
  `finally` do removeDirectoryRecursive wd
               renameDirectory (wd ++ "_orig") wd


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
