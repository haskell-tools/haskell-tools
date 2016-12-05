
{-# LANGUAGE LambdaCase
           , ViewPatterns
           , TypeFamilies
           , RecordWildCards
           , DeriveGeneric
           #-}
module Main where

import Criterion.Measurement hiding (runBenchmark)
import Criterion.Types hiding(measure)

import qualified Data.ByteString.Lazy.Char8 as LazyBS (pack, unpack)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import Data.Aeson
import Data.Knob
import GHC.Generics
import System.FilePath
import System.Directory
import System.IO
import Control.Monad
import Control.Exception
import System.Environment
import Data.List
import Data.List.Split
import Data.Time.Clock
import Data.Time.Calendar

import Language.Haskell.Tools.Refactor.CLI

rootDir = ".." </> ".." </> "examples"

main :: IO ()
main = do
  args <- getArgs
  (year, month, day) <- date
  cases <- bms2Mcases (Date {..}) bms
  case args of 
    [file] -> writeFile file (show $ encode cases)
    _ -> putStrLn $ LazyBS.unpack $ encode cases
  putStrLn "Execution times (cycles):"
  mapM_ (\c -> putStrLn $ "# " ++ bmId (bm c) ++ ": " ++ showGrouped (measCycles (ms c))) cases
    where showGrouped = reverse . concat . intersperse " " . chunksOf 3 . reverse . show

date :: IO (Integer,Int,Int) -- (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

data Date = Date { year  :: Integer
                 , month :: Int
                 , day   :: Int
                 }
  deriving (Eq, Show, Generic)

data BM = BM { bmId       :: String
             , workingDir :: FilePath
             , refactors  :: [String]
             }
  deriving (Eq, Show, Generic)

data BMCase = BMCase { bm :: BM
                     , ms :: Measured
                     , dt :: Date
                     }
  deriving (Eq, Show, Generic)

instance FromJSON Date
instance ToJSON Date
instance FromJSON BM
instance ToJSON BM
instance FromJSON BMCase
instance ToJSON BMCase

bms :: [BM]
bms = [ BM { bmId = "full-1", workingDir = rootDir </> "CppHs", refactors = [
          "SelectModule Language.Preprocessor.Cpphs.CppIfdef"
        , "ExtractBinding 182:8-182:36 parseResult"
        , "RenameDefinition 181:1 gDefined"
        , "GenerateSignature 51:5-51:5"
        , "GenerateSignature 50:5-50:5"
        , "GenerateSignature 49:5-49:5"
        , "ExtractBinding 47:46-47:64 linesFixed"
        , "RenameDefinition 46:1 cppIfDef"
        , "OrganizeImports"
        , "Exit"
        ]  }
      , BM { bmId = "full-2", workingDir = rootDir </> "CppHs", refactors = [
          "SelectModule Language.Preprocessor.Cpphs.MacroPass"
        , "ExtractBinding 96:29-96:47 tokenizeTT"
        , "ExtractBinding 90:11-90:67 fun"
        , "OrganizeImports"
        , "Exit"
        ]  }
      , BM { bmId = "full-3", workingDir = rootDir </> "CppHs", refactors = [
          "SelectModule Language.Preprocessor.Cpphs.CppIfdef"
        , "ExtractBinding 182:8-182:36 parseResult"
        , "RenameDefinition 181:1 gDefined"
        , "SelectModule Language.Preprocessor.Cpphs.MacroPass"
        , "ExtractBinding 96:29-96:47 tokenizeTT"
        , "SelectModule Language.Preprocessor.Cpphs.CppIfdef"
        , "GenerateSignature 51:5-51:5"
        , "GenerateSignature 50:5-50:5"
        , "GenerateSignature 49:5-49:5"
        , "SelectModule Language.Preprocessor.Cpphs.MacroPass"
        , "ExtractBinding 90:11-90:67 fun"
        , "SelectModule Language.Preprocessor.Cpphs.CppIfdef"
        , "ExtractBinding 47:46-47:64 linesFixed"
        , "RenameDefinition 46:1 cppIfDef"
        , "OrganizeImports"
        , "Exit"
        ]  }
      , BM { bmId = "3xGenerateTypeSignature", workingDir = rootDir </> "CppHs", refactors = [
          "SelectModule Language.Preprocessor.Cpphs.CppIfdef"
        , "GenerateSignature 51:5-51:5"
        , "GenerateSignature 50:5-50:5"
        , "GenerateSignature 49:5-49:5"
        , "Exit"
        ]  }
      , BM { bmId ="selects", workingDir = rootDir </> "CppHs", refactors = [
          "SelectModule Language.Preprocessor.Cpphs.CppIfdef"
        , "SelectModule Language.Preprocessor.Cpphs.MacroPass"
        , "SelectModule Language.Preprocessor.Cpphs.CppIfdef"
        , "SelectModule Language.Preprocessor.Cpphs.MacroPass"
        , "SelectModule Language.Preprocessor.Cpphs.CppIfdef"
        , "SelectModule Language.Preprocessor.Cpphs.MacroPass"
        , "SelectModule Language.Preprocessor.Cpphs.CppIfdef"
        , "SelectModule Language.Preprocessor.Cpphs.MacroPass"
        , "SelectModule Language.Preprocessor.Cpphs.CppIfdef"
        , "Exit"
        ]  }
      , BM { bmId ="empty", workingDir = rootDir </> "CppHs", refactors = [ "Exit" ] }
      ]



bms2Mcases :: Date -> [BM] -> IO [BMCase]
bms2Mcases = mapM . bm2Mcase

runBenchmark :: Benchmarkable -> IO Measured
runBenchmark bm = fst <$> (measure bm 1)

bm2Mms :: BM -> IO Measured
bm2Mms (BM { .. }) = runBenchmark $ benchmakable workingDir refactors

bm2Mcase :: Date -> BM -> IO BMCase
bm2Mcase d bm = BMCase bm <$> (bm2Mms bm) <*> (return d)

benchmakable :: String -> [String] -> Benchmarkable -- IO (Either String String)
benchmakable wd rfs = Benchmarkable $ \ _ -> do
  makeCliTest wd rfs

makeCliTest :: String -> [String] -> IO ()
makeCliTest wd rfs = do   
    copyDir wd (wd ++ "_orig")
    inKnob <- newKnob (BS.pack $ unlines rfs)
    inHandle <- newFileHandle inKnob "<input>" ReadMode
    outKnob <- newKnob (BS.pack [])
    outHandle <- newFileHandle outKnob "<output>" WriteMode
    refactorSession inHandle outHandle [wd]
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
