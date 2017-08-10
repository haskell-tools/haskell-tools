
{-# LANGUAGE LambdaCase
           , ViewPatterns
           , TypeFamilies
           , RecordWildCards
           , DeriveGeneric
           #-}
module Main where

import Criterion.Measurement hiding (runBenchmark)
import Criterion.Types hiding(measure)

import Control.Exception (finally)
import Control.Monad
import Data.Aeson (ToJSON, FromJSON, encode)
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Lazy.Char8 as LazyBS (unpack)
import Data.Knob (newKnob, newFileHandle)
import Data.List
import Data.List.Split (chunksOf)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import GHC.Generics (Generic)
import System.Directory
import System.Environment (getArgs)
import System.FilePath (FilePath, (</>))
import System.IO

import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings)
import Language.Haskell.Tools.Refactor.CLI (normalRefactorSession)

rootDir = "examples"

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
    void $ normalRefactorSession builtinRefactorings inHandle outHandle [wd]
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
