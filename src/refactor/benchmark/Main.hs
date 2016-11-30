
{-# LANGUAGE LambdaCase
           , ViewPatterns
           , TypeFamilies
           , RecordWildCards
           , DeriveGeneric
           #-}
module Main where

import GHC hiding (loadModule, ParsedModule)
import DynFlags
import GHC.Paths ( libdir )
import Module as GHC

import Control.Reference
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map
import Data.List
import Data.Either.Combinators
import Test.HUnit hiding (test)
import System.IO
import System.Exit
import System.FilePath
import Data.IntSet (member)
import Language.Haskell.TH.LanguageExtensions

import Language.Haskell.Tools.AST as AST
--import Language.Haskell.Tools.AST.Rewrite as G
--import Language.Haskell.Tools.AST.FromGHC
--import Language.Haskell.Tools.Transform
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor.Perform
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Predefined.OrganizeImports
import Language.Haskell.Tools.Refactor.Predefined.GenerateTypeSignature
import Language.Haskell.Tools.Refactor.Predefined.GenerateExports
import Language.Haskell.Tools.Refactor.Predefined.RenameDefinition
import Language.Haskell.Tools.Refactor.Predefined.ExtractBinding
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Refactor.Session

import Language.Haskell.Tools.Refactor.Predefined.DataToNewtype
import Language.Haskell.Tools.Refactor.Predefined.IfToGuards
import Language.Haskell.Tools.Refactor.Predefined.DollarApp

import Criterion.Measurement hiding (runBenchmark)
import Criterion.Types hiding(measure)
import Control.DeepSeq

import Data.Aeson
import GHC.Prim
import GHC.Generics
import System.Environment
import Data.Time.Clock
import Data.Time.Calendar
import Data.Knob

import Test.HUnit
import System.Exit
import System.Directory
import System.FilePath
import Control.Monad (forM_)
import Control.Exception
import qualified Data.List as List
import Data.ByteString.Char8 (pack, unpack)
import System.IO

import Language.Haskell.Tools.Refactor.CLI

rootDir = ".." </> ".." </> "examples"

main :: IO ()
main = do
  [file] <- getArgs
  (year, month, day) <- date
  cases <- bms2Mcases (Date {..}) bms
  writeFile file (show $ encode cases)

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
bms = [ BM { bmId = "full-1", workingDir = rootDir, refactors = [
          "SelectModule C"
        , "RenameDefinition 5:1-5:2 bb"
        , "RenameDefinition 3:1-3:2 cc"
        , "Exit"
        ]  }
      , BM { bmId = "full-2", workingDir = rootDir, refactors = [
          "SelectModule C"
        , ""
        , ""
        ]  }
      ]

{-
oneShotRefactor moduleName refactoring = refactorSession stdin stdout 
           [ "-dry-run", "-one-shot", "-package", "ghc"
           , "-module-name=" ++ moduleName  --Language.Haskell.Tools.AST"
           , "-refactoring=\"" ++ refactoring ++ "\""
           , "../../examples" ]
-}




bms2Mcases :: Date -> [BM] -> IO [BMCase]
bms2Mcases d = mapM (bm2Mcase d)

runBenchmark :: Benchmarkable -> IO Measured
runBenchmark bm = fst <$> (measure bm 1)

bm2Mms :: BM -> IO Measured
bm2Mms (BM { .. }) = runBenchmark $ benchmakable workingDir refactors

bm2Mcase :: Date -> BM -> IO BMCase
bm2Mcase d bm = BMCase bm <$> (bm2Mms bm) <*> (return d)


makeCliTest :: String -> [String] -> IO ()
makeCliTest wd rfs = let dir = joinPath $ longestCommonPrefix $ map splitDirectories [wd]
  in do   
    copyDir dir (dir ++ "_orig")
    inKnob <- newKnob (pack $ unlines rfs)
    inHandle <- newFileHandle inKnob "<input>" ReadMode
    outKnob <- newKnob (pack [])
    outHandle <- newFileHandle outKnob "<output>" WriteMode
    refactorSession inHandle outHandle wd
  `finally` do removeDirectoryRecursive dir
               renameDirectory (dir ++ "_orig") dir

benchmakable :: String -> [String] -> Benchmarkable -- IO (Either String String)
benchmakable wd rfs = Benchmarkable $ \ _ -> do
  makeCliTest wd rfs
    --res <- performRefactor c w f t
    --return $ res `deepseq` ()
{-
performRefactor :: String -> FilePath -> [String] -> String -> IO (Either String String)
performRefactor command workingDir flags target = 
  runGhc (Just libdir) $ do
    initGhcFlags
    useFlags flags
    useDirs [workingDir]
    ((\case Right r -> Right (newContent r); Left l -> Left l) <$> (refact =<< parseTyped =<< loadModule workingDir target))
  where refact m = performCommand (readCommand target command) (target,m) []
        newContent (ContentChanged (_, newContent) : ress) = prettyPrint newContent
        newContent (_ : ress) = newContent ress
-}





{-
  cpphs
  instance-control


    | OrganizeImports
    | GenerateExports
    | GenerateSignature RealSrcSpan
    | RenameDefinition RealSrcSpan String
    | ExtractBinding RealSrcSpan String



-}

commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []

longestCommonPrefix :: (Eq a) => [[a]] -> [a]
longestCommonPrefix = foldl1 commonPrefix

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
