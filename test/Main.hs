{-# LANGUAGE LambdaCase #-}
module Main where

import GHC
import DynFlags
import GHC.Paths ( libdir )

import Control.Monad.IO.Class
import Data.Maybe
import Test.HUnit hiding (test)
import System.IO

import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AST.FromGHC.Module
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AnnTrf.RangeToSource
import Language.Haskell.Tools.AnnTrf.RangeToTemplate
import Language.Haskell.Tools.PrettyPrint

main :: IO ()
main = do checkCorrectlyPrinted "..\\examples" "Package.Simple"
          -- checkCorrectlyPrinted "..\\examples" "Package.Export"
          checkCorrectlyPrinted "..\\examples" "Package.Import"

checkCorrectlyPrinted :: String -> String -> IO ()
checkCorrectlyPrinted workingDir moduleName 
  = do -- need to use binary or line endings will be translated
       expectedHandle <- openBinaryFile (workingDir ++ "\\" ++ map (\case '.' -> '\\'; c -> c) moduleName ++ ".hs") ReadMode
       expected <- hGetContents expectedHandle
       actual <- parseAndPrettyPrint workingDir moduleName
       assertEqual "The original and the transformed source differ" expected actual

parseAndPrettyPrint :: String -> String -> IO String
parseAndPrettyPrint workingDir moduleName = 
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    -- don't generate any code
    setSessionDynFlags $ gopt_set (dflags { importPaths = [workingDir], hscTarget = HscNothing, ghcLink = NoLink }) Opt_KeepRawTokenStream
    target <- guessTarget moduleName Nothing
    setTargets [target]
    load LoadAllTargets
    modSum <- getModSummary $ mkModuleName moduleName
    p <- parseModule modSum
    
    let annots = fst $ pm_annotations p
        srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
    
    return $ prettyPrint 
           $ rangeToSource srcBuffer
           $ cutUpRanges
           $ runTrf annots 
           $ trfModule 
           $ pm_parsed_source p
           