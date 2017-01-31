{-# LANGUAGE LambdaCase 
           #-}
module Main where

import Control.Applicative
import Control.Monad
import System.Directory
import System.Process
import System.Environment
import System.Exit
import Control.Concurrent
import Data.List
import Data.List.Split

data Result = GetFailure 
            | BuildFailure
            | RefactError
            | WrongCodeError
            | OK
  deriving Show

main :: IO ()
main = do args <- getArgs
          testHackage args

testHackage :: [String] -> IO ()
testHackage args = do 
  createDirectoryIfMissing False workDir
  withCurrentDirectory workDir $ do
    packages <- lines <$> readFile (last args)
    alreadyTested <- if noRetest then do appendFile resultFile "" 
                                         map (head . splitOn ";") . filter (not . null) . lines 
                                           <$> readFile resultFile
                                 else writeFile resultFile "" >> return []
    let filteredPackages = packages \\ alreadyTested
    createDirectoryIfMissing False "logs"
    mapM_ testAndEvaluate filteredPackages
  where workDir = "stackage-test"
        resultFile = "results.csv"

        noRetest = "-no-retest" `elem` args
        testAndEvaluate p = do
          res <- testPackage p
          appendFile resultFile (p ++ ";" ++ show res ++ "\n")


testPackage :: String -> IO Result
testPackage pack =
  runCommands [ Left ("cabal get " ++ pack, GetFailure)
              , Right $ do threadDelay 1000000
                           createDirectoryIfMissing False testedDir
                           removeDirectoryRecursive testedDir
                           renameDirectory pack testedDir
              , Left ("stack build --test --no-run-tests --bench --no-run-benchmarks > logs\\" ++ pack ++ "-build-log.txt 2>&1", BuildFailure)
              , Left ("stack exec ht-refact -- -one-shot -refactoring=ProjectOrganizeImports tested-package +RTS -M6G -RTS > logs\\" ++ pack ++ "-refact-log.txt 2>&1", RefactError)
              , Left ("stack build > logs\\" ++ pack ++ "-reload-log.txt 2>&1", WrongCodeError)
              ]
  where testedDir = "tested-package"

runCommands :: [Either (String, Result) (IO ())] -> IO Result
runCommands [] = return OK
runCommands (Left (cmd,failRes) : rest) = do 
  exitCode <- waitForProcess =<< runCommand cmd
  case exitCode of ExitSuccess -> runCommands rest
                   ExitFailure _ -> return failRes
runCommands (Right act : rest) = act >> runCommands rest