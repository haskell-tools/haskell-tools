module Main where

import System.IO
import System.Environment
import System.Exit

import Language.Haskell.Tools.Refactor.CLI

main :: IO ()
main = exit =<< normalRefactorSession stdin stdout =<< getArgs
  where exit :: Bool -> IO ()
        exit True = exitSuccess
        exit False = exitFailure
