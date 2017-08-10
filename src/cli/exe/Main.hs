module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (IO, stdout, stdin)

import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings)
import Language.Haskell.Tools.Refactor.CLI (normalRefactorSession)

main :: IO ()
main = exit =<< normalRefactorSession builtinRefactorings stdin stdout =<< getArgs
  where exit :: Bool -> IO ()
        exit True = exitSuccess
        exit False = exitFailure
