module Main where

import System.IO
import System.Environment

import Language.Haskell.Tools.Refactor.CLI

main :: IO ()
main = refactorSession stdin stdout =<< getArgs
