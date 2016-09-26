module Main where

import System.Environment

import Language.Haskell.Tools.Refactor.CLI

main :: IO ()
main = refactorSession =<< getArgs
