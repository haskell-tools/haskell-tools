module Main where

import System.Environment

import Language.Haskell.Tools.Refactor.Daemon

main :: IO ()
main = runDaemonCLI
