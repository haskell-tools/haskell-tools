module Main where

import Control.Concurrent.MVar (newEmptyMVar)
import Language.Haskell.Tools.Daemon (runDaemon)
import Language.Haskell.Tools.Daemon.Mode (socketMode)
import Language.Haskell.Tools.Daemon.Options (parseDaemonCLI)
import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings, builtinQueries)

main :: IO ()
main = do store <- newEmptyMVar
          runDaemon builtinRefactorings builtinQueries socketMode store =<< parseDaemonCLI
