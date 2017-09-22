module Main where

import Control.Concurrent.MVar (newEmptyMVar)
import Data.Semigroup ((<>))
import Language.Haskell.Tools.Daemon (DaemonOptions(..), runDaemon)
import Language.Haskell.Tools.Daemon.Mode (socketMode)
import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings)
import Options.Applicative

main :: IO ()
main = do store <- newEmptyMVar
          runDaemon builtinRefactorings socketMode store =<< execParser opts
  where opts
          = info (daemonOptions <**> helper)
                 (fullDesc
                   <> progDesc "Start the Haskell-tools daemon process"
                   <> header "ht-daemon: a background process for Haskell development tools.")

daemonOptions :: Parser DaemonOptions
daemonOptions = DaemonOptions <$> version <*> port <*> silent <*> noWatch <*> watch
  where version = switch (long "version"
                            <> short 'v'
                            <> help "Show the version of this software")
        port = option auto
                 (long "port"
                   <> short 'p'
                   <> value 4123
                   <> showDefault
                   <> help "Set the number of port where the daemon will wait for clients."
                   <> metavar "PORT_NUMBER")
        silent = switch (long "silent"
                          <> short 's'
                          <> help "Set to disable messages from daemon.")
        noWatch = switch (long "no-watch"
                          <> help "Disables file system watching.")
        watch = optional $ strOption
                  (long "watch-exe"
                    <> short 'w'
                    <> help "The file path of the watch executable that is used to monitor file system changes."
                    <> metavar "WATH_PATH")
