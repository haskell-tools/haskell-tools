module Language.Haskell.Tools.Daemon.Options
         (DaemonOptions(..), parseDaemonCLI, SharedDaemonOptions(..), sharedOptionsParser) where

import Data.Semigroup ((<>))
import Options.Applicative

-- | Command line options for the daemon process.
data DaemonOptions = DaemonOptions { daemonVersion :: Bool
                                   , portNumber :: Int
                                   , silentMode :: Bool
                                   , sharedOptions :: SharedDaemonOptions
                                   }

-- | Command line options shared by CLI and daemon.
data SharedDaemonOptions = SharedDaemonOptions { noWatch :: Bool
                                               , watchExe :: Maybe FilePath
                                               , generateCode :: Bool
                                               , disableHistory :: Bool
                                               }

parseDaemonCLI = execParser daemonCLI

daemonCLI = info (daemonOptionsParser <**> helper)
                 (fullDesc
                   <> progDesc "Start the Haskell-tools daemon process"
                   <> header "ht-daemon: a background process for Haskell development tools.")

daemonOptionsParser :: Parser DaemonOptions
daemonOptionsParser = DaemonOptions <$> version <*> port <*> silent <*> sharedOptionsParser
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

sharedOptionsParser :: Parser SharedDaemonOptions
sharedOptionsParser = SharedDaemonOptions <$> noWatch <*> watch <*> generateCode <*> noHistory
  where noWatch = switch (long "no-watch"
                            <> help "Disables file system watching.")
        watch = optional . strOption
                  $ long "watch-exe"
                      <> short 'w'
                      <> help "The file path of the watch executable that is used to monitor file system changes."
                      <> metavar "WATH_PATH"
        generateCode = switch (long "generate-code"
                                 <> help "Always generate code for the modules of the loaded project.")
        noHistory = switch (long "no-history"
                              <> help "Disables saving the performed refactorings.")
