module Language.Haskell.Tools.Daemon.Options
         (DaemonOptions(..), parseDaemonCLI, SharedDaemonOptions(..), sharedOptionsParser) where

import Control.Monad.Reader (MonadReader(..))
import Data.List.Split (splitOn)
import Data.Semigroup ((<>))
import Options.Applicative
import Options.Applicative.Types (Parser, ReadM(..))

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
                                               , ghcFlags :: Maybe [String]
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
sharedOptionsParser = SharedDaemonOptions <$> noWatch <*> watch <*> generateCode <*> noHistory <*> ghcFlags
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
        ghcFlags
          = optional $ option ghcFlagsParser
                         (long "ghc-options"
                           <> short 'g'
                           <> metavar "GHC_OPTIONS"
                           <> help "Flags passed to GHC when loading the packages, separated by spaces.")
          where ghcFlagsParser :: ReadM [String]
                ghcFlagsParser
                  = ReadM $ do str <- ask
                               let str' = case str of '=':'"':rest -> init rest
                                                      '=':rest     -> rest
                                                      '"':rest     -> init rest
                                                      other        -> other
                               return ( splitOn " " str')
