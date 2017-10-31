module Main where

import Control.Monad (Monad(..), (=<<))
import Control.Monad.Reader (MonadReader(..))
import Data.List (init)
import Data.Semigroup ((<>))
import Options.Applicative
import Options.Applicative.Types (Parser, ReadM(..))
import System.Exit (exitSuccess, exitFailure)
import System.IO (IO, stdout, stdin)

import Language.Haskell.Tools.Daemon.Options (sharedOptionsParser)
import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings)
import Language.Haskell.Tools.Refactor.CLI (normalRefactorSession, CLIOptions(..))

main :: IO ()
main = exit =<< normalRefactorSession builtinRefactorings stdin stdout =<< execParser opts
  where exit :: Bool -> IO ()
        exit True = exitSuccess
        exit False = exitFailure
        opts = info (cliOptions <**> helper)
                    (fullDesc
                      <> progDesc "Run a refactoring or open a session"
                      <> header "ht-refact: a command-line interface for Haskell-tools")

cliOptions :: Parser CLIOptions
cliOptions = CLIOptions <$> version <*> verb <*> oneShot <*> sharedOptionsParser
                        <*> packages
  where version = switch (long "version"
                            <> short 'v'
                            <> help "Show the version of this software")
        verb = switch (long "verbose"
                           <> help "Prints debugging information.")
        oneShot
          = optional $ strOption (long "execute"
                                   <> short 'e'
                                   <> metavar "COMMAND"
                                   <> help "Commands to execute in a one-shot refactoring run, separated by semicolons.")
        packages = many $ strArgument (metavar "PACKAGE_ROOT"
                                        <> help "The root folder of packages that are refactored")
