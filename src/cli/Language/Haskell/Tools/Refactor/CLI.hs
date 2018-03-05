{-# LANGUAGE FlexibleContexts
           , TypeFamilies
           , RecordWildCards
           , ScopedTypeVariables
           #-}
-- | The command line interface for Haskell-tools. It uses the Haskell-tools daemon package starting
-- the daemon in the same process and communicating with it through a channel.
-- It can be used in a one-shot mode, listing all actions in a command-line parameter or using its
-- standard input to perform a series of refactorings.
module Language.Haskell.Tools.Refactor.CLI
  (refactorSession, normalRefactorSession, CLIOptions(..), SharedDaemonOptions(..)) where

import Control.Concurrent
import Control.Exception (BlockedIndefinitelyOnMVar(..), catch)
import Control.Monad.State.Strict
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Version (showVersion)
import System.Directory (getCurrentDirectory)
import System.IO
import System.IO.Error (isEOFError)

import Language.Haskell.Tools.Daemon (runDaemon)
import Language.Haskell.Tools.Daemon.Mode (channelMode)
import Language.Haskell.Tools.Daemon.Options (SharedDaemonOptions(..), DaemonOptions(..))
import Language.Haskell.Tools.Daemon.Protocol (ResponseMsg(..), ClientMessage(..))
import Language.Haskell.Tools.Refactor
import Paths_haskell_tools_cli (version)
-- | Normal entry point of the cli.
normalRefactorSession :: [RefactoringChoice] -> [QueryChoice] -> Handle -> Handle -> CLIOptions -> IO Bool
normalRefactorSession refactorings queries input output options@CLIOptions{..}
  = do hSetBuffering stdout LineBuffering -- to synch our output with GHC's
       hSetBuffering stderr LineBuffering -- to synch our output with GHC's
       refactorSession refactorings queries
         (\st -> void $ forkIO $ do runDaemon refactorings queries channelMode st
                                      (DaemonOptions False 0 (not cliVerbose) sharedOptions))
         input output options

-- | Command-line options for the Haskell-tools CLI
data CLIOptions = CLIOptions { displayVersion :: Bool
                             , cliVerbose :: Bool
                             , executeCommands :: Maybe String
                             , sharedOptions :: SharedDaemonOptions
                             , packageRoots :: [FilePath]
                             } deriving Show

-- | Entry point with configurable initialization. Mainly for testing, call 'normalRefactorSession'
-- to use the command-line.
refactorSession :: [RefactoringChoice] -> [QueryChoice] -> ServerInit -> Handle -> Handle
                     -> CLIOptions -> IO Bool
refactorSession _ _ _ _ output CLIOptions{..} | displayVersion
  = do hPutStrLn output $ showVersion version
       return True
refactorSession refactorings queries init input output CLIOptions{..} = do
  connStore <- newEmptyMVar
  init connStore
  (recv,send) <- takeMVar connStore -- wait for the server to establish connection
  wd <- getCurrentDirectory
  writeChan send (SetWorkingDir wd)
  writeChan send (AddPackages packageRoots)
  case executeCommands of
    Just cmds -> performCmdOptions refactorings queries output send (splitOn ";" cmds)
    Nothing -> return ()
  when (isNothing executeCommands) (void $ forkIO $ processUserInput refactorings queries input output send)
  readFromSocket (isJust executeCommands) output recv

-- | An initialization action for the daemon.
type ServerInit = MVar (Chan ResponseMsg, Chan ClientMessage) -> IO ()

-- | Reads commands from standard input and executes them.
processUserInput :: [RefactoringChoice] -> [QueryChoice] -> Handle -> Handle
                      -> Chan ClientMessage -> IO ()
processUserInput refactorings queries input output chan = do
    cmd <- hGetLine input
    continue <- processCommand False refactorings queries output chan cmd
    when continue $ processUserInput refactorings queries input output chan
  `catch` \e -> if isEOFError e then return ()
                                else putStrLn (show e) >> return ()

-- | Perform a command received from the user. The resulting boolean states if the user may continue
-- (True), or the session is over (False).
processCommand :: Bool -> [RefactoringChoice] -> [QueryChoice] -> Handle -> Chan ClientMessage
                    -> String -> IO Bool
processCommand _ _ _ _ _ "" = return True
processCommand shutdown refactorings queries output chan cmd = do
  case splitOn " " cmd of
    ["Exit"] -> writeChan chan Disconnect >> return False
    ["AddFile", fn] -> writeChan chan (ReLoad [fn] [] []) >> return True
    ["ChangeFile", fn] -> writeChan chan (ReLoad [] [fn] []) >> return True
    ["RemoveFile", fn] -> writeChan chan (ReLoad [] [] [fn]) >> return True
    [cmd] | cmd `elem` ["AddFile", "ChangeFile", "RemoveFile"]
      -> hPutStrLn output (cmd ++ " needs one argument. None is given.") >> return False
    cmd:_ | cmd `elem` ["AddFile", "ChangeFile", "RemoveFile"]
      -> hPutStrLn output (cmd ++ " needs one argument. Too many arguments given.") >> return False
    ["Undo"] -> writeChan chan UndoLast >> return True
    ["Reset"] -> writeChan chan Reset >> return True -- undocumented feature
    ref : rest | let modPath:selection:details = rest ++ (replicate (2 - length rest) "")
               , ref `elem` refactorCommands refactorings
       -> do writeChan chan (PerformRefactoring ref modPath selection details shutdown False)
             return (not shutdown)
    ref : rest | let modPath:selection:details = rest ++ (replicate (2 - length rest) "")
               , ref `elem` queryCommands queries
       -> do writeChan chan (PerformQuery ref modPath selection details shutdown)
             return (not shutdown)
    "Try" : ref : rest | let modPath:selection:details = rest ++ (replicate (2 - length rest) "")
                       , ref `elem` refactorCommands refactorings
       -> do writeChan chan (PerformRefactoring ref modPath selection details shutdown True)
             return (not shutdown)
    ["Try"] -> hPutStrLn output "The 'Try' modifier requires a refactoring command specified to execute." >> return False
    _ -> do liftIO $ hPutStrLn output $ "'" ++ cmd ++ "' is not a known command. Commands are: Exit, Undo, AddFile, ChangeFile, RemoveFile, Try REFACTOR"
                                            ++ concat (map (", " ++) (refactorCommands refactorings))
            return True

-- | Read the responses of the daemon. The result states if the session exited normally or in an
-- erronous way.
readFromSocket :: Bool -> Handle -> Chan ResponseMsg -> IO Bool
readFromSocket pedantic output recv = do
    continue <- readChan recv >>= processMessage pedantic output
    maybe (readFromSocket pedantic output recv) return continue -- repeate if not stopping
  `catch` \(_ :: BlockedIndefinitelyOnMVar) -> return False -- other threads terminated

-- | Receives a single response from daemon. Returns Nothing if the execution should continue,
-- Just False on erronous termination and Just True on normal termination.
processMessage :: Bool -> Handle -> ResponseMsg -> IO (Maybe Bool)
processMessage _ output (ErrorMessage msg) = hPutStrLn output msg >> return (Just False)
processMessage pedantic output (CompilationProblem marks hints)
  = do mapM_ (hPutStrLn output) hints
       mapM_ (hPutStrLn output . show) marks
       return (if pedantic then Just False else Nothing)
processMessage _ output (LoadedModule fp name)
  = do hPutStrLn output $ "Loaded module: " ++ name ++ "( " ++ fp ++ ") "
       return Nothing
processMessage _ output (QueryResult value)
  = do hPutStrLn output $ "Query result: " ++ show value
       return Nothing
processMessage _ output (DiffInfo diff)
  = do hPutStrLn output diff
       return Nothing
processMessage _ output (LoadingModules mods)
  = do hPutStrLn output $ "Found modules: " ++ intercalate ", " mods
       return Nothing
processMessage _ output (UnusedFlags flags)
  = do hPutStrLn output $ "Warning: The following ghc-flags are not recognized: "
                             ++ intercalate " " flags
       return Nothing

processMessage _ _ Disconnected = return (Just True)
processMessage _ _ _ = return Nothing

-- | Perform the commands specified by the user as a command line argument.
performCmdOptions :: [RefactoringChoice] -> [QueryChoice] -> Handle -> Chan ClientMessage
                       -> [String] -> IO ()
performCmdOptions refactorings queries output chan cmds = do
    continue <- mapM (\(shutdown, cmd) -> processCommand shutdown refactorings
                                            queries output chan cmd)
                     (zip lastIsShutdown cmds)
    when (and continue) $ writeChan chan Disconnect
  where lastIsShutdown = replicate (length cmds - 1) False ++ [True]
