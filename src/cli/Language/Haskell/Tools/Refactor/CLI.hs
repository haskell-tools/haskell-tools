{-# LANGUAGE LambdaCase
           , TupleSections
           , FlexibleContexts
           , TemplateHaskell
           , TypeFamilies
           , StandaloneDeriving
           #-}
module Language.Haskell.Tools.Refactor.CLI
  (refactorSession, normalRefactorSession, tryOut) where

import Control.Concurrent
import Control.Monad.State.Strict
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Version (showVersion)
import System.Directory
import System.IO

import Language.Haskell.Tools.Daemon
import Language.Haskell.Tools.Daemon.Mode (channelMode)
import Language.Haskell.Tools.Daemon.Protocol
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin
import Paths_haskell_tools_cli (version)

tryOut :: IO ()
tryOut = void $ normalRefactorSession builtinRefactorings stdin stdout
                  [ "-exec=OrganizeImports src\\ast\\Language\\Haskell\\Tools\\AST.hs"
                  , "src/ast", "src/backend-ghc", "src/prettyprint", "src/rewrite", "src/refactor"]

type ServerInit = MVar (Chan ResponseMsg, Chan ClientMessage) -> IO ()

normalRefactorSession :: [RefactoringChoice IdDom] -> Handle -> Handle -> [String] -> IO Bool
normalRefactorSession refactorings = refactorSession refactorings (\st -> void $ forkIO $ runDaemon refactorings channelMode st [])

refactorSession :: [RefactoringChoice IdDom] -> ServerInit -> Handle -> Handle -> [String] -> IO Bool
refactorSession _ _ _ output args
  | "-v" `elem` args = do
    hPutStrLn output $ showVersion version
    return True
refactorSession refactorings init input output args = do
  connStore <- newEmptyMVar
  isInteractive <- newEmptyMVar
  init connStore
  (recv,send) <- takeMVar connStore -- wait for the server to establish connection
  wd <- getCurrentDirectory
  writeChan send (SetWorkingDir wd)
  -- TODO: separate cmd arguments here instead of in daemon
  writeChan send (SetGHCFlags args)
  forkIO $ forever $ do interactive <- takeMVar isInteractive
                        when interactive (processUserInput refactorings input output send)
  readFromSocket refactorings output isInteractive recv send

processUserInput :: [RefactoringChoice IdDom] -> Handle -> Handle -> Chan ClientMessage -> IO ()
processUserInput refactorings input output chan = do
  cmd <- hGetLine input
  continue <- processCommand refactorings output chan cmd
  when continue $ processUserInput refactorings input output chan

processCommand :: [RefactoringChoice IdDom] -> Handle -> Chan ClientMessage -> String -> IO Bool
processCommand refactorings output chan cmd = do
  case splitOn " " cmd of
    ["Exit"] -> writeChan chan Disconnect >> return False
    ref : rest | let modPath:selection:details = rest ++ (replicate (2 - length rest) "")
               , ref `elem` refactorCommands refactorings
       -> writeChan chan (PerformRefactoring ref modPath selection details) >> return True
    _ -> do liftIO $ hPutStrLn output $ "'" ++ cmd ++ "' is not a known command. Commands are: Exit, "
                                            ++ intercalate ", " (refactorCommands refactorings)
            return True

readFromSocket :: [RefactoringChoice IdDom] -> Handle -> MVar Bool -> Chan ResponseMsg -> Chan ClientMessage -> IO Bool
readFromSocket refactorings output isInteractive recv send = do
  continue <- readChan recv >>= processMessage refactorings output isInteractive send
  maybe (readFromSocket refactorings output isInteractive recv send) return continue

-- | Returns Nothing if the execution should continue, Just False on erronous termination
-- and Just True on normal termination.
processMessage :: [RefactoringChoice IdDom] -> Handle -> MVar Bool -> Chan ClientMessage -> ResponseMsg -> IO (Maybe Bool)
processMessage _ output _ _ (ErrorMessage msg) = hPutStrLn output msg >> return (Just False)
processMessage _ output _ _ (CompilationProblem marks) = hPutStrLn output (show marks) >> return Nothing
processMessage _ output _ _ (LoadedModules mods)
  = mapM (\(fp,name) -> hPutStrLn output $ "Loaded module: " ++ name ++ "( " ++ fp ++ ") ") mods >> return Nothing
processMessage refactorings output isInteractive chan (UnusedFlags flags)
  = do loadModules chan flags
       performCmdOptions refactorings output isInteractive chan flags
       return Nothing
processMessage _ _ _ _ Disconnected = return (Just True)
processMessage _ _ _ _ _ = return Nothing

loadModules :: Chan ClientMessage -> [String] -> IO ()
loadModules chan flags = writeChan chan (AddPackages roots)
  where roots = filter (not . ("-" `isPrefixOf`)) flags

performCmdOptions :: [RefactoringChoice IdDom] -> Handle -> MVar Bool -> Chan ClientMessage -> [String] -> IO Bool
performCmdOptions refactorings output isInteractive chan flags = do
  mapM_ (processCommand refactorings output chan) cmds
  putMVar isInteractive (null cmds)
  when (not $ null cmds) $ writeChan chan Disconnect
  return True
  where cmds = catMaybes $ map (\f -> case splitOn "=" f of ["-exec", mod] -> Just mod; _ -> Nothing) flags
