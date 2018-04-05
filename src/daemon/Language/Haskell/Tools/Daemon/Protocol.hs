{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module declares the messages that can be sent from the client to the
-- daemon engine and from the engine to the client.
module Language.Haskell.Tools.Daemon.Protocol where

import Control.DeepSeq (NFData)
import qualified Data.Aeson as A ((.=))
import Data.Aeson hiding ((.=))
import GHC.Generics (Generic)

import FastString (unpackFS)
import SrcLoc

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Daemon.PackageDB (PackageDB)

-- | The messages expected from the client.
data ClientMessage
  = KeepAlive -- ^ A simple ping message to check that the server is running.
  | Reset -- ^ A message that instructs the server to reset its internal state and re-load loaded packages.
  | Handshake { clientVersion :: [Int] }
    -- ^ Tells the client version and asks the servers version.
  | SetPackageDB { pkgDB :: PackageDB }
    -- ^ Sets the package database for the engine to use.
  | AddPackages { addedPathes :: [FilePath] }
    -- ^ Registers packages to the engine. They will be subject to subsequent
    -- refactorings. Will cause the packages to be loaded, resulting in
    -- LoadingModules, LoadedModule or CompilationProblem responses.
  | RemovePackages { removedPathes :: [FilePath] }
    -- ^ Deregisters the given packages from the engine. They will not be
    -- subjects of further refactorings.
  | SetWorkingDir { newWorkingDir :: FilePath }
    -- ^ Sets the working directory for the compilation. Important when
    -- compiling code that loads resources based on relative pathes.
  | SetGHCFlags { ghcFlags :: [String] }
    -- ^ Sets the compilation flags. The unused flags are returned via the
    -- UnusedFlags response.
  | PerformRefactoring { refactoring :: String
                       , modulePath :: FilePath
                       , editorSelection :: String
                       , details :: [String] -- ^ Additional details for the refactoring like the
                                             -- names of generated definitions.
                       , shutdownAfter :: Bool -- ^ Stop the daemon after performing the refactoring.
                       , diffMode :: Bool -- ^ Don't change the files, send back the result as
                                          -- a unified diff.
                       }
    -- ^ Orders the engine to perform the refactoring on the module given
    -- with the selection and details. Successful refactorings will cause re-loading of modules.
    -- If 'shutdownAfter' or 'diffMode' is not set, after the refactoring,
    -- modules are re-loaded, LoadingModules, LoadedModule responses are sent.
  | PerformQuery { query :: String
                 , modulePath :: FilePath
                 , editorSelection :: String
                 , details :: [String] -- ^ Additional details for the refactoring like the
                                       -- names of generated definitions.
                 , shutdownAfter :: Bool -- ^ Stop the daemon after performing the query.
                 }
    -- ^ Orders the engine to perform a query on the module given with the selection and details.
  | UndoLast
    -- ^ Asks the daemon to undo the last refactoring.
  | Disconnect
    -- ^ Stops the engine. It replies with Disconnected.
  | ReLoad { addedModules :: [FilePath]
           , changedModules :: [FilePath]
           , removedModules :: [FilePath]
           }
    -- ^ Instructs the engine to re-load a changed module.
    -- LoadingModules, LoadedModule responses may be sent.
  | Stop -- TODO: remove
    -- ^ Stops the server. OBSOLATE
  deriving (Show, Generic)

instance FromJSON ClientMessage

-- | The possible responses that the server can give.
data ResponseMsg
  = KeepAliveResponse -- ^ A response to KeepAlive
  | HandshakeResponse { serverVersion :: [Int] }
    -- ^ Tells the version of the server.
  | ErrorMessage { errorMsg :: String }
    -- ^ An error message marking internal problems or user mistakes.
    -- TODO: separate internal problems and user mistakes.
  | CompilationProblem { markers :: [Marker]
                       , errorHints :: [String]
                       }
    -- ^ A response that tells there are errors in the source code given.
  | DiffInfo { diffInfo :: String }
    -- ^ Information about changes that would be caused by the refactoring.
  | LoadingModules { modulesToLoad :: [FilePath] }
    -- ^ The traversal of the project is done, now the engine is loading the
    -- given modules.
  | LoadedModule { loadedModulePath :: FilePath
                 , loadedModuleName :: String
                 }
    -- ^ The engine has loaded the given module.
  | QueryResult { queryResult :: Value }
    -- ^ The result of querying the program representation.
  | UnusedFlags { unusedFlags :: [String] }
    -- ^ Returns the flags that are not used by the engine.
  | Disconnected
    -- ^ The engine has closed the connection.
  deriving (Show, Generic)

data Marker = Marker { location :: SrcSpan
                     , severity :: Severity
                     , message :: String
                     } deriving (Generic, Eq)

instance Show Marker where
  show marker = show (severity marker) ++ " at " ++ shortShowSpanWithFile (location marker) ++ ": " ++ message marker

data Severity = Error | Warning | Info
  deriving (Show, Generic, Eq)

instance ToJSON ResponseMsg
instance ToJSON Marker
instance ToJSON Severity

instance ToJSON SrcSpan where
  toJSON (RealSrcSpan sp) = object [ "file" A..= unpackFS (srcSpanFile sp)
                                   , "startRow" A..= srcLocLine (realSrcSpanStart sp)
                                   , "startCol" A..= srcLocCol (realSrcSpanStart sp)
                                   , "endRow" A..= srcLocLine (realSrcSpanEnd sp)
                                   , "endCol" A..= srcLocCol (realSrcSpanEnd sp)
                                   ]
  toJSON _ = Null

data UndoRefactor = RemoveAdded { undoRemovePath :: FilePath }
                  | RestoreRemoved { undoRestorePath :: FilePath
                                   , undoRestoreContents :: String
                                   }
                  | UndoChanges { undoChangedPath :: FilePath
                                , undoDiff :: FileDiff
                                }
  deriving (Show, Generic, NFData)

instance ToJSON UndoRefactor

type FileDiff = [(Int, Int, String)]
