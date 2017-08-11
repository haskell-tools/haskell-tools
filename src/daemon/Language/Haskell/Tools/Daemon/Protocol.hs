{-# LANGUAGE DeriveGeneric
           , OverloadedStrings
           #-}
-- | This module declares the messages that can be sent from the client to the
-- daemon engine and from the engine to the client.
module Language.Haskell.Tools.Daemon.Protocol where

import qualified Data.Aeson as A ((.=))
import Data.Aeson hiding ((.=))
import GHC.Generics (Generic)

import FastString (unpackFS)
import SrcLoc

import Language.Haskell.Tools.Daemon.PackageDB (PackageDB)

-- | The messages expected from the client.
data ClientMessage
  = KeepAlive -- ^ A simple ping message to check that the server is running.
  | Handshake { clientVersion :: [Int] }
    -- ^ Tells the client version and asks the servers version.
  | SetPackageDB { pkgDB :: PackageDB }
    -- ^ Sets the package database for the engine to use.
  | AddPackages { addedPathes :: [FilePath] }
    -- ^ Registers packages to the engine. They will be subject to subsequent
    -- refactorings. Will cause the packages to be loaded, resulting in
    -- LoadingModules, LoadedModules or CompilationProblem responses.
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
                       , details :: [String]
                       , shutdownAfter :: Bool
                       }
    -- ^ Orders the engine to perform the refactoring on the module given
    -- with the selection and details. Successful refactorings respond with
    -- ModulesChanged. If shutdownAfter is not specified, after the refactoring,
    -- modules are re-loaded, LoadingModules, LoadedModules responses are sent.
  | Disconnect
    -- ^ Stops the engine. It replies with Disconnected.
  | ReLoad { addedModules :: [FilePath]
           , changedModules :: [FilePath]
           , removedModules :: [FilePath]
           }
    -- ^ Instructs the engine to re-load a changed module.
    -- LoadingModules, LoadedModules responses may be sent.
  | Stop
    -- ^ Stops the server. OBSOLATE
  deriving (Show, Generic)

instance FromJSON ClientMessage

data ResponseMsg
  = KeepAliveResponse -- ^ A response to KeepAlive
  | HandshakeResponse { serverVersion :: [Int] }
    -- ^ Tells the version of the server.
  | ErrorMessage { errorMsg :: String }
    -- ^ An error message marking internal problems or user mistakes.
    -- TODO: separate internap problems and user mistakes.
  | CompilationProblem { errorMarkers :: [(SrcSpan, String)] }
    -- ^ A response that tells there are errors in the source code given.
  | ModulesChanged { undoChanges :: [UndoRefactor] }
    -- ^ The refactoring succeeded. The information to undo the changes is sent.
  | LoadingModules { modulesToLoad :: [FilePath] }
    -- ^ The traversal of the project is done, now the engine is loading the
    -- given modules.
  | LoadedModules { loadedModules :: [(FilePath, String)] }
    -- ^ The engine has loaded the given module.
  | UnusedFlags { unusedFlags :: [String] }
    -- ^ Returns the flags that are not used by the engine.
  | Disconnected
    -- ^ The engine has closed the connection.
  deriving (Show, Generic)

instance ToJSON ResponseMsg

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
  deriving (Show, Generic)

instance ToJSON UndoRefactor

type FileDiff = [(Int, Int, String)]
