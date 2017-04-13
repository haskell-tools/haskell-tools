{-# LANGUAGE DeriveGeneric #-}
module Language.Haskell.Tools.Refactor.Daemon.PackageDB where

import Control.Applicative (Applicative(..), (<$>), Alternative(..))
import Control.Monad
import Data.Aeson (FromJSON(..))
import Data.Char (isSpace)
import Data.List
import GHC.Generics (Generic(..))
import System.Directory
import System.FilePath (FilePath, (</>))
import System.Process (readProcessWithExitCode)

data PackageDB = AutoDB
               | DefaultDB
               | CabalSandboxDB
               | StackDB
               | ExplicitDB { packageDBPath :: FilePath }
  deriving (Show, Generic)

instance FromJSON PackageDB

packageDBLoc :: PackageDB -> FilePath -> IO [FilePath]
packageDBLoc AutoDB path = (++) <$> packageDBLoc StackDB path <*> packageDBLoc CabalSandboxDB path
packageDBLoc DefaultDB _ = return []
packageDBLoc CabalSandboxDB path = do
  hasConfigFile <- doesFileExist (path </> "cabal.config")
  hasSandboxFile <- doesFileExist (path </> "cabal.sandbox.config")
  config <- if hasConfigFile then readFile (path </> "cabal.config")
              else if hasSandboxFile then readFile (path </> "cabal.sandbox.config")
                                     else return ""
  return $ map (drop (length "package-db: ")) $ filter ("package-db: " `isPrefixOf`) $ lines config
packageDBLoc StackDB path = withCurrentDirectory path $ do
     (_, snapshotDB, snapshotDBErrs) <- readProcessWithExitCode "stack" ["path", "--allow-different-user", "--snapshot-pkg-db"] ""
     (_, localDB, localDBErrs) <- readProcessWithExitCode "stack" ["path", "--allow-different-user", "--local-pkg-db"] ""
     return $ [trim localDB | null localDBErrs] ++ [trim snapshotDB | null snapshotDBErrs]
packageDBLoc (ExplicitDB dir) path = do
  hasDir <- doesDirectoryExist (path </> dir)
  if hasDir then return [path </> dir]
            else return []

-- | Gets the (probable) location of autogen folder depending on which type of
-- build we are using.
detectAutogen :: FilePath -> PackageDB -> IO (Maybe FilePath)
detectAutogen root AutoDB = do
  defDB <- detectAutogen root DefaultDB
  sandboxDB <- detectAutogen root CabalSandboxDB
  stackDB <- detectAutogen root StackDB
  return $ choose [ defDB, sandboxDB, stackDB ]
detectAutogen root DefaultDB = ifExists (root </> "dist" </> "build" </> "autogen")
detectAutogen root (ExplicitDB _) = ifExists (root </> "dist" </> "build" </> "autogen")
detectAutogen root CabalSandboxDB = ifExists (root </> "dist" </> "build" </> "autogen")
detectAutogen root StackDB = do
  distExists <- doesDirectoryExist (root </> ".stack-work" </> "dist")
  existing <- if distExists then (do
    contents <- listDirectory (root </> ".stack-work" </> "dist")
    let dirs = map ((root </> ".stack-work" </> "dist") </>) contents
    subDirs <- mapM (\d -> map (d </>) <$> listDirectory d) dirs
    mapM (ifExists . (</> "build" </> "autogen")) (dirs ++ concat subDirs)) else return []
  return (choose existing)


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

choose :: Alternative f => [f a] -> f a
choose = foldl (<|>) empty

ifExists :: FilePath -> IO (Maybe FilePath)
ifExists fp = do exists <- doesDirectoryExist fp
                 if exists then return (Just fp)
                           else return Nothing
