{-# LANGUAGE DeriveGeneric #-}
module Language.Haskell.Tools.Refactor.Daemon.PackageDB where

import Data.Aeson hiding ((.=))
import System.FilePath
import System.Directory
import System.Process
import System.IO
import GHC.Generics
import Control.Applicative
import Control.Monad.IO.Class
import Data.List
import Data.Char (isSpace)
import Data.Maybe
import Data.List.Split
import Data.Function (on)

data PackageDB = AutoDB
               | DefaultDB
               | CabalSandboxDB
               | StackDB
               | ExplicitDB { packageDBPath :: FilePath }
  deriving (Show, Generic)

instance FromJSON PackageDB

packageDBLocs :: PackageDB -> [FilePath] -> IO [FilePath]
packageDBLocs pack = fmap concat . mapM (packageDBLoc pack) 

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
     (_, snapshotDB, snapshotDBErrs) <- readProcessWithExitCode "stack" ["path", "--snapshot-pkg-db"] ""
     (_, localDB, localDBErrs) <- readProcessWithExitCode "stack" ["path", "--local-pkg-db"] ""
     return $ [trim localDB | null localDBErrs] ++ [trim snapshotDB | null snapshotDBErrs]
packageDBLoc (ExplicitDB dir) path = do
  hasDir <- doesDirectoryExist (path </> dir)
  if hasDir then return [path </> dir]
            else return []

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace