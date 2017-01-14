{-# LANGUAGE DeriveGeneric #-}
module Language.Haskell.Tools.Refactor.Daemon.PackageDB where

import Data.Aeson (FromJSON(..))
import Data.Char (isSpace)
import Data.List
import GHC.Generics (Generic(..))
import System.Directory (withCurrentDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath (FilePath(..), (</>))
import System.Process (readProcessWithExitCode)

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