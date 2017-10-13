{-# LANGUAGE DeriveGeneric
           , ScopedTypeVariables
           , MultiWayIf
           #-}
-- | Setting the package database to use when compiling modules. The daemon must have one single
-- package database that cannot be changed after a package is loaded using that package database.
-- Available package databases are the cabal global, the cabal sandbox, the stack or one that had
-- been explicitely set by a file path.
module Language.Haskell.Tools.Daemon.PackageDB (PackageDB(..), packageDBLoc, detectAutogen) where

import Control.Applicative (Applicative(..), (<$>), Alternative(..))
import Control.Exception (SomeException, try)
import Control.Monad
import Data.Aeson (FromJSON(..))
import Data.Char (isSpace)
import Data.List
import GHC.Generics (Generic(..))
import System.Directory
import System.FilePath (FilePath, (</>))
import System.Process (readProcessWithExitCode)

-- | Possible package database configurations.
data PackageDB = AutoDB -- ^ Decide the package database automatically.
               | DefaultDB -- ^ Use the global cabal package database (like when using ghc).
               | CabalSandboxDB -- ^ Use the sandboxed cabal package database.
               | StackDB -- ^ Use the stack package databases (local and snapshot).
               | ExplicitDB { packageDBPath :: FilePath } -- ^ Set the package database explicitely.
  deriving (Show, Generic)

instance FromJSON PackageDB

-- | Finds the location of the package database based on the configuration.
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
packageDBLoc StackDB path = withCurrentDirectory path $ (fmap $ either (\(_ :: SomeException) -> []) id) $ try $ do
     (_, globalDB, globalDBErrs) <- readProcessWithExitCode "stack" ["path", "--allow-different-user", "--global-pkg-db"] ""
     (_, snapshotDB, snapshotDBErrs) <- readProcessWithExitCode "stack" ["path", "--allow-different-user", "--snapshot-pkg-db"] ""
     (_, localDB, localDBErrs) <- readProcessWithExitCode "stack" ["path", "--allow-different-user", "--local-pkg-db"] ""
     return $ [trim localDB | null localDBErrs] ++ [trim snapshotDB | null snapshotDBErrs] ++ [trim globalDB | null globalDBErrs]
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
detectAutogen root StackDB = (fmap $ either (\(_ :: SomeException) -> Nothing) id) $ try $ do
  dir <- withCurrentDirectory root $ do
    (_, distDir, distDirErrs) <- readProcessWithExitCode "stack" ["path", "--allow-different-user", "--dist-dir"] ""
    when (not $ null distDirErrs)  -- print errors if they occurred
      $ putStrLn $ "Errors while checking dist directory with stack: " ++ distDirErrs
    return $ trim distDir
  genExists <- doesDirectoryExist (root </> dir </> "build" </> "autogen")
  buildExists <- doesDirectoryExist (root </> dir </> "build")
  if | genExists -> return $ Just (root </> dir </> "build" </> "autogen")
     | buildExists -> do -- for some packages, the autogen folder is inside a folder named after the package
                         cont <- filterM doesDirectoryExist . map ((root </> dir </> "build") </>)
                                   =<< listDirectory (root </> dir </> "build")
                         existing <- mapM ifExists (map (</> "autogen") cont)
                         return $ choose existing
     | otherwise -> return Nothing

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

choose :: Alternative f => [f a] -> f a
choose = foldl (<|>) empty

ifExists :: FilePath -> IO (Maybe FilePath)
ifExists fp = do exists <- doesDirectoryExist fp
                 if exists then return (Just fp)
                           else return Nothing
