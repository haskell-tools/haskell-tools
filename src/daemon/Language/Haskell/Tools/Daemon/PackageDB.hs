{-# LANGUAGE DeriveGeneric
           , ScopedTypeVariables
           , MultiWayIf
           #-}
-- | Setting the package database to use when compiling modules. The daemon must have one single
-- package database that cannot be changed after a package is loaded using that package database.
-- Available package databases are the cabal global, the cabal sandbox, the stack or one that had
-- been explicitely set by a file path.
module Language.Haskell.Tools.Daemon.PackageDB (PackageDB(..), packageDBLoc, detectAutogen) where

import Control.Applicative ((<$>), Alternative(..))
import Control.Exception (SomeException, try)
import Control.Monad
import Data.Aeson (FromJSON(..))
import Data.Char (isSpace)
import Data.List
import Data.Maybe
import GHC.Generics (Generic(..))
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath (FilePath, (</>))
import System.Process (shell, readCreateProcessWithExitCode)

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
packageDBLoc AutoDB path = concat <$> sequence [ packageDBLoc StackDB path
                                               , packageDBLoc CabalSandboxDB path
                                               , packageDBLoc DefaultDB path
                                               ]
packageDBLoc DefaultDB _ = do
  dbs <- runCommandExpectOK "ghc-pkg list base"
  return $ maybe [] (filter (\l -> not (null l) && not (" " `isPrefixOf` l)) . lines) dbs
packageDBLoc CabalSandboxDB path = do
  hasConfigFile <- doesFileExist (path </> "cabal.config")
  hasSandboxFile <- doesFileExist (path </> "cabal.sandbox.config")
  config <- if hasConfigFile then readFile (path </> "cabal.config")
              else if hasSandboxFile then readFile (path </> "cabal.sandbox.config")
                                     else return ""
  return $ map (drop (length "package-db: ")) $ filter ("package-db: " `isPrefixOf`) $ lines config
packageDBLoc StackDB path = withCurrentDirectory path $ (fmap $ either (\(_ :: SomeException) -> []) id) $ try $ do
     projRoot <- runCommandExpectOK "stack path --allow-different-user --project-root"
     -- we only accept stack projects where the packages are (direct or indirect) subdirectories of the project root
     if maybe False (`isPrefixOf` path) projRoot then do
       globalDB <- runCommandExpectOK "stack path --allow-different-user --global-pkg-db"
       snapshotDB <- runCommandExpectOK "stack path --allow-different-user --snapshot-pkg-db"
       localDB <- runCommandExpectOK "stack path --allow-different-user --local-pkg-db"
       return $ maybeToList localDB ++ maybeToList snapshotDB ++ maybeToList globalDB
     else return []
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
    distDir <- runCommandExpectOK "stack path --allow-different-user --dist-dir"
    return $ trim (fromMaybe "" distDir)
  genExists <- doesDirectoryExist (root </> dir </> "build" </> "autogen")
  buildExists <- doesDirectoryExist (root </> dir </> "build")
  if | genExists -> return $ Just (root </> dir </> "build" </> "autogen")
     | buildExists -> do -- for some packages, the autogen folder is inside a folder named after the package
                         cont <- filterM doesDirectoryExist . map ((root </> dir </> "build") </>)
                                   =<< listDirectory (root </> dir </> "build")
                         existing <- mapM ifExists (map (</> "autogen") cont)
                         return $ choose existing
     | otherwise -> return Nothing

-- | Run a command and return its result if successful display an error message otherwise.
runCommandExpectOK :: String -> IO (Maybe String)
runCommandExpectOK cmd = do
  (exitCode, res, errs) <- readCreateProcessWithExitCode (shell cmd) ""
  case exitCode of ExitSuccess -> return (Just $ trim res)
                   ExitFailure code -> do putStrLn ("The command '" ++ cmd ++ "' exited with "
                                                      ++ show code ++ ":\n" ++ errs)
                                          return Nothing

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

choose :: Alternative f => [f a] -> f a
choose = foldl (<|>) empty

ifExists :: FilePath -> IO (Maybe FilePath)
ifExists fp = do exists <- doesDirectoryExist fp
                 if exists then return (Just fp)
                           else return Nothing
