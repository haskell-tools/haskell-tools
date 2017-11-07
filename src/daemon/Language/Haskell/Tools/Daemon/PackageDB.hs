{-# LANGUAGE DeriveGeneric
           , ScopedTypeVariables
           , MultiWayIf
           #-}
-- | Setting the package database to use when compiling modules. The daemon must have one single
-- package database that cannot be changed after a package is loaded using that package database.
-- Available package databases are the cabal global, the cabal sandbox, the stack or one that had
-- been explicitely set by a file path.
module Language.Haskell.Tools.Daemon.PackageDB (PackageDB(..), decidePkgDB, packageDBLoc, detectAutogen) where

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
data PackageDB = DefaultDB -- ^ Use the global cabal package database (like when using ghc).
               | CabalSandboxDB -- ^ Use the sandboxed cabal package database.
               | StackDB -- ^ Use the stack package databases (local and snapshot).
               | ExplicitDB { packageDBPath :: [FilePath] } -- ^ Set the package database explicitely.
  deriving (Eq, Show, Generic)

instance FromJSON PackageDB

-- | Decide which type of project we are dealing with based on the package folders.
-- Should only be invoked if the user did not select the project-type.
decidePkgDB :: [FilePath] -> IO (Maybe PackageDB)
decidePkgDB [] = return Nothing
decidePkgDB (firstRoot:packageRoots) = do
  fstRes <- decidePkgDB' firstRoot
  res <- mapM decidePkgDB' packageRoots
  if any (fstRes /=) res || (fstRes == CabalSandboxDB && not (null res))
    then return Nothing
    else return (Just fstRes)

decidePkgDB' :: FilePath -> IO PackageDB
decidePkgDB' root = do isSandbox <- checkSandbox
                       if isSandbox then return CabalSandboxDB
                                  else do isStack <- checkStack
                                          if isStack then return StackDB
                                                       else return DefaultDB
  where
    checkStack =
      withCurrentDirectory root $ (fmap $ either (\(_ :: SomeException) -> False) id) $ try $ do
        projRoot <- runCommandExpectOK "stack path --allow-different-user --project-root"
        absPath <- canonicalizePath root
        -- we only accept stack projects where the packages are (direct or indirect) subdirectories of the project root
        return $ maybe False (`isPrefixOf` absPath) projRoot
    checkSandbox = do
      hasConfigFile <- doesFileExist (root </> "cabal.config")
      hasSandboxFile <- doesFileExist (root </> "cabal.sandbox.config")
      return $ hasConfigFile || hasSandboxFile

-- | Finds the location of the package database based on the configuration.
packageDBLoc :: PackageDB -> FilePath -> IO [FilePath]
packageDBLoc DefaultDB _ = do
  dbs <- runCommandExpectOK "ghc-pkg list base"
  return $ maybe [] (filter (\l -> not (null l) && not (" " `isPrefixOf` l)) . lines) dbs
packageDBLoc CabalSandboxDB path = do
  hasConfigFile <- doesFileExist (path </> "cabal.config")
  config <- if hasConfigFile then readFile (path </> "cabal.config")
              else readFile (path </> "cabal.sandbox.config")
  return $ map (drop (length "package-db: ")) $ filter ("package-db: " `isPrefixOf`) $ lines config
packageDBLoc StackDB path = withCurrentDirectory path $ do
   -- TODO: group the 3 calls into one for speed, split the output
   globalDB <- runCommandExpectOK "stack path --allow-different-user --global-pkg-db"
   snapshotDB <- runCommandExpectOK "stack path --allow-different-user --snapshot-pkg-db"
   localDB <- runCommandExpectOK "stack path --allow-different-user --local-pkg-db"
   return $ maybeToList localDB ++ maybeToList snapshotDB ++ maybeToList globalDB
packageDBLoc (ExplicitDB dirs) path = return dirs

-- | Gets the (probable) location of autogen folder depending on which type of
-- build we are using.
detectAutogen :: FilePath -> PackageDB -> IO (Maybe FilePath)
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

-- take the first nonempty
choose :: (Eq (f a), Alternative f) => [f a] -> f a
choose = fromMaybe empty . find (/= empty)

ifExists :: FilePath -> IO (Maybe FilePath)
ifExists fp = do exists <- doesDirectoryExist fp
                 if exists then return (Just fp)
                           else return Nothing
