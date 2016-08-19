module Language.Haskell.Tools.Refactor.GetModules where

import Data.List (intersperse)
import Distribution.Verbosity (silent)
import Distribution.ModuleName (components)
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import System.FilePath.Posix
import System.Directory

-- | Get modules of the project with the indicated root directory.
-- If there is a cabal file, it uses that, otherwise it just scans the directory recursively for haskell sourcefiles.
getModules :: FilePath -> IO [String]
getModules 
  = do files <- listDirectory root
       case find (\p -> takeExtension p == ".hs") files of
          Just cabalFile -> modulesFromCabalFile cabalFile
          Nothing        -> modulesFromDirectory


modulesFromCabalFile :: FilePath -> IO [String]
-- now omitting conditional entries
modulesFromCabalFile cabal = getModules . flattenPackageDescription <$> readPackageDescription silent cabal
  where getModules pkg = map (concat . intersperse "." . components) 
                           $ maybe [] libModules (library pkg) 
                               ++ concatMap exeModules (executables pkg) 
                               ++ concatMap testModules (testSuites pkg) 
                               ++ concatMap benchmarkModules (benchmarks pkg) 

modulesFromDirectory :: FilePath -> IO [String]
-- now recognizing only .hs files
modulesFromDirectory root = concat <$> (mapM goOn =<< listDirectory root)
  where goOn fp = let path = root </> fp 
                   in do isDir <- doesDirectoryExist path  
                         if isDir
                           then modulesFromDirectory path 
                           else if takeExtension path == ".hs" 
                                  then return [concat $ intersperse "." $ splitDirectories $ dropExtension path] 
                                  else return []
