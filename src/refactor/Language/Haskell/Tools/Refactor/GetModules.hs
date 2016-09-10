module Language.Haskell.Tools.Refactor.GetModules where

import Data.List (intersperse, find)
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
getModules root
  = do files <- listDirectory root
       case find (\p -> takeExtension p == ".cabal") files of
          Just cabalFile -> modulesFromCabalFile (root </> cabalFile)
          Nothing        -> modulesFromDirectory root root


modulesFromCabalFile :: FilePath -> IO [String]
-- now adding all conditional entries, regardless of flags
modulesFromCabalFile cabal = getModules . flattenPackageDescription <$> readPackageDescription silent cabal
  where getModules pkg = map (concat . intersperse "." . components) 
                           $ maybe [] libModules (library pkg) 
                               ++ concatMap exeModules (executables pkg) 
                               ++ concatMap testModules (testSuites pkg) 
                               ++ concatMap benchmarkModules (benchmarks pkg) 

modulesFromDirectory :: FilePath -> FilePath -> IO [String]
-- now recognizing only .hs files
modulesFromDirectory root searchRoot = concat <$> (mapM goOn =<< listDirectory searchRoot)
  where goOn fp = let path = searchRoot </> fp 
                   in do isDir <- doesDirectoryExist path  
                         if isDir
                           then modulesFromDirectory root path 
                           else if takeExtension path == ".hs" 
                                  then return [concat $ intersperse "." $ splitDirectories $ dropExtension $ makeRelative root path] 
                                  else return []
