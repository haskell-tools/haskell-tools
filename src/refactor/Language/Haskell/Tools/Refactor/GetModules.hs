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
getModules :: FilePath -> IO [([FilePath], [String])]
getModules root
  = do files <- listDirectory root
       case find (\p -> takeExtension p == ".cabal") files of
          Just cabalFile -> modulesFromCabalFile (root </> cabalFile)
          Nothing        -> do mods <- modulesFromDirectory root root
                               return [([root], mods)]


modulesFromCabalFile :: FilePath -> IO [([FilePath], [String])]
-- now adding all conditional entries, regardless of flags
modulesFromCabalFile cabal = getModules . flattenPackageDescription <$> readPackageDescription silent cabal
  where getModules :: PackageDescription -> [([FilePath], [String])]
        getModules pkg = map (\(bi, mods) -> ( map (normalise . (takeDirectory cabal </>)) $ hsSourceDirs bi
                                             , map (concat . intersperse "." . components) mods) )
                           $ maybe [] ((:[]) . libRecord) (library pkg) ++ map exeRecord (executables pkg) 
                               ++ map testRecord (testSuites pkg) ++ map benchRecord (benchmarks pkg) 
        libRecord lib = (libBuildInfo lib, libModules lib)
        exeRecord exe = (buildInfo exe, exeModules exe)
        testRecord test = (testBuildInfo test, testModules test)
        benchRecord bench = (benchmarkBuildInfo bench, benchmarkModules bench)

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

srcDirFromRoot :: FilePath -> String -> FilePath
srcDirFromRoot fileName "" = fileName 
srcDirFromRoot fileName moduleName 
  = srcDirFromRoot (takeDirectory fileName) (dropWhile (/= '.') $ dropWhile (== '.') moduleName)
