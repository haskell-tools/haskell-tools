{-# LANGUAGE TupleSections
           , NamedFieldPuns
           , LambdaCase
           , TemplateHaskell
           , FlexibleContexts
           , TypeApplications
           , RankNTypes
           #-}
-- | Collecting modules contained in a module collection (library, executable, testsuite or
-- benchmark). Gets names, source file locations, compilation and load flags for these modules.
module Language.Haskell.Tools.Daemon.GetModules where

import Control.Monad
import Control.Reference
import Data.Char
import Data.Function (on)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Distribution.Compiler
import Distribution.ModuleName
import Distribution.Package (Dependency(..), PackageName(..), pkgName)
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Verbosity (silent)
import Language.Haskell.Extension as Cabal
import System.Directory
import System.FilePath

import DynFlags
import qualified DynFlags as GHC
import GHC hiding (ModuleName)

import Language.Haskell.Tools.Daemon.MapExtensions
import Language.Haskell.Tools.Daemon.Representation

-- | Gets all ModuleCollections from a list of source directories. It also orders the source directories that are package roots so that
-- they can be loaded in the order they are defined (no backward imports). This matters in those cases because for them there can be
-- special compilation flags.
getAllModules :: [FilePath] -> IO [ModuleCollection ModuleNameStr]
getAllModules pathes = orderMCs . concat <$> mapM getModules (map normalise pathes)

-- | Sorts model collection in an order to remove all backward references.
-- Works because module collections defined by directories cannot be recursive.
orderMCs :: [ModuleCollection k] -> [ModuleCollection k]
orderMCs = sortBy compareMCs
  where compareMCs :: ModuleCollection k -> ModuleCollection k -> Ordering
        compareMCs mc _ | DirectoryMC _ <- (mc ^. mcId) = GT
        compareMCs _ mc | DirectoryMC _ <- (mc ^. mcId) = LT
        compareMCs mc1 mc2 | (mc2 ^. mcId) `elem` (mc1 ^. mcDependencies) = GT
        compareMCs mc1 mc2 | (mc1 ^. mcId) `elem` (mc2 ^. mcDependencies) = LT
        compareMCs _ _ = EQ


-- | Get modules of the project with the indicated root directory.
-- If there is a cabal file, it uses that, otherwise it just scans the directory recursively for haskell sourcefiles.
-- Only returns the non-boot haskell modules, the boot modules will be found during loading.
getModules :: FilePath -> IO [ModuleCollection ModuleNameStr]
getModules root
  = do files <- listDirectory root
       case find (\p -> takeExtension p == ".cabal") files of
          Just cabalFile -> modulesFromCabalFile root cabalFile
          Nothing        -> do mods <- modulesFromDirectory root root
                               return [ModuleCollection (DirectoryMC root) root [root] [] (modKeys mods) return return []]
  where modKeys mods = Map.fromList $ map (, ModuleNotLoaded False True) mods

-- | Load the module giving a directory. All modules loaded from the folder and subfolders.
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

-- | Load the module using a cabal file. The modules described in the cabal file will be loaded.
-- The flags and extensions set in the cabal file will be used by default.
modulesFromCabalFile :: FilePath -> FilePath -> IO [ModuleCollection ModuleNameStr]
-- now adding all conditional entries, regardless of flags
modulesFromCabalFile root cabal = (getModules . setupFlags <$> readPackageDescription silent (root </> cabal))
  where getModules pkg = maybe [] (maybe [] (:[]) . toModuleCollection pkg) (library pkg)
                           ++ catMaybes (map (toModuleCollection pkg) (executables pkg))
                           ++ catMaybes (map (toModuleCollection pkg) (testSuites pkg))
                           ++ catMaybes (map (toModuleCollection pkg) (benchmarks pkg))

        toModuleCollection :: ToModuleCollection tmc => PackageDescription -> tmc -> Maybe (ModuleCollection ModuleNameStr)
        toModuleCollection PackageDescription{ buildType = Just Custom } _
          = error "While parsing cabal file \"build-type: custom\" is not supported"
        toModuleCollection pkg tmc
          = let bi = getBuildInfo tmc
                packageName = pkgName $ package pkg
             in if buildable bi
                  then Just $ ModuleCollection (mkModuleCollKey packageName tmc)
                                root
                                (map (normalise . (root </>)) $ hsSourceDirs bi)
                                (map (\(mn, fs) -> (moduleName mn, fs)) $ getModuleSourceFiles tmc)
                                (Map.fromList $ map modRecord $ getModuleNames tmc)
                                (flagsFromBuildInfo bi)
                                (loadFlagsFromBuildInfo bi)
                                (map (\(Dependency pkgName _) -> LibraryMC (unPackageName pkgName)) (targetBuildDepends bi))
                  else Nothing
          where modRecord mn = ( moduleName mn, ModuleNotLoaded False (needsToCompile tmc mn) )
        moduleName = concat . intersperse "." . components
        setupFlags = either (\deps -> error $ "Missing dependencies: " ++ show deps) fst
                       . finalizePackageDescription [] (const True) buildPlatform
                                                    (unknownCompilerInfo buildCompilerId NoAbiTag) []

class ToModuleCollection t where
  mkModuleCollKey :: PackageName -> t -> ModuleCollectionId
  getBuildInfo :: t -> BuildInfo
  getModuleNames :: t -> [ModuleName]
  getModuleSourceFiles :: t -> [(ModuleName, FilePath)]
  getModuleSourceFiles _ = []
  needsToCompile :: t -> ModuleName -> Bool
  getMain :: t -> String
  getMain l = getMain' (getBuildInfo l)

instance ToModuleCollection Library where
  mkModuleCollKey pn _ = LibraryMC (unPackageName pn)
  getBuildInfo = libBuildInfo
  getModuleNames = libModules
  needsToCompile l m = m `elem` exposedModules l

instance ToModuleCollection Executable where
  mkModuleCollKey pn exe = ExecutableMC (unPackageName pn) (exeName exe)
  getBuildInfo = buildInfo
  getModuleNames exe = fromString (getMain exe) : exeModules exe
  needsToCompile exe mn = components mn == [getMain exe]
  getModuleSourceFiles exe = [(fromString (getMain exe), modulePath exe)]

instance ToModuleCollection TestSuite where
  mkModuleCollKey pn test = TestSuiteMC (unPackageName pn) (testName test)
  getBuildInfo = testBuildInfo
  getModuleNames exe = fromString (getMain exe) : testModules exe
  needsToCompile exe mn = components mn == [getMain exe]
  getModuleSourceFiles exe
    = case testInterface exe of
        TestSuiteExeV10 _ fp -> [(fromString (getMain exe), fp)]
        _ -> []
  getMain t = case testInterface t of
        TestSuiteLibV09 _ mod -> intercalate "." $ components mod
        _ -> getMain' (getBuildInfo t)

instance ToModuleCollection Benchmark where
  mkModuleCollKey pn test = BenchmarkMC (unPackageName pn) (benchmarkName test)
  getBuildInfo = benchmarkBuildInfo
  getModuleNames exe = fromString (getMain exe) : benchmarkModules exe
  needsToCompile exe mn = components mn == [getMain exe]
  getModuleSourceFiles exe
    = case benchmarkInterface exe of
        BenchmarkExeV10 _ fp -> [(fromString (getMain exe), fp)]
        _ -> []

getMain' :: BuildInfo -> String
getMain' bi
  = case ls of _:e:_ -> intercalate "." $ filter (isUpper . head) $ groupBy ((==) `on` (== '.')) e
               _ -> "Main"
  where ls = dropWhile (/= "-main-is") (concatMap snd (options bi))

isDirectoryMC :: ModuleCollection k -> Bool
isDirectoryMC mc = case mc ^. mcId of DirectoryMC{} -> True; _ -> False

compileInContext :: ModuleCollection k -> [ModuleCollection k] -> DynFlags -> IO DynFlags
compileInContext mc mcs dfs
  = (\dfs' -> applyDependencies mcs (mc ^. mcDependencies) (selectEnabled dfs'))
       <$> (mc ^. mcFlagSetup $ dfs)
  where selectEnabled = if isDirectoryMC mc then id else onlyUseEnabled

applyDependencies :: [ModuleCollection k] -> [ModuleCollectionId] -> DynFlags -> DynFlags
applyDependencies mcs ids dfs
  = dfs { GHC.packageFlags = GHC.packageFlags dfs ++ (catMaybes $ map (dependencyToPkgFlag mcs) ids) }

onlyUseEnabled :: DynFlags -> DynFlags
onlyUseEnabled = GHC.setGeneralFlag' GHC.Opt_HideAllPackages

dependencyToPkgFlag :: [ModuleCollection k] -> ModuleCollectionId -> Maybe (GHC.PackageFlag)
dependencyToPkgFlag mcs lib@(LibraryMC pkgName)
  = if isNothing $ find (\mc -> (mc ^. mcId) == lib) mcs
      then Just $ GHC.ExposePackage pkgName (GHC.PackageArg pkgName) (GHC.ModRenaming True [])
      else Nothing
dependencyToPkgFlag _ _ = Nothing

setupLoadFlags :: [ModuleCollection k] -> DynFlags -> IO DynFlags
setupLoadFlags mcs dfs = applyDependencies mcs allDeps . selectEnabled <$> useSavedFlags dfs
  where allDeps = mcs ^? traversal & mcDependencies & traversal
        selectEnabled = if any (\(mc,rest) -> isDirectoryMC mc && isIndependentMc mc rest) (breaks mcs) then id else onlyUseEnabled
        useSavedFlags = foldl @[] (>=>) return (mcs ^? traversal & mcLoadFlagSetup)
        isIndependentMc mc rest = not $ any (`isPrefixOf` (mc ^. mcRoot)) (map (^. mcRoot) rest)

breaks :: [a] -> [(a,[a])]
breaks [] = []
breaks (e:rest) = (e,rest) : map (\(x,ls) -> (x,e:ls)) (breaks rest)

loadFlagsFromBuildInfo :: BuildInfo -> DynFlags -> IO DynFlags
loadFlagsFromBuildInfo bi@BuildInfo{ cppOptions } df
  = do (df',unused,warnings) <- parseDynamicFlags df (map (L noSrcSpan) $ cppOptions)
       mapM_ putStrLn (map unLoc warnings ++ map (("Flag is not used: " ++) . unLoc) unused)
       return (setupLoadExtensions df')
  where setupLoadExtensions = foldl (.) id (map setExtensionFlag' $ catMaybes $ map translateExtension loadExtensions)
        loadExtensions = [PatternSynonyms | patternSynonymsNeeded] ++ [ExplicitNamespaces | explicitNamespacesNeeded]
                           ++ [PackageImports | packageImportsNeeded] ++ [CPP | cppNeeded] ++ [MagicHash | magicHashNeeded]
        explicitNamespacesNeeded = not $ null $ map EnableExtension [ExplicitNamespaces, TypeFamilies, TypeOperators] `intersect` usedExtensions bi
        patternSynonymsNeeded = EnableExtension PatternSynonyms `elem` usedExtensions bi
        packageImportsNeeded = EnableExtension PackageImports `elem` usedExtensions bi
        cppNeeded = EnableExtension CPP `elem` usedExtensions bi
        magicHashNeeded = EnableExtension MagicHash `elem` usedExtensions bi

flagsFromBuildInfo :: BuildInfo -> DynFlags -> IO DynFlags
-- the import pathes are already set globally
flagsFromBuildInfo bi@BuildInfo{ options } df
  = do (df',unused,warnings) <- parseDynamicFlags df (map (L noSrcSpan) $ concatMap snd options)
       mapM_ putStrLn (map unLoc warnings ++ map (("Flag is not used: " ++) . unLoc) unused)
       return $ (flip lang_set (toGhcLang =<< defaultLanguage bi))
         $ foldl (.) id (map (\case EnableExtension ext -> setEnabled True ext
                                    DisableExtension ext -> setEnabled False ext
                        ) (usedExtensions bi))
         $ foldr (.) id (map (setEnabled True) (languageDefault (defaultLanguage bi)))
         $ df'
  where toGhcLang Cabal.Haskell98 = Just GHC.Haskell98
        toGhcLang Cabal.Haskell2010 = Just GHC.Haskell2010
        toGhcLang _ = Nothing

        -- We don't put the default settings (ImplicitPrelude, MonomorphismRestriction) here
        -- because that overrides the opposite extensions (NoImplicitPrelude, NoMonomorphismRestriction)
        -- enabled in modules.
        languageDefault (Just Cabal.Haskell2010)
          = [ DatatypeContexts, DoAndIfThenElse, EmptyDataDecls, ForeignFunctionInterface
            , PatternGuards, RelaxedPolyRec, TraditionalRecordSyntax ]
        -- Haskell 98 is the default
        languageDefault _
          = [ DatatypeContexts, NondecreasingIndentation, NPlusKPatterns, TraditionalRecordSyntax ]

        setEnabled enable ext
          = case translateExtension ext of
              Just e -> (if enable then setExtensionFlag' else unSetExtensionFlag') e
              Nothing -> id
