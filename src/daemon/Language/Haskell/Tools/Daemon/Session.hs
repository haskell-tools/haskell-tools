{-# LANGUAGE TemplateHaskell
           , TupleSections
           , TypeApplications
           , MultiWayIf
           , FlexibleContexts
           , BangPatterns
           , StandaloneDeriving
           #-}
-- | Common operations for managing Daemon-tools sessions, for example loading whole packages or
-- re-loading modules when they are changed. Maintains the state of the compilation with loaded
-- modules. Contains checks for compiling the modules to code when Template Haskell is used.
module Language.Haskell.Tools.Daemon.Session where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar
import Control.Monad.State.Strict
import Control.Reference
import Data.Function (on)
import Data.IORef
import qualified Data.List as List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import System.Directory
import System.FilePath

import Data.IntSet (member)
import Digraph as GHC
import DynFlags
import GHC
import GHCi
import GhcMonad (modifySession)
import HscTypes
import Language.Haskell.TH.LanguageExtensions as Exts
import Linker
import Module
import NameCache
import Outputable
import Packages
import UniqDFM
import UniqFM

import Language.Haskell.Tools.Daemon.GetModules
import Language.Haskell.Tools.Daemon.ModuleGraph
import Language.Haskell.Tools.Daemon.Representation
import Language.Haskell.Tools.Daemon.State
import Language.Haskell.Tools.Daemon.Utils
import Language.Haskell.Tools.Refactor hiding (ModuleName)

type DaemonSession a = StateT DaemonSessionState Ghc a

-- | Load packages from the given directories. Loads modules, performs the given callback action, warns for duplicate modules.
loadPackagesFrom :: (ModSummary -> IO a)
                      -> ([ModSummary] -> IO ())
                      -> (DaemonSessionState -> FilePath -> IO [FilePath])
                      -> [FilePath]
                      -> DaemonSession [a]
loadPackagesFrom report loadCallback additionalSrcDirs packages =
  do modColls <- liftIO $ getAllModules packages
     st <- get
     moreSrcDirs <- liftIO $ mapM (additionalSrcDirs st) packages
     lift $ useDirs ((modColls ^? traversal & mcSourceDirs & traversal) ++ concat moreSrcDirs)
     mcs' <- liftIO (traversal !~ locateModules $ modColls)
     modify' (refSessMCs .- (++ mcs'))
     mcs <- gets (^. refSessMCs)
     let alreadyLoadedFilesInOtherPackages
           = concatMap (map (^. sfkFileName) . Map.keys . Map.filter (isJust . (^? typedRecModule)) . (^. mcModules))
                       (filter (\mc -> (mc ^. mcRoot) `notElem` packages) mcs)
     currentTargets <- map targetId <$> (lift getTargets)
     lift $ mapM_ (\t -> when (targetId t `notElem` currentTargets) (addTarget t))
                  (map makeTarget $ List.nubBy ((==) `on` (^. sfkFileName))
                                  $ List.sort $ concatMap getExposedModules mcs')
     let flagsForLoad = liftIO . fmap ((st ^. pkgDbFlags) . (st ^. ghcFlagsSet))
                               . setupLoadFlags (mcs ^? traversal & mcId) (mcs ^? traversal & mcRoot)
                                                (mcs ^? traversal & mcDependencies & traversal)
                                                (foldl @[] (>=>) return (mcs ^? traversal & mcLoadFlagSetup))
     mods <- withAlteredDynFlags flagsForLoad $ do
       loadVisiblePackages -- need to update package state when setting the list of visible packages
       modsForColls <- lift $ depanal [] True
       let modsToParse = flattenSCCs $ topSortModuleGraph False modsForColls Nothing
           actuallyCompiled = filter (\ms -> getModSumOrig ms `notElem` alreadyLoadedFilesInOtherPackages) modsToParse
       modify' (refSessMCs .- foldl (.) id (map (insertIfMissing . keyFromMS) actuallyCompiled))
       dfs <- getSessionDynFlags
       return actuallyCompiled

     liftIO $ loadCallback mods
     checkEvaluatedMods mods
     mapM (reloadModule report) mods

  where getExposedModules :: ModuleCollection k -> [k]
        getExposedModules
          = Map.keys . Map.filter (\v -> fromMaybe True (v ^? recModuleExposed)) . (^. mcModules)

        locateModules :: ModuleCollection ModuleNameStr -> IO (ModuleCollection SourceFileKey)
        locateModules mc
          = mcModules !~ ((Map.fromList <$>)
                            . mapM (locateModule (mc ^. mcSourceDirs) (mc ^. mcModuleFiles))
                            . Map.assocs) $ mc

        locateModule :: [FilePath] -> [(ModuleNameStr, FilePath)]
                          -> (ModuleNameStr, ModuleRecord) -> IO (SourceFileKey,ModuleRecord)
        locateModule srcDirs modMaps (modName, record)
          = do candidate <- createTargetCandidate srcDirs modMaps modName
               return (SourceFileKey (either (const "") id candidate) modName, record)

        -- | Creates a possible target from a module name. If possible, finds the
        -- corresponding source file to distinguish between modules of the same name.
        createTargetCandidate :: [FilePath] -> [(ModuleNameStr, FilePath)] -> ModuleNameStr
                                    -> IO (Either ModuleName FilePath)
        createTargetCandidate srcFolders mapping modName
          = wrapEither <$> filterM doesFileExist
                             (map (</> toFileName modName) srcFolders)
          where toFileName modName
                  = case lookup modName mapping of
                      Just fileName -> fileName
                      Nothing -> List.intercalate [pathSeparator] (splitOn "." modName) <.> "hs"
                wrapEither [] = Left (GHC.mkModuleName modName)
                wrapEither (fn:_) = Right fn

        makeTarget (SourceFileKey "" modName) = Target (TargetModule (GHC.mkModuleName modName)) True Nothing
        makeTarget (SourceFileKey filePath _) = Target (TargetFile filePath Nothing) True Nothing

loadVisiblePackages :: DaemonSession ()
loadVisiblePackages = do
  dfs <- getSessionDynFlags
  (dfs', _) <- liftIO $ initPackages dfs
  setSessionDynFlags dfs' -- set the package flags (only for this load session)
  modify' (pkgDbFlags .= \dfs -> dfs { pkgDatabase = pkgDatabase dfs'
                                     , pkgState = pkgState dfs'
                                     }) -- save the package database

-- TODO: make getMods and getFileMods clearer

-- | Get the module that is selected for refactoring and all the other modules.
getMods :: Maybe SourceFileKey -> DaemonSession ( Maybe (SourceFileKey, UnnamedModule IdDom)
                                                , [(SourceFileKey, UnnamedModule IdDom)] )
getMods actMod
  = do mcs <- gets (^. refSessMCs)
       return $ ( (_2 !~ (^? typedRecModule)) =<< flip lookupModInSCs mcs =<< actMod
                , filter ((actMod /=) . Just . fst) $ concatMap (catMaybes . map (_2 !~ (^? typedRecModule)) . Map.assocs . (^. mcModules)) mcs )

-- | Get the module that is selected for refactoring and all the other modules.
getFileMods :: String -> DaemonSession ( Maybe (SourceFileKey, UnnamedModule IdDom)
                                       , [(SourceFileKey, UnnamedModule IdDom)] )
getFileMods fname
  = do mcs <- gets (^. refSessMCs)
       let mods = mapMaybe (\(k,m) -> fmap (,k) (m ^? modRecMS))
                           (concatMap @[] Map.assocs $ (mcs ^? traversal & mcModules))
       let sfs = catMaybes $ map (\(ms,k) -> if | Just fname == fmap normalise (ml_hs_file (ms_location ms)) -> Just (False, k)
                                                | fname == getModSumName ms -> Just (True, k)
                                                | otherwise -> Nothing) mods
       case List.sort sfs of (_,sf):_ -> getMods (Just sf)
                             []       -> getMods Nothing

-- | Reload the modules that have been changed (given by predicate). Pefrom the callback.
reloadChangedModules :: (ModSummary -> IO a) -> ([ModSummary] -> IO ()) -> (ModSummary -> Bool)
                           -> DaemonSession [a]
reloadChangedModules report loadCallback isChanged = do
  reachable <- getReachableModules loadCallback isChanged
  checkEvaluatedMods reachable
  -- remove module from session before reloading it, resolves space leak
  clearModules reachable
  mapM (reloadModule report) reachable

-- | Clears the given modules from the GHC state to enable re-loading them
-- From the Haskell-tools state we only clear them individually, when their module collection is determined.
clearModules :: [ModSummary] -> DaemonSession ()
clearModules [] = return ()
clearModules mods = do
  let reachableMods = map ms_mod_name mods
      notReloaded = (`notElem` reachableMods) . GHC.moduleName . mi_module . hm_iface
  env <- getSession
  let hptStay = filterHpt notReloaded (hsc_HPT env)
  -- clear the symbol cache for iserv
  liftIO $ purgeLookupSymbolCache env
  -- clear the global linker state
  liftIO $ unload env (mapMaybe hm_linkable (eltsHpt hptStay))
  -- clear name cache
  nameCache <- liftIO $ readIORef $ hsc_NC env
  let nameCache' = nameCache { nsNames = delModuleEnvList (nsNames nameCache) (map ms_mod mods) }
  liftIO $ writeIORef (hsc_NC env) nameCache'
  -- clear home package table and module graph
  lift $ modifySession (\s -> s { hsc_HPT = hptStay
                                , hsc_mod_graph = filter ((`notElem` reachableMods) . ms_mod_name) (hsc_mod_graph s)
                                })

-- | Get all modules that can be accessed from a given set of modules. Can be used to select which
-- modules need to be reloaded after a change.
getReachableModules :: ([ModSummary] -> IO ()) -> (ModSummary -> Bool) -> DaemonSession [ModSummary]
getReachableModules loadCallback selected = do
  ghcflags <- gets (^. ghcFlagsSet)
  dbFlags <- gets (^. pkgDbFlags)
  mcs <- gets (^. refSessMCs)
  -- IMPORTANT: make sure that the module collection is not passed into the flags, they
  -- might not be evaluated and then the reference could prevent garbage collection
  -- of entire ASTs
  withAlteredDynFlags (liftIO . fmap (dbFlags . ghcflags)
                              . setupLoadFlags (mcs ^? traversal & mcId) (mcs ^? traversal & mcRoot)
                                               (mcs ^? traversal & mcDependencies & traversal)
                                               (foldl @[] (>=>) return (mcs ^? traversal & mcLoadFlagSetup))) $ do
    allMods <- lift $ depanal [] True
    sortedRecompMods <- lift $ dependentModules (return . selected)
    liftIO $ loadCallback sortedRecompMods
    return sortedRecompMods

-- | Reload a given module. Perform a callback.
reloadModule :: (ModSummary -> IO a) -> ModSummary -> DaemonSession a
reloadModule report ms = do
  ghcfl <- gets (^. ghcFlagsSet)
  mcs <- gets (^. refSessMCs)
  let modName = getModSumName ms
      codeGen = needsGeneratedCode (keyFromMS ms) mcs
  case lookupModuleCollection ms mcs of
    Just mc -> reloadModuleIn codeGen modName ghcfl mc
    Nothing -> case mcs of mc:_ -> reloadModuleIn codeGen modName ghcfl mc
                           []   -> error "reloadModule: module collections empty"
  where
    reloadModuleIn codeGen modName ghcfl mc = do
      dbFlags <- gets (^. pkgDbFlags)
      let dfs = ms_hspp_opts ms
      -- IMPORTANT: make sure that the module collection is not passed into the flags, they
      -- might not be evaluated and then the reference could prevent garbage collection
      -- of entire ASTs
      newm <- lift $ withAlteredDynFlags (liftIO . fmap (dbFlags . ghcfl) . ((mc ^. mcFlagSetup) <=< (mc ^. mcLoadFlagSetup))) $ do
        dfs <- liftIO $ mc ^. mcFlagSetup $ ms_hspp_opts ms
        let ms' = ms { ms_hspp_opts = dfs }
        -- some flags are cached in mod summary, so we need to override
        parseTyped (if codeGen then forceCodeGen ms' else ms')
      -- replace the module in the program database
      modify' $ refSessMCs & traversal & filtered (\c -> (c ^. mcId) == (mc ^. mcId)) & mcModules
                  .- Map.insert (keyFromMS ms) ((if codeGen then ModuleCodeGenerated else ModuleTypeChecked) newm ms)
                       . removeModuleMS ms
      liftIO $ report ms

-- | Finds out if a newly added module forces us to generate code for another one.
-- If the other is already loaded it will be reloaded.
checkEvaluatedMods :: [ModSummary] -> DaemonSession ()
checkEvaluatedMods changed = do
    mcs <- gets (^. refSessMCs)
    -- IMPORTANT: make sure that the module collection is not passed into the flags, they
    -- might not be evaluated and then the reference could prevent garbage collection
    -- of entire ASTs
    let lookupFlags ms = maybe return (^. mcFlagSetup) mc $ ms_hspp_opts ms
          where mc = lookupModuleCollection ms mcs
    modsNeedCode <- lift (getEvaluatedMods changed lookupFlags)
    -- specify the need of code generation for later loading
    forM_ modsNeedCode (\ms -> modify $ refSessMCs .- codeGeneratedFor (keyFromMS ms))
    let reloaded = filter (\ms -> isAlreadyLoaded (keyFromMS ms) mcs) modsNeedCode
    clearModules reloaded
    -- reload modules that have already been loaded
    lift $ forM_ reloaded (\ms -> codeGenForModule mcs ms)

-- | Re-load the module with code generation enabled. Must be used when the module had already been loaded,
-- but code generation were not enabled by then.
codeGenForModule :: [ModuleCollection SourceFileKey] -> ModSummary -> Ghc ()
codeGenForModule mcs ms
-- we don't need to update anything, just re-compile (we don't store the typed AST) and generate the code
  = do withAlteredDynFlags (liftIO . (mc ^. mcFlagSetup)) $ void $ parseTyped (forceCodeGen ms)
  where mc = fromMaybe (error $ "codeGenForModule: The following module is not found: " ++ getModSumName ms)
               $ lookupModuleCollection ms mcs

-- | Check which modules can be reached from the module, if it uses template haskell.
-- A definition that needs code generation can be inside a module that does not uses the
-- TemplateHaskell extension.
getEvaluatedMods :: [ModSummary] -> (ModSummary -> IO DynFlags) -> Ghc [ModSummary]
-- We cannot really get the modules that need to be linked, because we cannot rename splice content if the
-- module is not type checked and that is impossible if the splice cannot be evaluated.
getEvaluatedMods changed additionalFlags
  = do let changedModulePathes = map getModSumOrig changed
       -- some flags are stored only in the module collection and are not recorded in the summary
       supportingModules (\ms -> (\flags -> getModSumOrig ms `elem` changedModulePathes && TemplateHaskell `xopt` flags)
                                    <$> liftIO (additionalFlags ms))
