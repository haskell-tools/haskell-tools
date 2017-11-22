{-# LANGUAGE FlexibleContexts, MultiWayIf, TypeApplications #-}
-- | Common operations for managing Daemon-tools sessions, for example loading whole packages or
-- re-loading modules when they are changed. Maintains the state of the compilation with loaded
-- modules. Contains checks for compiling the modules to code when Template Haskell is used.
module Language.Haskell.Tools.Daemon.Session where

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

import Digraph as GHC
import DynFlags
import GHC
import Exception
import GHCi
import GhcMonad (modifySession)
import HscTypes
import Language.Haskell.TH.LanguageExtensions as Exts
import Linker
import Module
import NameCache
import Packages

import Language.Haskell.Tools.Daemon.GetModules
import Language.Haskell.Tools.Daemon.ModuleGraph
import Language.Haskell.Tools.Daemon.Representation
import Language.Haskell.Tools.Daemon.State
import Language.Haskell.Tools.Daemon.Utils
import Language.Haskell.Tools.Refactor hiding (ModuleName)

type DaemonSession a = StateT DaemonSessionState Ghc a

-- | Load packages from the given directories. Loads modules, performs the given callback action, warns for duplicate modules.
loadPackagesFrom :: (ModSummary -> IO ())
                      -> ([ModSummary] -> IO ())
                      -> (DaemonSessionState -> FilePath -> IO [FilePath])
                      -> [FilePath]
                      -> DaemonSession [SourceError]
loadPackagesFrom report loadCallback additionalSrcDirs packages =
  do -- collecting modules to load
     modColls <- liftIO $ getAllModules packages
     st <- get
     moreSrcDirs <- liftIO $ mapM (additionalSrcDirs st) packages
     lift $ useDirs ((modColls ^? traversal & mcSourceDirs & traversal) ++ concat moreSrcDirs)
     mcs' <- liftIO (traversal !~ locateModules $ modColls)
     modify' (refSessMCs .- (++ mcs'))
     mcs <- gets (^. refSessMCs)
     let alreadyLoadedFiles
           = concatMap (map (^. sfkFileName) . Map.keys . Map.filter (isJust . (^? typedRecModule)) . (^. mcModules))
                       (filter (\mc -> (mc ^. mcRoot) `notElem` packages) mcs)
     currentTargets <- map targetId <$> (lift getTargets)
     lift $ mapM_ (\t -> when (targetId t `notElem` currentTargets) (addTarget t))
                  (map makeTarget $ List.nubBy ((==) `on` (^. sfkFileName))
                                  $ List.sort $ concatMap getExposedModules mcs')
     loadRes <- gtry (loadModules mcs alreadyLoadedFiles)
     case loadRes of
       Right mods -> do 
         modify (refSessMCs & traversal & filtered (\mc -> (mc ^. mcId) `elem` map (^. mcId) modColls) & mcLoadDone .= True)
         compileModules report mods
       Left err -> return [err]

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

        loadModules mcs alreadyLoaded = do
          mods <- withLoadFlagsForModules mcs $ do
            loadVisiblePackages -- need to update package state when setting the list of visible packages
            modsForColls <- lift $ depanal [] True
            let modsToParse = flattenSCCs $ topSortModuleGraph False modsForColls Nothing
                actuallyCompiled = filter (\ms -> getModSumOrig ms `notElem` alreadyLoaded) modsToParse
            modify' (refSessMCs .- foldl (.) id (map (insertIfMissing . keyFromMS) actuallyCompiled))
            return actuallyCompiled
          liftIO $ loadCallback mods
          return mods

        compileModules report mods = do
            checkEvaluatedMods mods
            compileWhileOk mods
          where compileWhileOk [] = return []
                compileWhileOk (mod:mods) 
                  = do res <- gtry (reloadModule report mod)
                       case res of
                          Left err -> do dependents <- lift $ dependentModules (return . (ms_mod mod ==) . ms_mod)
                                         (err :) <$> compileWhileOk (filter ((`notElem` map ms_mod dependents) . ms_mod) mods)
                          Right _ -> compileWhileOk mods
        

-- | Loads the packages that are declared visible (by .cabal file).
loadVisiblePackages :: DaemonSession ()
loadVisiblePackages = do
  dfs <- getSessionDynFlags
  (dfs', _) <- liftIO $ initPackages dfs
  setSessionDynFlags dfs' -- set the package flags (only for this load session)
  modify' (pkgDbFlags .= \dfs -> dfs { pkgDatabase = pkgDatabase dfs'
                                     , pkgState = pkgState dfs'
                                     }) -- save the package database

-- | Get the module that is selected for refactoring and all the other modules.
getFileMods :: String -> DaemonSession ( Maybe (SourceFileKey, UnnamedModule IdDom)
                                       , [(SourceFileKey, UnnamedModule IdDom)] )
getFileMods fnameOrModule = do
  modMaps <- gets (^? refSessMCs & traversal & mcModules)
  let modules = mapMaybe (\(k,m) -> (\ms tc -> (ms, (k,tc))) <$> (m ^? modRecMS) <*> (m ^? typedRecModule)) -- not type checkable modules are ignored
                  $ concatMap @[] Map.assocs modMaps
      (modSel, modOthers) = List.partition (\(ms,_) -> getModSumName ms == fnameOrModule
                                                         && (case ms_hsc_src ms of HsSrcFile -> True; _ -> False))
                                          modules
      maxSufLength = maximum $ map sufLength modules
      (fnSel, fnOthers) = if null modules || maxSufLength == 0
                            then ([], modules)
                            else List.partition ((== maxSufLength) . sufLength) modules
      sufLength = length . commonSuffix (splitPath fnameOrModule) . splitPath . getModSumOrig . fst
      commonSuffix l1 l2 = takeWhile (uncurry (==)) $ zip (reverse l1) (reverse l2)
      backup = case fnSel of
                 []      -> return (Nothing, map snd fnOthers)
                 [(_,m)] -> return (Just m, map snd fnOthers)
                 _:_     -> error "getFileMods: multiple modules selected"
  case modSel of
    []      -> backup
    [(_,m)] -> return (Just m, map snd modOthers)
    _:_     -> backup

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
  mcs <- gets (^. refSessMCs)
  withLoadFlagsForModules mcs $ do
    lift $ depanal [] True
    sortedRecompMods <- lift $ dependentModules (return . selected)
    liftIO $ loadCallback sortedRecompMods
    return sortedRecompMods

-- | Reload a given module. Perform a callback.
reloadModule :: (ModSummary -> IO a) -> ModSummary -> DaemonSession a
reloadModule report ms = do
  mcs <- gets (^. refSessMCs)
  ghcfl <- gets (^. ghcFlagsSet)
  let codeGen = needsGeneratedCode (keyFromMS ms) mcs
      mc = decideMC ms mcs
  newm <- withFlagsForModule mc $ lift $ do
    dfs <- liftIO $ fmap ghcfl $ mc ^. mcFlagSetup $ ms_hspp_opts ms
    let ms' = ms { ms_hspp_opts = dfs }
    -- some flags are cached in mod summary, so we need to override
    parseTyped (case codeGen of NoCodeGen -> ms'
                                InterpretedCode -> forceCodeGen ms'
                                GeneratedCode -> forceAsmGen ms')
  -- replace the module in the program database
  modify' $ refSessMCs & traversal & filtered (\c -> (c ^. mcId) == (mc ^. mcId)) & mcModules
              .- Map.insert (keyFromMS ms) (ModuleTypeChecked newm ms codeGen)
                   . removeModuleMS ms
  liftIO $ report ms

-- | Select which module collection we think the module is in
decideMC :: ModSummary -> [ModuleCollection SourceFileKey] -> ModuleCollection SourceFileKey
decideMC ms mcs =
  case lookupModuleCollection ms mcs of
    Just mc -> mc
    Nothing -> case filter (\mc -> (mc ^. mcRoot) `List.isPrefixOf` fileName) mcs of
                 mc:_ -> mc
                 _ -> case mcs of mc:_ -> mc
                                  []   -> error "reloadModule: module collections empty"
  where fileName = getModSumOrig ms

-- | Prepares the DynFlags for the compilation of a module
withFlagsForModule :: ModuleCollection SourceFileKey -> DaemonSession a -> DaemonSession a
withFlagsForModule mc action = do
  ghcfl <- gets (^. ghcFlagsSet)
  dbFlags <- gets (^. pkgDbFlags)
  -- IMPORTANT: make sure that the module collection is not passed into the flags, they
  -- might not be evaluated and then the reference could prevent garbage collection
  -- of entire ASTs
  withAlteredDynFlags (liftIO . fmap (dbFlags . ghcfl) . ((mc ^. mcFlagSetup) <=< (mc ^. mcLoadFlagSetup))) action

-- | Prepares the DynFlags for travesing the module graph
withLoadFlagsForModules :: [ModuleCollection SourceFileKey] -> DaemonSession a -> DaemonSession a
-- IMPORTANT: make sure that a module collection is not passed into the flags, they
-- might not be evaluated and then the reference could prevent garbage collection
-- of entire ASTs
withLoadFlagsForModules mcs action = do
  ghcfl <- gets (^. ghcFlagsSet)
  dbFlags <- gets (^. pkgDbFlags)
  withAlteredDynFlags (liftIO . fmap (dbFlags . ghcfl)
                              . setupLoadFlags (mcs ^? traversal & mcId) (mcs ^? traversal & mcRoot)
                                               (mcs ^? traversal & mcDependencies & traversal)
                                               (foldl @[] (>=>) return (mcs ^? traversal & mcLoadFlagSetup))) action

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
    (modsNeedCode, modsNeedAsm) <- lift (getEvaluatedMods changed lookupFlags)
    -- specify the need of code generation for later loading
    forM_ modsNeedCode (\ms -> modify $ refSessMCs .- codeGeneratedFor (keyFromMS ms) InterpretedCode)
    forM_ modsNeedAsm (\ms -> modify $ refSessMCs .- codeGeneratedFor (keyFromMS ms) GeneratedCode)
    let interpreted = filter (\ms -> isAlreadyLoaded (keyFromMS ms) InterpretedCode mcs) 
                             modsNeedCode
        codeGenerated = filter (\ms -> isAlreadyLoaded (keyFromMS ms) GeneratedCode mcs) modsNeedAsm
    clearModules (interpreted ++ codeGenerated)
    -- reload modules that have already been loaded
    forM_ interpreted (codeGenForModule mcs InterpretedCode)
    forM_ codeGenerated (codeGenForModule mcs GeneratedCode)

-- | Re-load the module with code generation enabled. Must be used when the module had already been loaded,
-- but code generation were not enabled by then.
codeGenForModule :: [ModuleCollection SourceFileKey] -> CodeGenPolicy -> ModSummary -> DaemonSession ()
codeGenForModule mcs codeGen ms
-- we don't need to update anything, just re-compile (we don't store the typed AST) and generate the code
  = withFlagsForModule mc $ lift $ void $ parseTyped (case codeGen of InterpretedCode -> forceCodeGen ms
                                                                      GeneratedCode -> forceAsmGen ms
                                                                      _ -> ms)
  where mc = fromMaybe (error $ "codeGenForModule: The following module is not found: " ++ getModSumName ms)
               $ lookupModuleCollection ms mcs

-- | Check which modules can be reached from the module, if it uses template haskell.
-- A definition that needs code generation can be inside a module that does not uses the
-- TemplateHaskell extension.
getEvaluatedMods :: [ModSummary] -> (ModSummary -> IO DynFlags) -> Ghc ([ModSummary],[ModSummary])
-- We cannot really get the modules that need to be linked, because we cannot rename splice content if the
-- module is not type checked and that is impossible if the splice cannot be evaluated.
getEvaluatedMods changed additionalFlags
  = do let changedModulePathes = map getModSumOrig changed
       -- some flags are stored only in the module collection and are not recorded in the summary
       eval <- supportingModules (\ms -> (\flags -> getModSumOrig ms `elem` changedModulePathes && TemplateHaskell `xopt` flags)
                                           <$> liftIO (additionalFlags ms))
       asm <- supportingModules (\ms -> (\flags -> getModSumOrig ms `elem` changedModulePathes 
                                                     && (StaticPointers `xopt` flags || UnboxedTuples `xopt` flags || UnboxedSums `xopt` flags))
                                           <$> liftIO (additionalFlags ms))
       let asmOrigs = map getModSumOrig asm
       return (filter (\ms -> getModSumOrig ms `notElem` asmOrigs) eval, asm)
