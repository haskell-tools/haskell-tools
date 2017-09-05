{-# LANGUAGE TemplateHaskell
           , TupleSections
           , TypeApplications
           , MultiWayIf
           , FlexibleContexts
           , BangPatterns
           #-}
-- | Common operations for managing Daemon-tools sessions, for example loading whole packages or
-- re-loading modules when they are changed. Maintains the state of the compilation with loaded
-- modules. Contains checks for compiling the modules to code when Template Haskell is used.
module Language.Haskell.Tools.Daemon.Session where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad.State.Strict
import Control.Reference
import qualified Data.List as List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.Function (on)
import System.Directory
import System.FilePath

import Data.IntSet (member)
import Digraph as GHC
import DynFlags
import Exception (ExceptionMonad)
import GHC
import HscTypes as GHC
import Language.Haskell.TH.LanguageExtensions

import Language.Haskell.Tools.Daemon.GetModules
import Language.Haskell.Tools.Daemon.State
import Language.Haskell.Tools.Daemon.ModuleGraph
import Language.Haskell.Tools.Daemon.Representation
import Language.Haskell.Tools.Daemon.Utils
import Language.Haskell.Tools.Refactor hiding (ModuleName)

type DaemonSession a = StateT DaemonSessionState Ghc a

-- | Load packages from the given directories. Loads modules, performs the given callback action, warns for duplicate modules.
loadPackagesFrom :: (ModSummary -> IO a)
                      -> ([ModSummary] -> IO ())
                      -> (DaemonSessionState -> FilePath -> IO [FilePath])
                      -> [FilePath]
                      -> DaemonSession (Either RefactorException [a])
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
     handleErrors $ withAlteredDynFlags (liftIO . fmap (st ^. ghcFlagsSet) . setupLoadFlags (mcs ^? traversal & mcId) (mcs ^? traversal & mcRoot)
                                                                                            (mcs ^? traversal & mcDependencies & traversal)
                                                                                            (foldl @[] (>=>) return (mcs ^? traversal & mcLoadFlagSetup))) $ do
       modsForColls <- lift $ depanal [] True
       let modsToParse = flattenSCCs $ topSortModuleGraph False modsForColls Nothing
           actuallyCompiled = filter (\ms -> getModSumOrig ms `notElem` alreadyLoadedFilesInOtherPackages) modsToParse
       liftIO $ loadCallback actuallyCompiled
       void $ checkEvaluatedMods (\_ -> return ()) actuallyCompiled
       mods <- mapM (loadModule report) actuallyCompiled
       return mods

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


        loadModule :: (ModSummary -> IO a) -> ModSummary -> DaemonSession a
        loadModule report ms
          = do needsCodeGen <- gets (needsGeneratedCode (keyFromMS ms) . (^. refSessMCs))
               reloadModule report (if needsCodeGen then forceCodeGen ms else ms)

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

-- | Handle GHC exceptions and RefactorException.
handleErrors :: ExceptionMonad m => m a -> m (Either RefactorException a)
handleErrors action = handleSourceError (return . Left . SourceCodeProblem . srcErrorMessages) (Right <$> action)
                        `gcatch` (return . Left)

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
                           -> DaemonSession (Either RefactorException [a])
reloadChangedModules report loadCallback isChanged = handleErrors $ do
  reachable <- getReachableModules loadCallback isChanged
  void $ checkEvaluatedMods report reachable
  mapM (reloadModule report) reachable

-- | Get all modules that can be accessed from a given set of modules. Can be used to select which
-- modules need to be reloaded after a change.
getReachableModules :: ([ModSummary] -> IO ()) -> (ModSummary -> Bool) -> DaemonSession [ModSummary]
getReachableModules loadCallback selected = do
  mcs <- gets (^. refSessMCs)
  -- IMPORTANT: make sure that the module collection is not passed into the flags, they
  -- might not be evaluated and then the reference could prevent garbage collection
  -- of entire ASTs
  withAlteredDynFlags (liftIO . setupLoadFlags (mcs ^? traversal & mcId) (mcs ^? traversal & mcRoot)
                                               (mcs ^? traversal & mcDependencies & traversal)
                                               (foldl @[] (>=>) return (mcs ^? traversal & mcLoadFlagSetup)) ) $ do
    allMods <- lift $ depanal [] True
    let (allModsGraph, lookup) = moduleGraphNodes False allMods
        changedMods = catMaybes $ map (\ms -> lookup (ms_hsc_src ms) (moduleName $ ms_mod ms))
                        $ filter selected allMods
        recompMods = map (ms_mod . getModFromNode) $ reachablesG (transposeG allModsGraph) changedMods
        sortedMods = reverse $ topologicalSortG allModsGraph
        sortedRecompMods = filter ((`elem` recompMods) . ms_mod) $ map getModFromNode sortedMods
    liftIO $ loadCallback sortedRecompMods
    return sortedRecompMods

-- | Reload a given module. Perform a callback.
reloadModule :: (ModSummary -> IO a) -> ModSummary -> DaemonSession a
reloadModule report ms = do
  ghcfl <- gets (^. ghcFlagsSet)
  mcs <- gets (^. refSessMCs)
  
  let fp = getModSumOrig ms
      modName = getModSumName ms
      codeGen = needsGeneratedCode (keyFromMS ms) mcs
  case lookupSourceFileColl fp mcs <|> lookupModuleColl modName mcs of
    Just mc -> reloadModuleIn codeGen modName ghcfl mc
    Nothing -> case mcs of mc:_ -> reloadModuleIn codeGen modName ghcfl mc
                           []   -> error "reloadModule: module collections empty"
  where
    reloadModuleIn codeGen modName ghcfl mc = do
      let dfs = ms_hspp_opts ms
      -- IMPORTANT: make sure that the module collection is not passed into the flags, they
      -- might not be evaluated and then the reference could prevent garbage collection
      -- of entire ASTs
      dfs' <- liftIO $ fmap ghcfl $ ((mc ^. mcFlagSetup) <=< (mc ^. mcLoadFlagSetup)) dfs
      let ms' = ms { ms_hspp_opts = dfs' }
      newm <- lift $ withAlteredDynFlags (\_ -> return dfs') $
        parseTyped (if codeGen then forceCodeGen ms' else ms')
      -- replace the module in the program database
      modify' $ refSessMCs & traversal & filtered (== mc) & mcModules
                  .- Map.insert (keyFromMS ms') ((if codeGen then ModuleCodeGenerated else ModuleTypeChecked) newm ms')
                       . Map.delete (SourceFileKey "" modName)
      liftIO $ report ms'

-- | Finds out if a newly added module forces us to generate code for another one.
-- If the other is already loaded it will be reloaded.
checkEvaluatedMods :: (ModSummary -> IO a) -> [ModSummary] -> DaemonSession [a]
checkEvaluatedMods report mods = do
    mcs <- gets (^. refSessMCs)
    -- IMPORTANT: make sure that the module collection is not passed into the flags, they
    -- might not be evaluated and then the reference could prevent garbage collection
    -- of entire ASTs
    let lookupFlags ms = maybe return (^. mcFlagSetup) mc
          where modName = getModSumName ms
                mc = lookupModuleColl modName mcs
    modsNeedCode <- lift (getEvaluatedMods mods lookupFlags)
    catMaybes <$> forM modsNeedCode (reloadIfNeeded mcs)
  where reloadIfNeeded mcs ms
          = let key = keyFromMS ms
              in if not (hasGeneratedCode key mcs)
                   then do -- mark the module for code generation
                           modify $ refSessMCs .- codeGeneratedFor key
                           if (isAlreadyLoaded key mcs) then
                               -- The module is already loaded but code is not generated. Need to reload.
                               Just <$> lift (codeGenForModule report (codeGeneratedFor key mcs) ms)
                             else return Nothing
                   else return Nothing

-- | Re-load the module with code generation enabled. Must be used when the module had already been loaded,
-- but code generation were not enabled by then.
codeGenForModule :: (ModSummary -> IO a) -> [ModuleCollection SourceFileKey] -> ModSummary -> Ghc a
codeGenForModule report mcs ms
  = let modName = getModSumName ms
        mc = fromMaybe (error $ "codeGenForModule: The following module is not found: " ++ modName) $ lookupModuleColl modName mcs
     in -- TODO: don't recompile, only load?
        do withAlteredDynFlags (liftIO . (mc ^. mcFlagSetup))
             $ void $ parseTyped (forceCodeGen ms)
           liftIO $ report ms

-- | Check which modules can be reached from the module, if it uses template haskell.
-- A definition that needs code generation can be inside a module that does not uses the
-- TemplateHaskell extension.
getEvaluatedMods :: [ModSummary] -> (ModSummary -> DynFlags -> IO DynFlags) -> Ghc [GHC.ModSummary]
-- We cannot really get the modules that need to be linked, because we cannot rename splice content if the
-- module is not type checked and that is impossible if the splice cannot be evaluated.
getEvaluatedMods mods additionalFlags
  = do allMods <- getModuleGraph
       flags <- getSessionDynFlags
       let (allModsGraph, lookup) = moduleGraphNodes False allMods
       -- some flags are stored only in the module collection and are not recorded in the summary
       thmods <- liftIO $ filterM (\ms -> ((|| isTH ms) . xopt TemplateHaskell) <$> additionalFlags ms flags) mods
       let modsWithTH = catMaybes $ map (\ms -> lookup (ms_hsc_src ms) (moduleName $ ms_mod ms)) thmods
           recompMods = map (moduleName . ms_mod . getModFromNode) $ reachablesG allModsGraph modsWithTH
           sortedMods = map getModFromNode $ reverse $ topologicalSortG allModsGraph
           sortedTHMods = filter ((`elem` recompMods) . moduleName . ms_mod) sortedMods
       return sortedTHMods
  where isTH mod = fromEnum TemplateHaskell `member` extensionFlags (ms_hspp_opts mod)
