{-# LANGUAGE TemplateHaskell
           , TupleSections
           , TypeApplications
           , MultiWayIf
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

-- | Load packages from the given directories. Loads modules, performs the given callback action, warns for duplicate modules.
loadPackagesFrom :: (ModSummary -> IO a)
                      -> ([ModSummary] -> IO ())
                      -> (DaemonSessionState -> FilePath -> IO [FilePath])
                      -> [FilePath]
                      -> StateT DaemonSessionState Ghc (Either RefactorException [a])
loadPackagesFrom report loadCallback additionalSrcDirs packages =
  do modColls <- liftIO $ getAllModules packages
     modify $ refSessMCs .- (++ map modCollToSfk modColls)
     allModColls <- gets (^. refSessMCs)
     st <- get
     moreSrcDirs <- liftIO $ mapM (additionalSrcDirs st) packages
     lift $ useDirs ((modColls ^? traversal & mcSourceDirs & traversal) ++ concat moreSrcDirs)
     let dirsMods = map (\mc -> (mc ^. mcSourceDirs, getExposedModules mc)) modColls
         alreadyLoadedFilesInOtherPackages
           = concatMap (map (^. sfkFileName) . Map.keys . Map.filter (isJust . (^? typedRecModule)) . (^. mcModules))
                       (filter (\mc -> (mc ^. mcRoot) `notElem` packages) allModColls)
     targets <- map targetId <$> (lift getTargets)
     lift $ mapM_ (\t -> when (targetId t `notElem` targets) (addTarget t)) . concat
             =<< liftIO (mapM (\(dirs,mods) -> mapM (createTarget dirs) mods) dirsMods)
     handleErrors $ withAlteredDynFlags (liftIO . fmap (st ^. ghcFlagsSet) . setupLoadFlags allModColls) $ do
       modsForColls <- lift $ depanal [] True
       let modsToParse = flattenSCCs $ topSortModuleGraph False modsForColls Nothing
           actuallyCompiled = filter (\ms -> getModSumOrig ms `notElem` alreadyLoadedFilesInOtherPackages) modsToParse
       liftIO $ loadCallback actuallyCompiled
       void $ checkEvaluatedMods (\_ -> return ()) actuallyCompiled
       mods <- mapM (loadModule report) actuallyCompiled
       return mods

  where loadModule :: (ModSummary -> IO a) -> ModSummary -> StateT DaemonSessionState Ghc a
        loadModule report ms
          = do needsCodeGen <- gets (needsGeneratedCode (keyFromMS ms) . (^. refSessMCs))
               reloadModule report (if needsCodeGen then forceCodeGen ms else ms)

        -- | Creates a target from a module name. If possible, finds the
        -- corresponding source file to distinguish between modules of the same name.
        createTarget :: [FilePath] -> String -> IO Target
        createTarget srcFolders modName
          = makeTarget <$> filterM doesFileExist
                             (map (</> toFileName modName) srcFolders)
          where toFileName = (<.> "hs") . List.intercalate [pathSeparator] . splitOn "."
                makeTarget [] = Target (TargetModule (GHC.mkModuleName modName)) True Nothing
                makeTarget (fn:_) = Target (TargetFile fn Nothing) True Nothing

        getExposedModules :: ModuleCollection ModuleNameStr -> [ModuleNameStr]
        getExposedModules
          = Map.keys . Map.filter (\v -> fromMaybe True (v ^? recModuleExposed)) . (^. mcModules)

-- | Handle GHC exceptions and RefactorException.
handleErrors :: ExceptionMonad m => m a -> m (Either RefactorException a)
handleErrors action = handleSourceError (return . Left . SourceCodeProblem . srcErrorMessages) (Right <$> action)
                        `gcatch` (return . Left)

getMods :: (Monad m, IsRefactSessionState st)
        => Maybe SourceFileKey -> StateT st m ( Maybe (SourceFileKey, UnnamedModule IdDom)
                                              , [(SourceFileKey, UnnamedModule IdDom)] )
getMods actMod
  = do mcs <- gets (^. refSessMCs)
       return $ ( (_2 !~ (^? typedRecModule)) =<< flip lookupModInSCs mcs =<< actMod
                , filter ((actMod /=) . Just . fst) $ concatMap (catMaybes . map (_2 !~ (^? typedRecModule)) . Map.assocs . (^. mcModules)) mcs )

getFileMods :: (GhcMonad m, IsRefactSessionState st)
        => String -> StateT st m ( Maybe (SourceFileKey, UnnamedModule IdDom)
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
reloadChangedModules :: (ModSummary -> IO a) -> ([ModSummary] -> IO ()) -> (ModSummary -> Bool) -> StateT DaemonSessionState Ghc (Either RefactorException [a])
reloadChangedModules report loadCallback isChanged = handleErrors $ do
  reachable <- getReachableModules loadCallback isChanged
  void $ checkEvaluatedMods report reachable
  mapM (reloadModule report) reachable

getReachableModules :: ([ModSummary] -> IO ()) -> (ModSummary -> Bool) -> StateT DaemonSessionState Ghc [ModSummary]
getReachableModules loadCallback selected = do
  allModColls <- gets (^. refSessMCs)
  withAlteredDynFlags (liftIO . setupLoadFlags allModColls) $ do
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
reloadModule :: (ModSummary -> IO a) -> ModSummary -> StateT DaemonSessionState Ghc a
reloadModule report ms = do
  ghcfl <- gets (^. ghcFlagsSet)
  mcs <- gets (^. refSessMCs)
  let fp = getModSumOrig ms
      modName = getModSumName ms
      codeGen = needsGeneratedCode (keyFromMS ms) mcs
  case lookupSourceFileColl fp mcs <|> lookupModuleColl modName mcs of
    Just mc -> do
      let dfs = ms_hspp_opts ms
      dfs' <- liftIO $ fmap ghcfl $ compileInContext mc mcs dfs
      let ms' = ms { ms_hspp_opts = dfs' }
      newm <- lift $ withAlteredDynFlags (\_ -> return dfs') $
        parseTyped (if codeGen then forceCodeGen ms' else ms')
      modify $ refSessMCs & traversal & filtered (== mc) & mcModules
                 .- Map.insert (keyFromMS ms) ((if codeGen then ModuleCodeGenerated else ModuleTypeChecked) newm ms)
                      . Map.delete (SourceFileKey "" modName)
      liftIO $ report ms
    Nothing -> liftIO $ throwIO $ ModuleNotInPackage modName

checkEvaluatedMods :: (ModSummary -> IO a) -> [ModSummary] -> StateT DaemonSessionState Ghc [a]
checkEvaluatedMods report mods = do
    mcs <- gets (^. refSessMCs)
    let lookupFlags ms = maybe return (^. mcFlagSetup) mc
          where modName = getModSumName ms
                mc = lookupModuleColl modName mcs

    modsNeedCode <- lift (getEvaluatedMods mods lookupFlags)
    mcs <- gets (^. refSessMCs)
    res <- forM modsNeedCode $ \ms -> reloadIfNeeded ms mcs
    return $ catMaybes res
  where reloadIfNeeded ms mcs
          = let key = keyFromMS ms
              in if not (hasGeneratedCode key mcs)
                   then do modify $ refSessMCs .- codeGeneratedFor key
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
        do withAlteredDynFlags (liftIO . compileInContext mc mcs)
             $ void $ parseTyped (forceCodeGen ms)
           liftIO $ report ms

-- | Check which modules can be reached from the module, if it uses template haskell.
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
