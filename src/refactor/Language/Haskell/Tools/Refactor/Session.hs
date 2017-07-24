{-# LANGUAGE TemplateHaskell
           , TupleSections
           , TypeApplications
           , MultiWayIf
           #-}
-- | Common operations for managing refactoring sessions, for example loading packages, re-loading modules.
module Language.Haskell.Tools.Refactor.Session where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad.State.Strict
import Control.Reference
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import System.FilePath
import System.IO

import Data.IntSet (member)
import Digraph as GHC
import ErrUtils
import Exception (ExceptionMonad)
import FastString as GHC
import GHC
import DynFlags
import HscTypes as GHC
import Language.Haskell.TH.LanguageExtensions
import Outputable

import Language.Haskell.Tools.AST (IdDom)
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.RefactorBase

-- | The state common for refactoring tools, carrying the state of modules.
data RefactorSessionState
  = RefactorSessionState { __refSessMCs :: [ModuleCollection]
                         }

makeReferences ''RefactorSessionState

instance IsRefactSessionState RefactorSessionState where
  refSessMCs = _refSessMCs
  initSession = RefactorSessionState []

-- | Load packages from the given directories. Loads modules, performs the given callback action, warns for duplicate modules.
loadPackagesFrom :: IsRefactSessionState st => (ModSummary -> IO a) -> ([ModSummary] -> IO ()) -> (st -> FilePath -> IO [FilePath]) -> [FilePath] -> StateT st Ghc (Either RefactorException [a])
loadPackagesFrom report loadCallback additionalSrcDirs packages =
  do modColls <- liftIO $ getAllModules packages
     modify $ refSessMCs .- (++ modColls)
     allModColls <- gets (^. refSessMCs)
     st <- get
     moreSrcDirs <- liftIO $ mapM (additionalSrcDirs st) packages
     lift $ useDirs ((modColls ^? traversal & mcSourceDirs & traversal) ++ concat moreSrcDirs)
     let fileNames = map (^. sfkFileName) $ concat
                      $ map (Map.keys . Map.filter (\v -> fromMaybe True (v ^? recModuleExposed)))
                      $ modColls ^? traversal & mcModules
         alreadyLoadedFilesInOtherPackages
           = concatMap (map (^. sfkFileName) . Map.keys . Map.filter (isJust . (^? typedRecModule)) . (^. mcModules))
                       (filter (\mc -> (mc ^. mcRoot) `notElem` packages) allModColls)
     targets <- map targetId <$> (lift getTargets)
     lift $ mapM_ (\t -> when (targetId t `notElem` targets) (addTarget t))
          $ map (\f -> (Target (TargetFile f Nothing) True Nothing)) fileNames
     handleErrors $ withAlteredDynFlags (liftIO . setupLoadFlags allModColls) $ do
       modsForColls <- lift $ depanal [] True
       let modsToParse = flattenSCCs $ topSortModuleGraph False modsForColls Nothing
           actuallyCompiled = filter (\ms -> getModSumOrig ms `notElem` alreadyLoadedFilesInOtherPackages) modsToParse
       liftIO $ loadCallback actuallyCompiled
       void $ checkEvaluatedMods (\_ -> return ()) actuallyCompiled
       mods <- mapM (loadModule report) actuallyCompiled
       return mods

  where loadModule :: IsRefactSessionState st => (ModSummary -> IO a) -> ModSummary -> StateT st Ghc a
        loadModule report ms
          = do needsCodeGen <- gets (needsGeneratedCode (keyFromMS ms) . (^. refSessMCs))
               reloadModule report (if needsCodeGen then forceCodeGen ms else ms)

-- | Handle GHC exceptions and RefactorException.
handleErrors :: ExceptionMonad m => m a -> m (Either RefactorException a)
handleErrors action = handleSourceError (return . Left . SourceCodeProblem . srcErrorMessages) (Right <$> action)
                        `gcatch` (return . Left)

keyFromMS :: ModSummary -> SourceFileKey
keyFromMS ms = SourceFileKey (normalise $ getModSumOrig ms) (modSumName ms)

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
reloadChangedModules :: IsRefactSessionState st => (ModSummary -> IO a) -> ([ModSummary] -> IO ()) -> (ModSummary -> Bool) -> StateT st Ghc (Either RefactorException [a])
reloadChangedModules report loadCallback isChanged = handleErrors $ do
  reachable <- getReachableModules loadCallback isChanged
  void $ checkEvaluatedMods report reachable
  mapM (reloadModule report) reachable

getReachableModules :: IsRefactSessionState st => ([ModSummary] -> IO ()) -> (ModSummary -> Bool) -> StateT st Ghc [ModSummary]
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
reloadModule :: IsRefactSessionState st => (ModSummary -> IO a) -> ModSummary -> StateT st Ghc a
reloadModule report ms = do
  mcs <- gets (^. refSessMCs)
  let fp = getModSumOrig ms
      modName = modSumName ms
      codeGen = needsGeneratedCode (keyFromMS ms) mcs
  case lookupSourceFileColl fp mcs <|> lookupModuleColl modName mcs of
    Just mc -> do
      let dfs = ms_hspp_opts ms
      dfs' <- liftIO $ compileInContext mc mcs dfs
      let ms' = ms { ms_hspp_opts = dfs' }
      newm <- lift $ withAlteredDynFlags (\_ -> return (ms_hspp_opts ms')) $
        parseTyped (mc ^. mcRoot) (if codeGen then forceCodeGen ms' else ms')
      modify $ refSessMCs & traversal & filtered (== mc) & mcModules
                 .- Map.insert (keyFromMS ms) ((if codeGen then ModuleCodeGenerated else ModuleTypeChecked) newm ms)
      liftIO $ report ms
    Nothing -> liftIO $ throwIO $ ModuleNotInPackage modName

checkEvaluatedMods :: IsRefactSessionState st => (ModSummary -> IO a) -> [ModSummary] -> StateT st Ghc [a]
checkEvaluatedMods report mods = do
    mcs <- gets (^. refSessMCs)
    let lookupFlags ms = maybe return (^. mcFlagSetup) mc
          where modName = modSumName ms
                mc = lookupModuleColl modName mcs

    modsNeedCode <- lift (getEvaluatedMods mods lookupFlags)
    mcs <- gets (^. refSessMCs)
    res <- forM modsNeedCode $ \ms -> reloadIfNeeded ms mcs
    return $ catMaybes res
  where reloadIfNeeded ms mcs
          = let key = keyFromMS ms
              in if not (hasGeneratedCode key mcs)
                   then do md <- gets (^. refSessMCs)
                           modify $ refSessMCs .- codeGeneratedFor key
                           md <- gets (^. refSessMCs)
                           if (isAlreadyLoaded key mcs) then
                               -- The module is already loaded but code is not generated. Need to reload.
                               Just <$> lift (codeGenForModule report (codeGeneratedFor key mcs) ms)
                             else return Nothing
                   else return Nothing

-- | Re-load the module with code generation enabled. Must be used when the module had already been loaded,
-- but code generation were not enabled by then.
codeGenForModule :: (ModSummary -> IO a) -> [ModuleCollection] -> ModSummary -> Ghc a
codeGenForModule report mcs ms
  = let modName = modSumName ms
        mc = fromMaybe (error $ "codeGenForModule: The following module is not found: " ++ modName) $ lookupModuleColl modName mcs
     in -- TODO: don't recompile, only load?
        do withAlteredDynFlags (liftIO . compileInContext mc mcs)
             $ void $ parseTyped (mc ^. mcRoot) (forceCodeGen ms)
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


modSumName :: ModSummary -> String
modSumName = GHC.moduleNameString . moduleName . ms_mod

-- * code copied from GHC because it is not public in GhcMake module

type NodeKey   = (ModuleName, HscSource)
type NodeMap a = Map.Map NodeKey a
type SummaryNode = (ModSummary, Int, [Int])

getModFromNode :: SummaryNode -> ModSummary
getModFromNode (ms, _, _) = ms

moduleGraphNodes :: Bool -> [ModSummary]
  -> (Graph SummaryNode, HscSource -> ModuleName -> Maybe SummaryNode)
moduleGraphNodes drop_hs_boot_nodes summaries = (graphFromEdgedVertices nodes, lookup_node)
  where
    numbered_summaries = zip summaries [1..]

    lookup_node :: HscSource -> ModuleName -> Maybe SummaryNode
    lookup_node hs_src mod = Map.lookup (mod, hs_src) node_map

    lookup_key :: HscSource -> ModuleName -> Maybe Int
    lookup_key hs_src mod = fmap summaryNodeKey (lookup_node hs_src mod)

    node_map :: NodeMap SummaryNode
    node_map = Map.fromList [ ((moduleName (ms_mod s), (ms_hsc_src s)), node)
                            | node@(s, _, _) <- nodes ]

    nodes :: [SummaryNode]
    nodes = [ (s, key, out_keys)
            | (s, key) <- numbered_summaries
            , not (isBootSummary s && drop_hs_boot_nodes)
            , let out_keys = out_edge_keys hs_boot_key (map unLoc (ms_home_srcimps s)) ++
                             out_edge_keys HsSrcFile   (map unLoc (ms_home_imps s)) ++
                             (-- see [boot-edges] below
                              if drop_hs_boot_nodes || ms_hsc_src s == HsBootFile
                              then []
                              else case lookup_key HsBootFile (ms_mod_name s) of
                                    Nothing -> []
                                    Just k  -> [k]) ]

    hs_boot_key | drop_hs_boot_nodes = HsSrcFile
                | otherwise          = HsBootFile

    out_edge_keys :: HscSource -> [ModuleName] -> [Int]
    out_edge_keys hi_boot ms = mapMaybe (lookup_key hi_boot) ms

summaryNodeKey :: SummaryNode -> Int
summaryNodeKey (_, k, _) = k

ms_home_imps :: ModSummary -> [Located ModuleName]
ms_home_imps = home_imps . ms_imps

ms_home_srcimps :: ModSummary -> [Located ModuleName]
ms_home_srcimps = home_imps . ms_srcimps

home_imps :: [(Maybe FastString, Located ModuleName)] -> [Located ModuleName]
home_imps imps = [ lmodname |  (mb_pkg, lmodname) <- imps,
                                  isLocal mb_pkg ]
  where isLocal Nothing = True
        isLocal (Just pkg) | pkg == fsLit "this" = True -- "this" is special
        isLocal _ = False
