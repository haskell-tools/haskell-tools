{-# LANGUAGE TemplateHaskell 
           , TupleSections
           #-}
module Language.Haskell.Tools.Refactor.Session where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Data.Function (on)
import Control.Monad.State
import Control.Reference
import System.IO
import System.FilePath
import Debug.Trace

import GHC
import Outputable
import ErrUtils
import GhcMonad as GHC
import HscTypes as GHC
import Digraph as GHC
import DynFlags as GHC
import FastString as GHC
import Data.IntSet (member)
import Language.Haskell.TH.LanguageExtensions

import Language.Haskell.Tools.AST (IdDom, semanticsModule)
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.RefactorBase

data RefactorSessionState
  = RefactorSessionState { __refSessMCs :: [ModuleCollection]
                         }

makeReferences ''RefactorSessionState

class IsRefactSessionState st where
  refSessMCs :: Simple Lens st [ModuleCollection]
  initSession :: st

instance IsRefactSessionState RefactorSessionState where
  refSessMCs = _refSessMCs
  initSession = RefactorSessionState []


loadPackagesFrom :: IsRefactSessionState st => (ModSummary -> IO a) -> [FilePath] -> StateT st Ghc (Either String ([a], [String]))
loadPackagesFrom report packages = 
  do modColls <- liftIO $ getAllModules packages
     modify $ refSessMCs .- (++ modColls)
     allModColls <- gets (^. refSessMCs)
     lift $ useDirs (modColls ^? traversal & mcSourceDirs & traversal)
     let (ignored, modNames) = extractDuplicates $ map (^. sfkModuleName) $ concat $ map Map.keys $ modColls ^? traversal & mcModules
         alreadyExistingMods = concatMap (map (^. sfkModuleName) . Map.keys . (^. mcModules)) (allModColls List.\\ modColls)
     lift $ mapM addTarget $ map (\mod -> (Target (TargetModule (GHC.mkModuleName mod)) True Nothing)) modNames
     handleSourceError (return . Left . concat . List.intersperse "\n\n" . map showSDocUnsafe . pprErrMsgBagWithLoc . srcErrorMessages) $
       withAlteredDynFlags (return . enableAllPackages allModColls) $ do
         modsForColls <- lift $ depanal [] True
         let modsToParse = flattenSCCs $ topSortModuleGraph False modsForColls Nothing
             actuallyCompiled = filter (not . (`elem` alreadyExistingMods) . modSumName) modsToParse
         checkEvaluatedMods report modsToParse
         mods <- mapM (loadModule report) actuallyCompiled
         return $ Right (mods, ignored)

  where extractDuplicates :: Eq a => [a] -> ([a],[a])
        extractDuplicates (a:rest) 
          = case extractDuplicates rest of (repl, orig) -> if a `elem` orig then (a:repl, orig) else (repl, a:orig)
        extractDuplicates [] = ([],[])

        loadModule :: IsRefactSessionState st => (ModSummary -> IO a) -> ModSummary -> StateT st Ghc a
        loadModule report ms = do
          needsCodeGen <- gets (needsGeneratedCode (keyFromMS ms) . (^. refSessMCs))
          reloadModule report (if needsCodeGen then forceCodeGen ms else ms)

keyFromMS :: ModSummary -> SourceFileKey
keyFromMS ms = SourceFileKey (case ms_hsc_src ms of HsSrcFile -> NormalHs; _ -> IsHsBoot) (modSumName ms)

getMods :: (Monad m, IsRefactSessionState st) 
        => Maybe SourceFileKey -> StateT st m ( Maybe (SourceFileKey, UnnamedModule IdDom)
                                              , [(SourceFileKey, UnnamedModule IdDom)] )
getMods actMod 
  = do mcs <- gets (^. refSessMCs)
       return $ ( (_2 !~ (^? typedRecModule)) =<< flip lookupModInSCs mcs =<< actMod
                , filter ((actMod /=) . Just . fst) $ concatMap (catMaybes . map (_2 !~ (^? typedRecModule)) . Map.assocs . (^. mcModules)) mcs )

getFileMods :: (GhcMonad m, IsRefactSessionState st) 
        => FilePath -> StateT st m ( Maybe (SourceFileKey, UnnamedModule IdDom)
                                   , [(SourceFileKey, UnnamedModule IdDom)] )
getFileMods fname 
  = do mcs <- gets (^. refSessMCs)
       let mods = map (\(k,m) -> (fromJust $ m ^? modRecMS, k))
                      (concatMap Map.assocs $ (mcs ^? traversal & mcModules :: [Map.Map SourceFileKey ModuleRecord]))
       let sfs = catMaybes $ map (\(ms,k) -> if Just fname == fmap normalise (ml_hs_file (ms_location ms)) then Just k else Nothing) mods
       case sfs of sf:_ -> getMods (Just sf)
                   [] -> getMods Nothing

reloadChangedModules :: IsRefactSessionState st => (ModSummary -> IO a) -> (ModSummary -> Bool) -> StateT st Ghc [a]
reloadChangedModules report isChanged = do
  reachable <- getReachableModules isChanged
  checkEvaluatedMods report reachable
  mapM (reloadModule report) reachable

getReachableModules :: IsRefactSessionState st => (ModSummary -> Bool) -> StateT st Ghc [ModSummary]
getReachableModules selected = do 
  allModColls <- gets (^. refSessMCs)
  withAlteredDynFlags (return . enableAllPackages allModColls) $ do
    allMods <- lift $ depanal [] True
    let (allModsGraph, lookup) = moduleGraphNodes False allMods
        changedMods = catMaybes $ map (\ms -> lookup (ms_hsc_src ms) (moduleName $ ms_mod ms))
                        $ filter selected allMods
        recompMods = map (ms_mod . getModFromNode) $ reachablesG (transposeG allModsGraph) changedMods
        sortedMods = reverse $ topologicalSortG allModsGraph
    return $ filter ((`elem` recompMods) . ms_mod) $ map getModFromNode sortedMods

reloadModule :: IsRefactSessionState st => (ModSummary -> IO a) -> ModSummary -> StateT st Ghc a
reloadModule report ms = do 
  let modName = modSumName ms
  mcs <- gets (^. refSessMCs)
  let Just mc = lookupModuleColl modName mcs
      codeGen = hasGeneratedCode (keyFromMS ms) mcs
  let dfs = ms_hspp_opts ms 
  dfs' <- liftIO $ compileInContext mc mcs dfs
  let ms' = ms { ms_hspp_opts = dfs' }
  newm <- lift $ withAlteredDynFlags (liftIO . compileInContext mc mcs) $ 
    parseTyped (if codeGen then forceCodeGen ms' else ms')
  modify $ refSessMCs & traversal & filtered (\mc' -> (mc' ^. mcRoot) == (mc ^. mcRoot)) & mcModules 
             .- Map.insert (keyFromMS ms) ((if codeGen then ModuleCodeGenerated else ModuleTypeChecked) newm ms)
  liftIO $ report ms

checkEvaluatedMods :: IsRefactSessionState st => (ModSummary -> IO a) -> [ModSummary] -> StateT st Ghc [a]
checkEvaluatedMods report mods = do
    modsNeedCode <- lift (getEvaluatedMods mods)
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

codeGenForModule :: (ModSummary -> IO a) -> [ModuleCollection] -> ModSummary -> Ghc a
codeGenForModule report mcs ms 
  = let modName = modSumName ms
        Just mc = lookupModuleColl modName mcs
        Just rec = lookupModInSCs (keyFromMS ms) mcs
     in -- TODO: don't recompile, only load?
        do withAlteredDynFlags (liftIO . compileInContext mc mcs)
             $ parseTyped (forceCodeGen ms)
           liftIO $ report ms 

-- | Check which modules can be reached from the module, if it uses template haskell.
getEvaluatedMods :: [ModSummary] -> Ghc [GHC.ModSummary]
-- We cannot really get the modules that need to be linked, because we cannot rename splice content if the
-- module is not type checked and that is impossible if the splice cannot be evaluated.
getEvaluatedMods mods
  = do allMods <- getModuleGraph
       let (allModsGraph, lookup) = moduleGraphNodes False allMods
           modsWithTH = catMaybes $ map (\ms -> lookup (ms_hsc_src ms) (moduleName $ ms_mod ms)) $ filter isTH mods
           recompMods = map (moduleName . ms_mod . getModFromNode) $ reachablesG allModsGraph modsWithTH
           sortedMods = map getModFromNode $ reverse $ topologicalSortG allModsGraph
           sortedTHMods = filter ((`elem` recompMods) . moduleName . ms_mod) sortedMods
       return sortedTHMods
  where isTH mod = fromEnum TemplateHaskell `member` extensionFlags (ms_hspp_opts mod)


modSumName :: ModSummary -> String
modSumName = GHC.moduleNameString . moduleName . ms_mod

-- * code copied from GHC because it is not public in GhcMake module

type NodeKey   = (ModuleName, IsBoot)
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
    lookup_node hs_src mod = Map.lookup (mod, hscSourceToIsBoot hs_src) node_map

    lookup_key :: HscSource -> ModuleName -> Maybe Int
    lookup_key hs_src mod = fmap summaryNodeKey (lookup_node hs_src mod)

    node_map :: NodeMap SummaryNode
    node_map = Map.fromList [ ((moduleName (ms_mod s),
                                hscSourceToIsBoot (ms_hsc_src s)), node)
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

hscSourceToIsBoot :: HscSource -> IsBoot
hscSourceToIsBoot HsBootFile = IsHsBoot
hscSourceToIsBoot _ = NormalHs

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
