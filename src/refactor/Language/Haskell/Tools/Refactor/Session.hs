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

import GHC
import GhcMonad as GHC
import HscTypes as GHC
import Digraph as GHC
import FastString as GHC

import Language.Haskell.Tools.AST (IdDom)
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.RefactorBase

data RefactorSessionState
  = RefactorSessionState { __refSessMCs :: [ModuleCollection (UnnamedModule IdDom)]
                         }

makeReferences ''RefactorSessionState

class IsRefactSessionState st where
  refSessMCs :: Simple Lens st [ModuleCollection (UnnamedModule IdDom)]
  initSession :: st

instance IsRefactSessionState RefactorSessionState where
  refSessMCs = _refSessMCs
  initSession = RefactorSessionState []


loadPackagesFrom :: IsRefactSessionState st => (String -> IO a) -> [FilePath] -> StateT st Ghc ([a], [(ModuleCollectionId, String)])
loadPackagesFrom report packages = 
  do modColls <- liftIO $ getAllModules packages
     res <- lift $ flip evalStateT [] $ forM modColls $ \mc -> do
       alreadyLoaded <- get
       let loadedModNames = map GHC.moduleNameString alreadyLoaded
           newModNames = map (^. sfkModuleName) $ Map.keys $ mc ^. mcModules
           ignoredMods = newModNames `List.intersect` loadedModNames
       lift $ useDirs (mc ^. mcSourceDirs)
       lift $ setTargets $ map (\mod -> (Target (TargetModule (GHC.mkModuleName mod)) True Nothing)) 
                                        (newModNames List.\\ ignoredMods)
       -- depanal sets the dynamic flags for modules, so they need to be set before calling it
       withAlteredDynFlags (liftIO . (mc ^. mcFlagSetup)) $
         do modsForMC <- lift $ depanal alreadyLoaded True
            let modsToParse = flattenSCCs $ topSortModuleGraph False modsForMC Nothing
            mods <- lift $ mapM (loadModule report) modsToParse
            modify $ (++ map ms_mod_name modsForMC)
            return $ ((map fst mods, map (mc ^. mcId, ) ignoredMods), (mcModules .= Map.fromList (map snd mods)) mc)
     modify $ refSessMCs .= map snd res
     return (concatMap (fst . fst) res, concatMap (snd . fst) res)

  where loadModule :: (String -> IO a) -> ModSummary -> Ghc (a, (SourceFileKey, UnnamedModule IdDom))
        loadModule report ms = 
          do let modName = GHC.moduleNameString $ moduleName $ ms_mod ms
             mm <- parseTyped ms
             rep <- liftIO $ report modName
             res <- return (rep, (SourceFileKey (case ms_hsc_src ms of HsSrcFile -> NormalHs; _ -> IsHsBoot) modName, mm))
             return res

getMods :: (Monad m, IsRefactSessionState st) => Maybe SourceFileKey -> StateT st m (Maybe (SourceFileKey, UnnamedModule IdDom), [(SourceFileKey, UnnamedModule IdDom)])
getMods actMod 
  = do mcs <- gets (^. refSessMCs)
       return $ ( flip lookupModInSCs mcs =<< actMod
                , filter ((actMod /=) . Just . fst) $ concatMap (Map.assocs . (^. mcModules)) mcs )

assocToNamedMod :: (SourceFileKey, UnnamedModule dom) -> ModuleDom dom
assocToNamedMod (SourceFileKey _ n, mod) = (n, mod)


withAlteredDynFlags :: GhcMonad m => (DynFlags -> m DynFlags) -> m a -> m a
withAlteredDynFlags modDFs action = do
  dfs <- getSessionDynFlags
  setSessionDynFlags =<< modDFs dfs
  res <- action
  setSessionDynFlags dfs
  return res

reloadChangedModules :: IsRefactSessionState st => (String -> IO a) -> [String] -> StateT st Ghc [a]
reloadChangedModules report changedModNames = do
  allMods <- lift $ depanal [] True
  let (allModsGraph, lookup) = moduleGraphNodes False allMods
      changedMods = catMaybes $ map (\ms -> lookup (ms_hsc_src ms) (moduleName $ ms_mod ms))
                      $ filter (\ms -> (GHC.moduleNameString $ moduleName $ ms_mod ms) `elem` changedModNames) allMods
      recompMods = map (ms_mod . getModFromNode) $ reachablesG (transposeG allModsGraph) changedMods
      sortedMods = reverse $ topologicalSortG allModsGraph
      sortedRecompMods = filter ((`elem` recompMods) . ms_mod . getModFromNode) sortedMods
  mapM (reloadModule report) (map getModFromNode sortedRecompMods)

reloadModule :: IsRefactSessionState st => (String -> IO a) -> ModSummary -> StateT st Ghc a
reloadModule report ms = do 
  let modName = GHC.moduleNameString $ moduleName $ ms_mod ms
  Just mc <- gets (lookupModuleColl modName . (^. refSessMCs))
  newm <- lift $ withAlteredDynFlags (liftIO . (mc ^. mcFlagSetup)) $
    parseTyped ms
  modify $ refSessMCs .- updateModule modName NormalHs newm
  liftIO $ report modName

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
