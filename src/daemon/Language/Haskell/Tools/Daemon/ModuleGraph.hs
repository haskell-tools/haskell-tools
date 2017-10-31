-- | Creating a dependency graph of the modules loaded into a session.
-- Code copied from GHC because it is not public in GhcMake module
module Language.Haskell.Tools.Daemon.ModuleGraph
  (moduleGraphNodes, getModFromNode, dependentModules, supportingModules) where

import Control.Monad
import Control.Monad.IO.Class
import Language.Haskell.Tools.Refactor hiding (ModuleName)
import qualified Data.Map as Map (fromList, Map, lookup)
import Data.Maybe (Maybe(..), mapMaybe, catMaybes)

import Digraph as GHC
import FastString as GHC (FastString, fsLit)
import GHC
import HscTypes as GHC

type NodeKey   = (ModuleName, HscSource)
type NodeMap a = Map.Map NodeKey a
type SummaryNode = (ModSummary, Int, [Int])

getModFromNode :: SummaryNode -> ModSummary
getModFromNode (ms, _, _) = ms

-- Creates the dependency graph of modules currently loaded. Used for checking which modules need
-- to be reloaded after a recompilation.
moduleGraphNodes :: Bool -> [ModSummary]
  -> (Graph SummaryNode, HscSource -> ModuleName -> Maybe SummaryNode)
moduleGraphNodes drop_hs_boot_nodes summaries = (graphFromEdgedVerticesOrd nodes, lookup_node)
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

supportingModules :: (ModSummary -> Ghc Bool) -> Ghc [ModSummary]
supportingModules = reachedModules False

dependentModules :: (ModSummary -> Ghc Bool) -> Ghc [ModSummary]
dependentModules = reachedModules True

reachedModules :: Bool -> (ModSummary -> Ghc Bool) -> Ghc [ModSummary]
reachedModules dependent pred = do
  let op = if dependent then transposeG else id
  allMods <- getModuleGraph
  selected <- filterM pred allMods
  let (allModsGraph, lookup) = moduleGraphNodes False allMods
      selectedMods = catMaybes $ map (\ms -> lookup (ms_hsc_src ms) (moduleName $ ms_mod ms)) selected
      recompMods = map (moduleName . ms_mod . getModFromNode) $ reachablesG (op allModsGraph) selectedMods -- TODO: compare on file name
      sortedMods = map getModFromNode $ reverse $ topologicalSortG allModsGraph
      sortedSelectedMods = filter ((`elem` recompMods) . moduleName . ms_mod) sortedMods -- TODO: compare on file name
  return sortedSelectedMods
