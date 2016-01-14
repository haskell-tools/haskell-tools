{-# LANGUAGE CPP, LambdaCase, FlexibleInstances, FlexibleContexts, ViewPatterns, TypeOperators, DefaultSignatures, StandaloneDeriving, DeriveGeneric #-}
module Language.Haskell.Tools.Refactor where

import qualified Language.Haskell.Tools.AST.Module as AST
import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.SourceMap
import Language.Haskell.Tools.AST.Instances
import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AnnTrf.RangeToTemplate
import Language.Haskell.Tools.AnnTrf.RangeToSource
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.PrettyPrint.RoseTree
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor.RangeDebug
import Language.Haskell.Tools.Refactor.RangeDebug.Instances

import GHC
import Outputable
import Bag
import Var
import GHC.Paths ( libdir )
 
import Data.List
import GHC.Generics
import Data.StructuralTraversal
import qualified Data.Map as Map
import Data.Maybe
import System.Directory
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
 
import DynFlags
import StringBuffer

instance Show (GenLocated SrcSpan AnnotationComment) where
  show = show . unLoc
 
analyze :: String -> String -> IO ()
analyze workingDir moduleName = 
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        -- don't generate any code
        setSessionDynFlags $ gopt_set (dflags { importPaths = [workingDir], hscTarget = HscNothing, ghcLink = NoLink }) Opt_KeepRawTokenStream
        target <- guessTarget moduleName Nothing
        setTargets [target]
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName moduleName
        p <- parseModule modSum
        t <- typecheckModule p
        
        let annots = fst $ pm_annotations $ tm_parsed_module t


        liftIO $ putStrLn $ prettyPrint $ rangeToSource (fromJust $ ms_hspp_buf $ pm_mod_summary p) $ cutUpRanges $ runTrf annots $ trfModule $ pm_parsed_source $ tm_parsed_module t
        -- liftIO $ putStrLn $ sourceTemplateDebug $ rangeToSource (fromJust $ ms_hspp_buf $ pm_mod_summary p) $ cutUpRanges $ runTrf annots $ trfModule $ pm_parsed_source $ tm_parsed_module t
        -- liftIO $ putStrLn $ templateDebug $ cutUpRanges $ runTrf annots $ trfModule $ pm_parsed_source $ tm_parsed_module t
        -- liftIO $ putStrLn $ rangeDebug $ runTrf annots $ trfModule $ pm_parsed_source $ tm_parsed_module t
        
        liftIO $ putStrLn "==========="
        
        -- liftIO $ putStrLn $ showSDocUnsafe $ ppr $ pm_parsed_source $ tm_parsed_module t
        -- liftIO $ print $ getLoc $ pm_parsed_source $ tm_parsed_module t
        
        liftIO $ putStrLn "==========="
        
        -- liftIO $ mapM_ print $ Map.toList $ annotationsToSrcMap annots
                
        -- let mod = pm_parsed_source $ tm_parsed_module t
            -- adtName = msum $ map ((\case TyClD (DataDecl {tcdLName = name}) -> Just (unLoc name); _ -> Nothing) . unLoc) (hsmodDecls (unLoc mod))
        -- case adtName of 
          -- Just name -> liftIO $ putStrLn $ showSDocUnsafe $ ppr $ lookupName name
        
        -- liftIO $ putStrLn $ showSDocUnsafe $ ppr $ tm_renamed_source t
        
        -- liftIO $ putStrLn "==========="
        
        -- case tm_renamed_source t of 
          -- Just (renamedMod,_,_,_) -> do
            -- let adtName = msum $ map ((\case DataDecl {tcdLName = name} -> Just (unLoc name); _ -> Nothing) . unLoc) (concatMap group_tyclds $ hs_tyclds renamedMod)
            -- case adtName of 
              -- Just name -> lookupName name >>= liftIO . putStrLn . showSDocUnsafe . ppr . fmap (\(ATyCon tc) -> map varType (tyConTyVars tc))
              -- Nothing -> return ()
        
        -- liftIO $ putStrLn "==========="
        -- liftIO $ putStrLn $ showSDocUnsafe $ ppr $ tm_typechecked_source t
        -- liftIO $ putStrLn "==========="
        -- g <- getModuleGraph
        -- liftIO $ putStrLn $ showSDocUnsafe $ ppr g
      
deriving instance Generic SrcSpan
deriving instance Generic RangeTemplate

getIndices :: StructuralTraversable e => Ann e RangeTemplate -> IO (Ann e ())
getIndices = traverseDown (return ()) (return ()) print
                             
bottomUp :: (StructuralTraversable e, Show a) => Ann e a -> IO (Ann e ())
bottomUp = traverseUp (putStrLn "desc") (putStrLn "asc") print

topDown :: (StructuralTraversable e, Show a) => Ann e a -> IO (Ann e ())
topDown = traverseDown (putStrLn "desc") (putStrLn "asc") print


