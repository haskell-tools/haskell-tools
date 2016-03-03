{-# LANGUAGE CPP, LambdaCase, FlexibleInstances, FlexibleContexts, ViewPatterns, TypeOperators, DefaultSignatures, StandaloneDeriving, DeriveGeneric #-}
module Language.Haskell.Tools.Refactor where

import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.RangeToTemplate
import Language.Haskell.Tools.AnnTrf.RangeToSource
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.PrettyPrint.RoseTree
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor.RangeDebug
import Language.Haskell.Tools.Refactor.RangeDebug.Instances

import GHC
import Outputable
import BasicTypes
import Bag
import Var
import SrcLoc
import Module
import FastString
import HscTypes
import GHC.Paths ( libdir )
 
import Data.List
import GHC.Generics
import Data.StructuralTraversal
import qualified Data.Map as Map
import Data.Maybe
import System.Directory
import Data.IORef
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

import Language.Haskell.Tools.Refactor.DebugGhcAST
import Language.Haskell.Tools.Refactor.OrganizeImports
-- import Language.Haskell.Tools.Refactor.IfToCase
 
import DynFlags
import StringBuffer

instance Show (GenLocated SrcSpan AnnotationComment) where
  show = show . unLoc
  
-- instance Show SourceAndName where
  -- show (SN name range) = maybe "" (showSDocUnsafe . ppr) name ++ shortShowSpan range
 
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
        let r = tm_renamed_source t
        let annots = fst $ pm_annotations $ tm_parsed_module t

        -- trfAst <- runTrf annots $ trfModuleRename (fromJust $ tm_renamed_source t) (pm_parsed_source $ tm_parsed_module t)
        -- let mod = rangeToSource (fromJust $ ms_hspp_buf $ pm_mod_summary p) $ cutUpRanges trfAst
        -- res <- organizeImports mod
        -- liftIO $ putStrLn $ prettyPrint res
        
        -- liftIO $ putStrLn $ prettyPrint $ rangeToSource (fromJust $ ms_hspp_buf $ pm_mod_summary p) $ cutUpRanges $ runTrf annots $ trfModule $ pm_parsed_source $ tm_parsed_module t
        transformed <- runTrf annots $ trfModuleRename (fromJust $ tm_renamed_source t) (pm_parsed_source $ tm_parsed_module t)
        liftIO $ putStrLn $ rangeDebug transformed
        liftIO $ putStrLn "==========="
        let cutUp = cutUpRanges transformed
        liftIO $ putStrLn $ templateDebug cutUp
        liftIO $ putStrLn "==========="
        let sourced = rangeToSource (fromJust $ ms_hspp_buf $ pm_mod_summary p) cutUp
        liftIO $ putStrLn $ sourceTemplateDebug sourced
        liftIO $ putStrLn "==========="
        let prettyPrinted = prettyPrint sourced
        liftIO $ putStrLn prettyPrinted
        liftIO $ putStrLn "==========="
      
deriving instance Generic SrcSpan
deriving instance (Generic sema, Generic src) => Generic (NodeInfo sema src)
deriving instance Generic RangeTemplate
deriving instance Show SemanticInfo
deriving instance Generic SemanticInfo
deriving instance Generic SourceTemplate
deriving instance Generic SpanInfo

getIndices :: StructuralTraversable e => Ann e RangeTemplate -> IO (Ann e ())
getIndices = traverseDown (return ()) (return ()) print
                             
bottomUp :: (StructuralTraversable e, Show a) => Ann e a -> IO (Ann e ())
bottomUp = traverseUp (putStrLn "desc") (putStrLn "asc") print

topDown :: (StructuralTraversable e, Show a) => Ann e a -> IO (Ann e ())
topDown = traverseDown (putStrLn "desc") (putStrLn "asc") print
