{-# LANGUAGE CPP
           , LambdaCase
           , FlexibleInstances
           , FlexibleContexts
           , ViewPatterns
           , TypeOperators
           , DefaultSignatures
           , StandaloneDeriving
           , DeriveGeneric
           , RankNTypes 
           , ImpredicativeTypes 
           #-}
module Language.Haskell.Tools.Refactor (performRefactor, organizeImportsStr) where

import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AnnTrf.RangeToRangeTemplate
import Language.Haskell.Tools.AnnTrf.RangeTemplateToSourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.RangeTemplate
import Language.Haskell.Tools.AnnTrf.PlaceComments
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
import Data.List.Split
import GHC.Generics hiding (moduleName)
import Data.StructuralTraversal
import qualified Data.Map as Map
import Data.Maybe
import Data.Time.Clock
import System.Directory
import Data.IORef
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Reference
import System.Directory
import System.FilePath
import Data.Generics.Uniplate.Operations

import Language.Haskell.Tools.Refactor.DebugGhcAST
import Language.Haskell.Tools.Refactor.OrganizeImports
import Language.Haskell.Tools.Refactor.GenerateTypeSignature
-- import Language.Haskell.Tools.Refactor.IfToCase
 
import DynFlags
import StringBuffer
    
organizeImportsStr :: String -> String -> String -> IO String
organizeImportsStr workingDir moduleName sourceText
  = (runGhc (Just libdir) (return . prettyPrint 
                             =<< organizeImports 
                             =<< transformRenamed 
                             =<< loadFile workingDir moduleName sourceText))
     <* removeDirectoryRecursive workingDir
   
type TemplateWithSema = NodeInfo SemanticInfo SourceTemplate
   
transformRenamed :: ModSummary -> Ghc (Ann AST.Module TemplateWithSema)
transformRenamed modSum = do
  p <- parseModule modSum
  tc <- typecheckModule p
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  rangeToSource srcBuffer . cutUpRanges . fixRanges . placeComments (snd annots) 
    <$> (runTrf (fst annots) $ trfModuleRename 
                                 (fromJust $ tm_renamed_source tc) 
                                 (pm_parsed_source p))
       
loadFile :: String -> String -> String -> Ghc ModSummary
loadFile workingDir moduleNameStr sourceText = do
  let moduleName = mkModuleName moduleNameStr
  dflags <- getSessionDynFlags
  -- don't generate any code
  setSessionDynFlags $ gopt_set (dflags { importPaths = [workingDir], hscTarget = HscNothing, ghcLink = NoLink }) Opt_KeepRawTokenStream
  time <- liftIO getCurrentTime
  let target = Target (TargetModule moduleName) True (Just (stringToStringBuffer sourceText, time))
  setTargets [target]
  let newFile = workingDir ++ [pathSeparator] ++ moduleNameSlashes moduleName ++ ".hs"
      newFileDir = dropWhileEnd (/= pathSeparator) newFile 
  liftIO $ createDirectoryIfMissing True newFileDir
  liftIO $ writeFile newFile sourceText
  load LoadAllTargets 
  getModSummary moduleName
                            
    
data RefactorCommand = OrganizeImports
                     | GenerateSignature RealSrcSpan
    
readCommand :: String -> String -> RefactorCommand
readCommand fileName s = case splitOn " " s of 
  ["OrganizeImports"] -> OrganizeImports
  ["GenerateSignature", sp] -> GenerateSignature (readSrcSpan fileName sp)
  
readSrcSpan :: String -> String -> RealSrcSpan
readSrcSpan fileName s = case splitOn "-" s of
  [from,to] -> mkRealSrcSpan (readSrcLoc fileName from) (readSrcLoc fileName to)
  
readSrcLoc :: String -> String -> RealSrcLoc
readSrcLoc fileName s = case splitOn ":" s of
  [line,col] -> mkRealSrcLoc (mkFastString fileName) (read line) (read col)
    
performRefactor :: String -> String -> String -> IO ()
performRefactor command workingDir moduleName = 
  
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
    let annots = pm_annotations $ tm_parsed_module t


    -- liftIO $ putStrLn $ show annots
    -- liftIO $ putStrLn "==========="
    liftIO $ putStrLn $ show (pm_parsed_source $ tm_parsed_module t)
    liftIO $ putStrLn "==========="
    -- transformed <- runTrf (fst annots) $ trfModule (pm_parsed_source $ tm_parsed_module t)
    transformed <- runTrf (fst annots) $ trfModuleRename (fromJust $ tm_renamed_source t) (pm_parsed_source $ tm_parsed_module t)
    liftIO $ putStrLn $ rangeDebug transformed
    liftIO $ putStrLn "==========="
    let commented = fixRanges $ placeComments (snd annots) transformed
    liftIO $ putStrLn $ rangeDebug commented
    liftIO $ putStrLn "==========="
    let cutUp = cutUpRanges commented
    liftIO $ putStrLn $ templateDebug cutUp
    liftIO $ putStrLn "==========="
    -- let locIndices = getLocIndices cutUp
    -- liftIO $ putStrLn $ show locIndices
    -- liftIO $ putStrLn "==========="
    -- let mappedLocs = mapLocIndices (fromJust $ ms_hspp_buf $ pm_mod_summary p) $ getLocIndices cutUp
    -- liftIO $ putStrLn $ show mappedLocs
    -- liftIO $ putStrLn "==========="
    let sourced = rangeToSource (fromJust $ ms_hspp_buf $ pm_mod_summary p) cutUp
    liftIO $ putStrLn $ sourceTemplateDebug sourced
    liftIO $ putStrLn "==========="
    let prettyPrinted = prettyPrint sourced
    liftIO $ putStrLn prettyPrinted
    transformed <- case readCommand (fromJust $ ml_hs_file $ ms_location modSum) command of
      OrganizeImports -> do
        liftIO $ putStrLn "==========="
        organized <- organizeImports sourced
        liftIO $ putStrLn $ sourceTemplateDebug organized
        return organized
      GenerateSignature sp -> do
        liftIO $ putStrLn "==========="
        modified <- generateTypeSignature (nodesInside sp) -- top-level declarations
                                          (nodesInside sp) -- local declarations
                                          (getNode sp) 
                                          sourced
        liftIO $ putStrLn $ sourceTemplateDebug modified
        return modified
    liftIO $ putStrLn "==========="
    let prettyPrinted = prettyPrint transformed
    liftIO $ putStrLn prettyPrinted
    liftIO $ putStrLn "==========="
    
      
deriving instance Generic SrcSpan
deriving instance (Generic sema, Generic src) => Generic (NodeInfo sema src)
deriving instance Generic RangeTemplate
deriving instance Show SemanticInfo
deriving instance Generic SemanticInfo
deriving instance Generic SourceTemplate
deriving instance Generic SpanInfo
