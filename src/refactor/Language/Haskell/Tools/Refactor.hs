{-# LANGUAGE StandaloneDeriving
           , DeriveGeneric
           , LambdaCase
           , ScopedTypeVariables
           , BangPatterns
           , MultiWayIf
           #-}
module Language.Haskell.Tools.Refactor (demoRefactor, performRefactor, onlineRefactor, readCommand, readSrcSpan) where

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

import GHC hiding (loadModule)
import Panic (handleGhcException)
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
import Data.Typeable
import Data.Time.Clock
import Data.IORef
import Data.Either.Combinators
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Reference
import Control.Exception
import System.Directory
import System.IO
import System.FilePath
import Data.Generics.Uniplate.Operations

import Language.Haskell.Tools.Refactor.DebugGhcAST
import Language.Haskell.Tools.Refactor.OrganizeImports
import Language.Haskell.Tools.Refactor.GenerateTypeSignature
import Language.Haskell.Tools.Refactor.GenerateExports
import Language.Haskell.Tools.Refactor.RenameDefinition
import Language.Haskell.Tools.Refactor.ExtractBinding
import Language.Haskell.Tools.Refactor.RefactorBase
 
import DynFlags
import StringBuffer            
    

type TemplateWithNames = NodeInfo (SemanticInfo GHC.Name) SourceTemplate
type TemplateWithTypes = NodeInfo (SemanticInfo GHC.Id) SourceTemplate

data RefactorCommand = NoRefactor 
                     | OrganizeImports
                     | GenerateExports
                     | GenerateSignature RealSrcSpan
                     | RenameDefinition RealSrcSpan String
                     | ExtractBinding RealSrcSpan String
    
performCommand :: RefactorCommand -> Ann AST.Module TemplateWithTypes -> Ghc (Either String (Ann AST.Module TemplateWithTypes))
performCommand rf mod = runRefactor mod $ selectCommand rf
  where selectCommand  NoRefactor = return
        selectCommand OrganizeImports = organizeImports
        selectCommand GenerateExports = generateExports 
        selectCommand (GenerateSignature sp) = generateTypeSignature' sp
        selectCommand (RenameDefinition sp str) = renameDefinition' sp str
        selectCommand (ExtractBinding sp str) = extractBinding' sp str

readCommand :: String -> String -> RefactorCommand
readCommand fileName s = case splitOn " " s of 
  [""] -> NoRefactor
  ("OrganizeImports":_) -> OrganizeImports
  ("GenerateExports":_) -> GenerateExports
  ["GenerateSignature", sp] -> GenerateSignature (readSrcSpan fileName sp)
  ["RenameDefinition", sp, name] -> RenameDefinition (readSrcSpan fileName sp) name
  ["ExtractBinding", sp, name] -> ExtractBinding (readSrcSpan fileName sp) name
  
readSrcSpan :: String -> String -> RealSrcSpan
readSrcSpan fileName s = case splitOn "-" s of
  [from,to] -> mkRealSrcSpan (readSrcLoc fileName from) (readSrcLoc fileName to)
  
readSrcLoc :: String -> String -> RealSrcLoc
readSrcLoc fileName s = case splitOn ":" s of
  [line,col] -> mkRealSrcLoc (mkFastString fileName) (read line) (read col)

onlineRefactor :: String -> FilePath -> String -> IO (Either String String)
onlineRefactor command workingDir moduleStr
  = do withBinaryFile fileName WriteMode (`hPutStr` moduleStr)
       modOpts <- runGhc (Just libdir) $ ms_hspp_opts <$> loadModule workingDir moduleName
       if | xopt Opt_Cpp modOpts -> return (Left "The use of C preprocessor is not supported, please turn off Cpp extension")
          | xopt Opt_TemplateHaskell modOpts -> return (Left "The use of Template Haskell is not supported yet, please turn off TemplateHaskell extension")
          | xopt Opt_RecordWildCards modOpts -> return (Left "The scoping rules of RecordWildCards extension are not fully supported")
          | otherwise -> do 
              res <- performRefactor command workingDir moduleName
              removeFile fileName
              return res
  where moduleName = "Test"
        fileName = workingDir </> (moduleName ++ ".hs")

performRefactor :: String -> String -> String -> IO (Either String String)
performRefactor command workingDir target = 
  runGhc (Just libdir) $
     (mapBoth id prettyPrint <$> (refact =<< parseTyped =<< loadModule workingDir target))
  where refact = performCommand (readCommand (map (\case '.' -> '\\'; c -> c) target ++ ".hs") command)

loadModule :: String -> String -> Ghc ModSummary
loadModule workingDir moduleName 
  = do dflags <- getSessionDynFlags
       -- don't generate any code
       setSessionDynFlags 
         $ flip gopt_set Opt_KeepRawTokenStream
         $ flip gopt_set Opt_NoHsMain
         $ dflags { importPaths = [workingDir]
                  , hscTarget = HscInterpreted
                  , ghcLink = LinkInMemory
                  , ghcMode = CompManager 
                  }
       target <- guessTarget moduleName Nothing
       setTargets [target]
       load LoadAllTargets
       getModSummary $ mkModuleName moduleName
    
parseTyped :: ModSummary -> Ghc (Ann AST.Module TemplateWithTypes)
parseTyped modSum = do
  p <- parseModule modSum
  tc <- typecheckModule p
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  rangeToSource srcBuffer . cutUpRanges . fixRanges . placeComments (snd annots) 
    <$> (addTypeInfos (typecheckedSource tc) 
           =<< (runTrf (fst annots) (getPragmaComments $ snd annots)
              $ trfModuleRename (ms_mod $ modSum)
                  (fromJust $ tm_renamed_source tc) 
                  (pm_parsed_source p)))
    
-- | Should be only used for testing
demoRefactor :: String -> String -> String -> IO ()
demoRefactor command workingDir moduleName = 
  runGhc (Just libdir) $ do
    modSum <- loadModule workingDir moduleName
    p <- parseModule modSum
    t <- typecheckModule p
        
    let r = tm_renamed_source t
    let annots = pm_annotations $ tm_parsed_module t

    -- liftIO $ putStrLn $ show annots
    -- liftIO $ putStrLn "==========="
    liftIO $ putStrLn $ show (fromJust $ tm_renamed_source t)
    liftIO $ putStrLn "==========="
    --transformed <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule (pm_parsed_source $ tm_parsed_module t)
    transformed <- addTypeInfos (typecheckedSource t) =<< (runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModuleRename (ms_mod $ modSum) (fromJust $ tm_renamed_source t) (pm_parsed_source $ tm_parsed_module t))
    liftIO $ putStrLn $ rangeDebug transformed
    liftIO $ putStrLn "==========="
    let commented = fixRanges $ placeComments (getNormalComments $ snd annots) transformed
    liftIO $ putStrLn $ rangeDebug commented
    liftIO $ putStrLn "==========="
    let cutUp = cutUpRanges commented
    liftIO $ putStrLn $ templateDebug cutUp
    liftIO $ putStrLn "==========="
    let sourced = rangeToSource (fromJust $ ms_hspp_buf $ pm_mod_summary p) cutUp
    liftIO $ putStrLn $ sourceTemplateDebug sourced
    liftIO $ putStrLn "==========="
    let prettyPrinted = prettyPrint sourced
    liftIO $ putStrLn prettyPrinted
    liftIO $ putStrLn "==========="
    liftIO $ putStrLn $ fromJust $ ml_hs_file $ ms_location modSum
    transformed <- performCommand (readCommand (fromJust $ ml_hs_file $ ms_location modSum) command) sourced
    case transformed of 
      Right correctlyTransformed -> do
        liftIO $ putStrLn "==========="
        liftIO $ putStrLn $ sourceTemplateDebug correctlyTransformed
        liftIO $ putStrLn "==========="
        let prettyPrinted = prettyPrint correctlyTransformed
        liftIO $ putStrLn prettyPrinted
        liftIO $ putStrLn "==========="
      Left transformProblem -> do
        liftIO $ putStrLn "==========="
        liftIO $ putStrLn transformProblem
        liftIO $ putStrLn "==========="
    
      
deriving instance Generic SrcSpan
deriving instance (Generic sema, Generic src) => Generic (NodeInfo sema src)
deriving instance Generic RangeTemplate
deriving instance Generic (SemanticInfo n)
deriving instance Generic SourceTemplate
deriving instance Generic SpanInfo
