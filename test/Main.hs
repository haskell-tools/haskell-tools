{-# LANGUAGE LambdaCase #-}
module Main where

import GHC
import DynFlags
import GHC.Paths ( libdir )

import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import Test.HUnit hiding (test)
import System.IO

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AnnTrf.RangeTemplateToSourceTemplate
import Language.Haskell.Tools.AnnTrf.RangeToRangeTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.PlaceComments
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.OrganizeImports

main :: IO Counts
main = runTestTT $ TestList $ map makeReprintTest (languageTests ++ refactorTests)
                               ++ map makeOrganizeImportsTest refactorTests
        
languageTests =
  [ "CppHsPos"
  , "Decl.ClosedTypeFamily"
  , "Decl.DataFamily"
  , "Decl.DataType"
  , "Decl.DataTypeDerivings"
  , "Decl.FunBind"
  , "Decl.FunGuards"
  , "Decl.LocalBindings"
  , "Decl.OperatorDecl"
  , "Decl.ParamDataType"
  , "Decl.PatternBind"
  , "Decl.RecordType"
  , "Decl.TypeClass"
  , "Decl.TypeFamily"
  , "Decl.TypeInstance"
  , "Decl.TypeSynonym"
  , "Decl.ValBind"
  , "Expr.GeneralizedListComp"
  , "Expr.If"
  , "Expr.ListComp"
  , "Expr.Negate"
  , "Expr.Operator"
  , "Expr.ParListComp"
  , "Expr.RecordWildcards"
  , "Expr.Sections"
  , "Module.Simple"
  , "Module.Export"
  , "Module.Import"
  , "Pattern.Constructor"
  , "Type.Bang"
  , "Type.Ctx"
  , "Type.Forall"
  , "Type.Wildcard"
  , "Refactor.CommentHandling.CommentTypes"
  , "Refactor.CommentHandling.BlockComments"
  , "Refactor.CommentHandling.Crosslinking"
  , "Refactor.CommentHandling.FunctionArgs"
  ]
        
refactorTests = 
  [ "Refactor.OrganizeImports.Narrow"
  , "Refactor.OrganizeImports.Reorder"
  , "Refactor.OrganizeImports.Unused"
  , "Refactor.OrganizeImports.Ctor"
  , "Refactor.OrganizeImports.Class"
  , "Refactor.OrganizeImports.Operator"
  , "Refactor.OrganizeImports.SameName"
  , "Refactor.OrganizeImports.Removed"
  ]
       
type TemplateWithSema = NodeInfo SemanticInfo SourceTemplate
       
makeOrganizeImportsTest :: String -> Test
makeOrganizeImportsTest mod 
  = TestLabel mod $ TestCase $ checkCorrectlyTransformed organizeImports "examples" mod
       
checkCorrectlyTransformed :: (Ann AST.Module TemplateWithSema -> Ghc (Ann AST.Module TemplateWithSema)) -> String -> String -> IO ()
checkCorrectlyTransformed transform workingDir moduleName
  = do -- need to use binary or line endings will be translated
       expectedHandle <- openBinaryFile (workingDir ++ "\\" ++ map (\case '.' -> '\\'; c -> c) moduleName ++ "_res.hs") ReadMode
       expected <- hGetContents expectedHandle
       transformed <- runGhc (Just libdir) (return . prettyPrint 
                                              =<< transform 
                                              =<< transformRenamed 
                                              =<< parse workingDir moduleName)
       assertEqual "The transformed result is not what is expected" expected transformed
       
makeReprintTest :: String -> Test       
makeReprintTest mod = TestLabel mod $ TestCase (checkCorrectlyPrinted "examples" mod)

checkCorrectlyPrinted :: String -> String -> IO ()
checkCorrectlyPrinted workingDir moduleName 
  = do -- need to use binary or line endings will be translated
       expectedHandle <- openBinaryFile (workingDir ++ "\\" ++ map (\case '.' -> '\\'; c -> c) moduleName ++ ".hs") ReadMode
       expected <- hGetContents expectedHandle
       (actual, actual') <- runGhc (Just libdir) $ do
         parsed <- parse workingDir moduleName
         actual <- prettyPrint <$> transformParsed parsed
         actual' <- prettyPrint <$> transformRenamed parsed
         return (actual, actual')
       assertEqual "The original and the transformed source differ" expected actual
       assertEqual "The original and the transformed source differ" expected actual'
              
transformParsed :: ModSummary -> Ghc (Ann AST.Module (NodeInfo () SourceTemplate))
transformParsed modSum = do
  p <- parseModule modSum
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  rangeToSource srcBuffer . cutUpRanges . fixRanges . placeComments (snd annots) 
     <$> (runTrf (fst annots) $ trfModule $ pm_parsed_source p)

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
       
parse :: String -> String -> Ghc ModSummary
parse workingDir moduleName = do
  dflags <- getSessionDynFlags
  -- don't generate any code
  setSessionDynFlags $ gopt_set (dflags { importPaths = [workingDir], hscTarget = HscNothing, ghcLink = NoLink }) Opt_KeepRawTokenStream
  target <- guessTarget moduleName Nothing
  setTargets [target]
  load LoadAllTargets
  getModSummary $ mkModuleName moduleName
                            
           