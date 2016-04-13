{-# LANGUAGE LambdaCase
           , ViewPatterns
           #-}
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
import Language.Haskell.Tools.Refactor.GenerateTypeSignature

type TemplateWithNames = NodeInfo (SemanticInfo GHC.Name) SourceTemplate
type TemplateWithTypes = NodeInfo (SemanticInfo GHC.Id) SourceTemplate

main :: IO Counts
main = runTestTT $ TestList $ map makeReprintTest (languageTests ++ organizeImportTests ++ map fst generateSignatureTests)
                               ++ map makeOrganizeImportsTest organizeImportTests
                               ++ map makeGenerateSignatureTest generateSignatureTests
        
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
  , "Decl.PatternSynonym"
  , "Decl.RecordType"
  , "Decl.RewriteRule"
  , "Decl.StandaloneDeriving"
  , "Decl.TypeClass"
  , "Decl.TypeFamily"
  , "Decl.TypeInstance"
  , "Decl.TypeRole"
  , "Decl.TypeSynonym"
  , "Decl.ValBind"
  , "Expr.ArrowNotation"
  , "Expr.DoNotation"
  , "Expr.GeneralizedListComp"
  , "Expr.If"
  , "Expr.ListComp"
  , "Expr.Negate"
  , "Expr.Operator"
  , "Expr.ParListComp"
  , "Expr.RecordWildcards"
  , "Expr.Sections"
  , "Expr.StaticPtr"
  , "Module.Simple"
  , "Module.Export"
  , "Module.Import"
  , "Pattern.Constructor"
  , "Pattern.NPlusK"
  , "TH.QuasiQuote.Use"
  , "TH.Brackets"
  , "Type.Bang"
  , "Type.Builtin"
  , "Type.Ctx"
  , "Type.Forall"
  , "Type.Wildcard"
  , "Refactor.CommentHandling.CommentTypes"
  , "Refactor.CommentHandling.BlockComments"
  , "Refactor.CommentHandling.Crosslinking"
  , "Refactor.CommentHandling.FunctionArgs"
  ]
        
organizeImportTests = 
  [ "Refactor.OrganizeImports.Narrow"
  , "Refactor.OrganizeImports.Reorder"
  , "Refactor.OrganizeImports.Unused"
  , "Refactor.OrganizeImports.Ctor"
  , "Refactor.OrganizeImports.Class"
  , "Refactor.OrganizeImports.Operator"
  , "Refactor.OrganizeImports.SameName"
  , "Refactor.OrganizeImports.Removed"
  ]
  
generateSignatureTests = 
  [ ("Refactor.GenerateTypeSignature.Simple", "3:1-3:10")
  , ("Refactor.GenerateTypeSignature.Function", "3:1-3:15")
  , ("Refactor.GenerateTypeSignature.HigherOrder", "3:1-3:14")
  , ("Refactor.GenerateTypeSignature.Polymorph", "3:1-3:10")
  , ("Refactor.GenerateTypeSignature.PolymorphSub", "5:3-5:4")
  , ("Refactor.GenerateTypeSignature.PolymorphSubMulti", "5:3-5:4")
  , ("Refactor.GenerateTypeSignature.Placement", "4:1-4:10")
  , ("Refactor.GenerateTypeSignature.Tuple", "3:1-3:18")
  , ("Refactor.GenerateTypeSignature.Complex", "3:1-3:21")
  , ("Refactor.GenerateTypeSignature.Local", "4:3-4:12")
  , ("Refactor.GenerateTypeSignature.Let", "3:9-3:18")
  ]
   
makeOrganizeImportsTest :: String -> Test
makeOrganizeImportsTest mod 
  = TestLabel mod $ TestCase $ checkCorrectlyTransformed organizeImports "examples" mod

makeGenerateSignatureTest :: (String, String) -> Test
makeGenerateSignatureTest (mod, readSrcSpan (toFileName mod) -> rng) 
  = TestLabel mod $ TestCase $ checkCorrectlyTransformed trf "examples" mod
  where trf = generateTypeSignature (nodesContaining rng) (nodesContaining rng) (getValBindInList rng)
  
checkCorrectlyTransformed :: (Ann AST.Module TemplateWithTypes -> Ghc (Ann AST.Module TemplateWithTypes)) -> String -> String -> IO ()
checkCorrectlyTransformed transform workingDir moduleName
  = do -- need to use binary or line endings will be translated
       expectedHandle <- openBinaryFile (workingDir ++ "\\" ++ map (\case '.' -> '\\'; c -> c) moduleName ++ "_res.hs") ReadMode
       expected <- hGetContents expectedHandle
       transformed <- runGhc (Just libdir) (return . prettyPrint 
                                              =<< transform 
                                              =<< transformTyped 
                                              =<< parse workingDir moduleName)
       assertEqual "The transformed result is not what is expected" (standardizeLineEndings expected) 
                                                                    (standardizeLineEndings transformed)
       
standardizeLineEndings = filter (/= '\r')
       
toFileName mod = "examples\\" ++ map (\case '.' -> '\\'; c -> c) mod ++ ".hs"
       
makeReprintTest :: String -> Test       
makeReprintTest mod = TestLabel mod $ TestCase (checkCorrectlyPrinted "examples" mod)

checkCorrectlyPrinted :: String -> String -> IO ()
checkCorrectlyPrinted workingDir moduleName 
  = do -- need to use binary or line endings will be translated
       expectedHandle <- openBinaryFile (workingDir ++ "\\" ++ map (\case '.' -> '\\'; c -> c) moduleName ++ ".hs") ReadMode
       expected <- hGetContents expectedHandle
       (actual, actual', actual'') <- runGhc (Just libdir) $ do
         parsed <- parse workingDir moduleName
         actual <- prettyPrint <$> transformParsed parsed
         actual' <- prettyPrint <$> transformRenamed parsed
         actual'' <- prettyPrint <$> transformTyped parsed
         return (actual, actual', actual'')
       assertEqual "The original and the transformed source differ" expected actual
       assertEqual "The original and the transformed source differ" expected actual'
       assertEqual "The original and the transformed source differ" expected actual''
              
transformParsed :: ModSummary -> Ghc (Ann AST.Module (NodeInfo () SourceTemplate))
transformParsed modSum = do
  p <- parseModule modSum
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  rangeToSource srcBuffer . cutUpRanges . fixRanges . placeComments (snd annots) 
     <$> (runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule $ pm_parsed_source p)

transformRenamed :: ModSummary -> Ghc (Ann AST.Module TemplateWithNames)
transformRenamed modSum = do
  p <- parseModule modSum
  tc <- typecheckModule p
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  rangeToSource srcBuffer . cutUpRanges . fixRanges . placeComments (snd annots) 
    <$> (runTrf (fst annots) (getPragmaComments $ snd annots) 
         $ trfModuleRename (ms_mod $ modSum) (fromJust $ tm_renamed_source tc) 
                           (pm_parsed_source p))
                                 
transformTyped :: ModSummary -> Ghc (Ann AST.Module TemplateWithTypes)
transformTyped modSum = do
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
       
parse :: String -> String -> Ghc ModSummary
parse workingDir moduleName = do
  dflags <- getSessionDynFlags
  -- don't generate any code
  setSessionDynFlags $ gopt_set (dflags { importPaths = [workingDir]
                                        , hscTarget = HscAsm -- needed for static pointers
                                        , ghcLink = LinkInMemory
                                        , ghcMode = CompManager 
                                        }) Opt_KeepRawTokenStream
  target <- guessTarget moduleName Nothing
  setTargets [target]
  load LoadAllTargets
  getModSummary $ mkModuleName moduleName
                            
           