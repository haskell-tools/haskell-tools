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
import Data.Either.Combinators
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
import Language.Haskell.Tools.Refactor.GenerateExports
import Language.Haskell.Tools.Refactor.RenameDefinition
import Language.Haskell.Tools.Refactor.ExtractBinding
import Language.Haskell.Tools.Refactor.RefactorBase

main :: IO Counts
main = runTestTT $ TestList $ map makeReprintTest (languageTests 
                                                     ++ organizeImportTests 
                                                     ++ map fst generateSignatureTests 
                                                     ++ generateExportsTests
                                                     ++ map (\(mod,_,_) -> mod) renameDefinitionTests
                                                     ++ map (\(mod,_,_) -> mod) wrongRenameDefinitionTests
                                                     ++ map (\(mod,_,_) -> mod) extractBindingTests
                                                     ++ map (\(mod,_,_) -> mod) wrongExtractBindingTests)
                                ++ map makeCpphsTest cppHsTests
                                ++ map makeInstanceControlTest instanceControlTests
                                ++ map makeOrganizeImportsTest organizeImportTests
                                ++ map makeGenerateSignatureTest generateSignatureTests
                                ++ map makeGenerateExportsTest generateExportsTests
                                ++ map makeRenameDefinitionTest renameDefinitionTests
                                ++ map makeWrongRenameDefinitionTest wrongRenameDefinitionTests
                                ++ map makeExtractBindingTest extractBindingTests
                                ++ map makeWrongExtractBindingTest wrongExtractBindingTests
        
languageTests =
  [ "Decl.AmbiguousFields"
  , "Decl.ClosedTypeFamily"
  , "Decl.CtorOp"
  , "Decl.DataFamily"
  , "Decl.DataType"
  , "Decl.DataTypeDerivings"
  , "Decl.GADT"
  , "Decl.FunBind"
  , "Decl.FunctionalDeps"
  , "Decl.FunGuards"
  , "Decl.LocalBindings"
  , "Decl.LocalFixity"
  , "Decl.InjectiveTypeFamily"
  , "Decl.OperatorBind"
  , "Decl.OperatorDecl"
  , "Decl.ParamDataType"
  , "Decl.PatternBind"
  , "Decl.PatternSynonym"
  , "Decl.RecordPatternSynonyms"
  , "Decl.RecordType"
  , "Decl.RewriteRule"
  , "Decl.StandaloneDeriving"
  , "Decl.TypeClass"
  , "Decl.TypeFamily"
  , "Decl.TypeFamilyKindSig"
  , "Decl.TypeInstance"
  , "Decl.TypeRole"
  , "Decl.TypeSynonym"
  , "Decl.ValBind"
  , "Expr.ArrowNotation"
  , "Expr.Case"
  , "Expr.DoNotation"
  , "Expr.GeneralizedListComp"
  , "Expr.If"
  , "Expr.LambdaCase"
  , "Expr.ListComp"
  , "Expr.MultiwayIf"
  , "Expr.Negate"
  , "Expr.Operator"
  , "Expr.ParenName"
  , "Expr.ParListComp"
  , "Expr.RecordPuns"
  , "Expr.RecordWildcards"
  , "Expr.RecursiveDo"
  , "Expr.Sections"
  , "Expr.StaticPtr"
  , "Expr.TupleSections"
  --, "Expr.UnicodeSyntax"
  , "Module.Simple"
  , "Module.Export"
  , "Module.NamespaceExport"
  , "Module.Import"
  , "Pattern.Backtick"
  , "Pattern.Constructor"
  , "Pattern.Infix"
  , "Pattern.NPlusK"
  , "Pattern.Record"
  --, "TH.QuasiQuote.Use"
  --, "TH.Brackets"
  , "Type.Bang"
  , "Type.Builtin"
  , "Type.Ctx"
  , "Type.ExplicitTypeApplication"
  , "Type.Forall"
  , "Type.Primitives"
  , "Type.TypeOperators"
  , "Type.Wildcard"
  , "Refactor.CommentHandling.CommentTypes"
  , "Refactor.CommentHandling.BlockComments"
  , "Refactor.CommentHandling.Crosslinking"
  , "Refactor.CommentHandling.FunctionArgs"
  ]

cppHsTests = 
  [ "Main"
  , "Language.Preprocessor.Cpphs"
  , "Language.Preprocessor.Unlit"
  , "Language.Preprocessor.Cpphs.CppIfdef"
  , "Language.Preprocessor.Cpphs.HashDefine"
  , "Language.Preprocessor.Cpphs.MacroPass"
  , "Language.Preprocessor.Cpphs.Options"
  , "Language.Preprocessor.Cpphs.Position"
  , "Language.Preprocessor.Cpphs.ReadFirst"
  , "Language.Preprocessor.Cpphs.RunCpphs"
  , "Language.Preprocessor.Cpphs.SymTab"
  , "Language.Preprocessor.Cpphs.Tokenise"
  ]

instanceControlTests = 
  [ "Control.Instances.Test"
  , "Control.Instances.Morph"
  , "Control.Instances.ShortestPath"
  , "Control.Instances.TypeLevelPrelude"
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
  , ("Refactor.GenerateTypeSignature.TypeDefinedInModule", "3:1-3:1")
  , ("Refactor.GenerateTypeSignature.BringToScope.AlreadyQualImport", "6:1-6:2")
  ]

generateExportsTests = 
  [ "Refactor.GenerateExports.Normal"
  , "Refactor.GenerateExports.Operators"
  ]

renameDefinitionTests =
  [ ("Refactor.RenameDefinition.AmbiguousFields", "4:14-4:15", "xx")
  , ("Refactor.RenameDefinition.RecordField", "3:22-3:23", "xCoord")
  , ("Refactor.RenameDefinition.Constructor", "3:14-3:19", "Point2D")
  , ("Refactor.RenameDefinition.Type", "5:16-5:16", "Point2D")
  , ("Refactor.RenameDefinition.Function", "3:1-3:2", "q")
  , ("Refactor.RenameDefinition.QualName", "3:1-3:2", "q")
  , ("Refactor.RenameDefinition.BacktickName", "3:1-3:2", "g")
  , ("Refactor.RenameDefinition.ParenName", "4:3-4:5", "<->")
  , ("Refactor.RenameDefinition.RecordPatternSynonyms", "4:16-4:17", "xx")
  , ("Refactor.RenameDefinition.ClassMember", "7:3-7:4", "q")
  , ("Refactor.RenameDefinition.LocalFunction", "4:5-4:6", "g")
  , ("Refactor.RenameDefinition.Arg", "4:3-4:4", "y")
  , ("Refactor.RenameDefinition.FunTypeVar", "3:6-3:7", "x")
  , ("Refactor.RenameDefinition.FunTypeVarLocal", "5:10-5:11", "b")
  , ("Refactor.RenameDefinition.ClassTypeVar", "3:9-3:10", "f")
  ]

wrongRenameDefinitionTests =
  [ ("Refactor.RenameDefinition.LibraryFunction", "4:5-4:7", "identity")
  , ("Refactor.RenameDefinition.NameClash", "4:1-4:2", "g")
  , ("Refactor.RenameDefinition.WrongName", "4:1-4:2", "F")
  , ("Refactor.RenameDefinition.WrongName", "6:6-6:7", "x")
  , ("Refactor.RenameDefinition.WrongName", "6:10-6:11", "x")
  ]

extractBindingTests =
  [ ("Refactor.ExtractBinding.Simple", "3:19-3:27", "exaggerate")
  , ("Refactor.ExtractBinding.Parentheses", "3:23-3:62", "sqDistance")
  , ("Refactor.ExtractBinding.ClassInstance", "6:30-6:35", "g")
  , ("Refactor.ExtractBinding.Records", "5:6-5:38", "plus")
  , ("Refactor.ExtractBinding.RecordWildcards", "6:6-6:28", "plus")
  ]

wrongExtractBindingTests = 
  [ ("Refactor.ExtractBinding.TooSimple", "3:19-3:20", "x")
  , ("Refactor.ExtractBinding.NameConflict", "3:19-3:27", "stms")
  ]
   
makeOrganizeImportsTest :: String -> Test
makeOrganizeImportsTest mod 
  = TestLabel mod $ TestCase $ checkCorrectlyTransformed organizeImports "examples" mod

makeGenerateSignatureTest :: (String, String) -> Test
makeGenerateSignatureTest (mod, readSrcSpan (toFileName mod) -> rng) 
  = TestLabel mod $ TestCase $ checkCorrectlyTransformed (generateTypeSignature' rng) "examples" mod

makeGenerateExportsTest :: String -> Test
makeGenerateExportsTest mod 
  = TestLabel mod $ TestCase $ checkCorrectlyTransformed generateExports "examples" mod

makeRenameDefinitionTest :: (String, String, String) -> Test
makeRenameDefinitionTest (mod, readSrcSpan (toFileName mod) -> rng, newName) 
  = TestLabel mod $ TestCase $ checkCorrectlyTransformed (renameDefinition' rng newName) "examples" mod


makeWrongRenameDefinitionTest :: (String, String, String) -> Test
makeWrongRenameDefinitionTest (mod, readSrcSpan (toFileName mod) -> rng, newName) 
  = TestLabel mod $ TestCase $ checkTransformFails (renameDefinition' rng newName) "examples" mod

makeExtractBindingTest :: (String, String, String) -> Test
makeExtractBindingTest (mod, readSrcSpan (toFileName mod) -> rng, newName) 
  = TestLabel mod $ TestCase $ checkCorrectlyTransformed (extractBinding' rng newName) "examples" mod
  
makeWrongExtractBindingTest :: (String, String, String) -> Test
makeWrongExtractBindingTest (mod, readSrcSpan (toFileName mod) -> rng, newName) 
  = TestLabel mod $ TestCase $ checkTransformFails (extractBinding' rng newName) "examples" mod

checkCorrectlyTransformed :: (Ann AST.Module TemplateWithTypes -> Refactor GHC.Id (Ann AST.Module TemplateWithTypes)) -> String -> String -> IO ()
checkCorrectlyTransformed transform workingDir moduleName
  = do -- need to use binary or line endings will be translated
       expectedHandle <- openBinaryFile (workingDir ++ "\\" ++ map (\case '.' -> '\\'; c -> c) moduleName ++ "_res.hs") ReadMode
       expected <- hGetContents expectedHandle
       transformed <- runGhc (Just libdir) ((\mod -> mapBoth id prettyPrint <$> (runRefactor mod transform))
                                              =<< parseTyped 
                                              =<< parse workingDir moduleName)
       assertEqual "The transformed result is not what is expected" (Right (standardizeLineEndings expected)) 
                                                                    (mapRight standardizeLineEndings transformed)
       
checkTransformFails :: (Ann AST.Module TemplateWithTypes -> Refactor GHC.Id (Ann AST.Module TemplateWithTypes)) -> String -> String -> IO ()
checkTransformFails transform workingDir moduleName
  = do -- need to use binary or line endings will be translated
       transformed <- runGhc (Just libdir) ((\mod -> mapBoth id prettyPrint <$> (runRefactor mod transform))
                                              =<< parseTyped 
                                              =<< parse workingDir moduleName)
       assertBool "The transform should fail for the given input" (isLeft transformed)

standardizeLineEndings = filter (/= '\r')
       
toFileName mod = "examples\\" ++ map (\case '.' -> '\\'; c -> c) mod ++ ".hs"
       
makeReprintTest :: String -> Test       
makeReprintTest mod = TestLabel mod $ TestCase (checkCorrectlyPrinted "examples" mod)

makeCpphsTest :: String -> Test       
makeCpphsTest mod = TestLabel mod $ TestCase (checkCorrectlyPrinted "examples/CppHs" mod)

makeInstanceControlTest :: String -> Test       
makeInstanceControlTest mod = TestLabel mod $ TestCase (checkCorrectlyPrinted "examples/InstanceControl" mod)

checkCorrectlyPrinted :: String -> String -> IO ()
checkCorrectlyPrinted workingDir moduleName 
  = do -- need to use binary or line endings will be translated
       expectedHandle <- openBinaryFile (workingDir ++ "\\" ++ map (\case '.' -> '\\'; c -> c) moduleName ++ ".hs") ReadMode
       expected <- hGetContents expectedHandle
       (actual, actual', actual'') <- runGhc (Just libdir) $ do
         parsed <- parse workingDir moduleName
         actual <- prettyPrint <$> parseAST parsed
         actual' <- prettyPrint <$> parseRenamed parsed
         actual'' <- prettyPrint <$> parseTyped parsed
         return (actual, actual', actual'')
       assertEqual "The original and the transformed source differ" expected actual
       assertEqual "The original and the transformed source differ" expected actual'
       assertEqual "The original and the transformed source differ" expected actual''
              
parseAST :: ModSummary -> Ghc (Ann AST.Module (NodeInfo (SemanticInfo RdrName) SourceTemplate))
parseAST modSum = do
  p <- parseModule modSum
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  rangeToSource srcBuffer . cutUpRanges . fixRanges . placeComments (snd annots) 
     <$> (runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule $ pm_parsed_source p)
                                 
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
                            
           