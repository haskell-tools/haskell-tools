{-# LANGUAGE LambdaCase, TypeFamilies #-}
           
module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit

import GHC hiding (loadModule, ParsedModule)
import GHC.Paths ( libdir )

import System.FilePath
import System.IO

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor

main :: IO ()
main = defaultMain nightlyTests

nightlyTests :: TestTree
nightlyTests
  = testGroup "all tests" [ testGroup "reprint tests" (map makeReprintTest languageTests)
                          , testGroup "CppHs tests" $ map makeCpphsTest cppHsTests
                          , testGroup "instance-control tests" $ map makeInstanceControlTest instanceControlTests
                          ]

makeReprintTest :: String -> TestTree
makeReprintTest mod = testCase mod (checkCorrectlyPrinted rootDir mod)

makeCpphsTest :: String -> TestTree
makeCpphsTest mod = testCase mod (checkCorrectlyPrinted (rootDir </> "CppHs") mod)

makeInstanceControlTest :: String -> TestTree
makeInstanceControlTest mod = testCase mod (checkCorrectlyPrinted (rootDir </> "InstanceControl") mod)

checkCorrectlyPrinted :: String -> String -> IO ()
checkCorrectlyPrinted workingDir moduleName
  = do -- need to use binary or line endings will be translated
       expectedHandle <- openBinaryFile (workingDir </> map (\case '.' -> pathSeparator; c -> c) moduleName ++ ".hs") ReadMode
       expected <- hGetContents expectedHandle
       actual <- runGhc (Just libdir) $ do
         parsed <- loadModule workingDir moduleName
         prettyPrint <$> parseTyped parsed
       assertEqual "The original and the transformed source differ" expected actual


rootDir = "examples"

cppHsTests =
  [ "Language.Preprocessor.Cpphs"
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

languageTests =
  [ "CPP.JustEnabled"
  , "CPP.ConditionalCode"
  , "Decl.AmbiguousFields"
  , "Decl.AnnPragma"
  , "Decl.ClassInfix"
  , "Decl.ClosedTypeFamily"
  , "Decl.CompletePragma"
  , "Decl.CtorOp"
  , "Decl.DataFamily"
  , "Decl.DataType"
  , "Decl.DataInstanceGADT"
  , "Decl.DataTypeDerivings"
  , "Decl.DefaultDecl"
  , "Decl.FunBind"
  , "Decl.FunctionalDeps"
  , "Decl.FunGuards"
  , "Decl.GADT"
  , "Decl.GadtConWithCtx"
  , "Decl.InfixAssertion"
  , "Decl.InfixInstances"
  , "Decl.InfixPatSyn"
  , "Decl.InjectiveTypeFamily"
  , "Decl.InlinePragma"
  , "Decl.InstanceFamily"
  , "Decl.InstanceOverlaps"
  , "Decl.InstanceSpec"
  , "Decl.LocalBindings"
  , "Decl.LocalBindingInDo"
  , "Decl.LocalFixity"
  , "Decl.MinimalPragma"
  , "Decl.MultipleFixity"
  , "Decl.MultipleSigs"
  , "Decl.OperatorBind"
  , "Decl.OperatorDecl"
  , "Decl.ParamDataType"
  , "Decl.PatternBind"
  , "Decl.PatternSynonym"
  , "Decl.RecordPatternSynonyms"
  , "Decl.RecordType"
  , "Decl.RewriteRule"
  , "Decl.SpecializePragma"
  , "Decl.StandaloneDeriving"
  , "Decl.TypeClass"
  , "Decl.TypeClassMinimal"
  , "Decl.TypeFamily"
  , "Decl.TypeFamilyKindSig"
  , "Decl.TypeInstance"
  , "Decl.TypeRole"
  , "Decl.TypeSynonym"
  , "Decl.ValBind"
  , "Decl.ViewPatternSynonym"
  , "Expr.ArrowNotation"
  , "Expr.Case"
  , "Expr.DoNotation"
  , "Expr.GeneralizedListComp"
  , "Expr.EmptyCase"
  , "Expr.FunSection"
  , "Expr.If"
  , "Expr.LambdaCase"
  , "Expr.ListComp"
  , "Expr.MultiwayIf"
  , "Expr.Negate"
  , "Expr.Operator"
  , "Expr.ParenName"
  , "Expr.ParListComp"
  , "Expr.PatternAndDo"
  , "Expr.RecordPuns"
  , "Expr.RecordWildcards"
  , "Expr.RecursiveDo"
  , "Expr.Sections"
  , "Expr.SemicolonDo"
  , "Expr.StaticPtr"
  , "Expr.TupleSections"
  , "Expr.UnboxedSum"
  , "Module.Simple"
  , "Module.GhcOptionsPragma"
  , "Module.Export"
  , "Module.ExportSubs"
  , "Module.ExportModifiers"
  , "Module.NamespaceExport"
  , "Module.Import"
  , "Module.ImportOp"
  , "Module.LangPragmas"
  , "Module.PatternImport"
  , "Pattern.Backtick"
  , "Pattern.Constructor"
  -- , "Pattern.ImplicitParams"
  , "Pattern.Infix"
  , "Pattern.NestedWildcard"
  , "Pattern.NPlusK"
  , "Pattern.OperatorPattern"
  , "Pattern.Record"
  , "Pattern.UnboxedSum"
  , "Type.Bang"
  , "Type.Builtin"
  , "Type.Ctx"
  , "Type.ExplicitTypeApplication"
  , "Type.Forall"
  , "Type.Primitives"
  , "Type.TupleAssert"
  , "Type.TypeOperators"
  , "Type.Unpack"
  , "Type.Wildcard"
  , "TH.Brackets"
  , "TH.QuasiQuote.Define"
  , "TH.QuasiQuote.Use"
  , "TH.Splice.Define"
  , "TH.Splice.Use"
  , "TH.CrossDef"
  , "TH.ClassUse"
  , "TH.Splice.UseImported"
  , "TH.Splice.UseQual"
  , "TH.Splice.UseQualMulti"
  , "TH.LocalDefinition"
  , "TH.MultiImport"
  , "TH.NestedSplices"
  , "TH.Quoted"
  , "TH.WithWildcards"
  , "TH.DoubleSplice"
  , "TH.GADTFields"
  , "CommentHandling.CommentTypes"
  , "CommentHandling.BlockComments"
  , "CommentHandling.Crosslinking"
  , "CommentHandling.FunctionArgs"
  ]
