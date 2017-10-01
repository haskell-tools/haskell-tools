{-# LANGUAGE LambdaCase
           , ViewPatterns
           , TypeFamilies
           #-}
module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit

import DynFlags (xopt)
import GHC hiding (loadModule, ParsedModule)
import GHC.Paths ( libdir )
import Module as GHC (mkModuleName)
import StringBuffer (hGetStringBuffer)

import Control.Monad (Monad(..), mapM, (=<<))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State
import Control.Reference ((^.))
import Data.Either.Combinators (mapRight, isLeft)
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (Maybe(..), fromJust)
import Language.Haskell.TH.LanguageExtensions (Extension(..))
import System.Directory (listDirectory)
import System.FilePath
import System.IO

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC (runTrf, trfModule, trfModuleRename)
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings)

main :: IO ()
main = defaultMain nightlyTests

nightlyTests :: TestTree
nightlyTests
  = testGroup "all tests" [ testGroup "functional tests" functionalTests
                          , testGroup "CppHs tests" $ map makeCpphsTest cppHsTests
                          , testGroup "instance-control tests" $ map makeInstanceControlTest instanceControlTests
                          ]

functionalTests :: [TestTree]
functionalTests
  = [ testGroup "reprint tests" (map makeReprintTest checkTestCases)
    , testGroup "refactor tests"
        $ map makeOrganizeImportsTest organizeImportTests
            ++ map makeGenerateSignatureTest generateSignatureTests
            ++ map makeWrongGenerateSigTest wrongGenerateSigTests
            ++ map makeGenerateExportsTest generateExportsTests
            ++ map makeRenameDefinitionTest renameDefinitionTests
            ++ map makeWrongRenameDefinitionTest wrongRenameDefinitionTests
            ++ map makeExtractBindingTest extractBindingTests
            ++ map makeWrongExtractBindingTest wrongExtractBindingTests
            ++ map makeInlineBindingTest inlineBindingTests
            ++ map makeWrongInlineBindingTest wrongInlineBindingTests
            ++ map makeFloatOutTest floatOutTests
            ++ map makeWrongFloatOutTest wrongFloatOutTests
            ++ map (makeMultiModuleTest checkMultiResults) multiModuleTests
            ++ map (makeMultiModuleTest checkMultiFail) wrongMultiModuleTests
    ]
  where checkTestCases = languageTests
                          ++ organizeImportTests
                          ++ map fst generateSignatureTests
                          ++ generateExportsTests
                          ++ map (\(mod,_,_) -> mod) renameDefinitionTests
                          ++ map (\(mod,_,_) -> mod) wrongRenameDefinitionTests
                          ++ map (\(mod,_,_) -> mod) extractBindingTests
                          ++ map (\(mod,_,_) -> mod) wrongExtractBindingTests
                          ++ map (\(mod,_) -> mod) inlineBindingTests

rootDir = "examples"

languageTests =
  [ "CPP.JustEnabled"
  , "CPP.ConditionalCode"
  , "Decl.AmbiguousFields"
  , "Decl.AnnPragma"
  , "Decl.ClassInfix"
  , "Decl.ClosedTypeFamily"
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
  -- , "Expr.ImplicitParams"
  , "Expr.ImplicitParamsOrder"
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
  , "Refactor.CommentHandling.CommentTypes"
  , "Refactor.CommentHandling.BlockComments"
  , "Refactor.CommentHandling.Crosslinking"
  , "Refactor.CommentHandling.FunctionArgs"
  ]

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

organizeImportTests =
  [ "Refactor.OrganizeImports.Narrow"
  , "Refactor.OrganizeImports.Reorder"
  , "Refactor.OrganizeImports.Ctor"
  , "Refactor.OrganizeImports.Class"
  , "Refactor.OrganizeImports.Coerce"
  , "Refactor.OrganizeImports.Fields"
  , "Refactor.OrganizeImports.Operator"
  , "Refactor.OrganizeImports.SameName"
  , "Refactor.OrganizeImports.Removed"
  , "Refactor.OrganizeImports.ReorderGroups"
  , "Refactor.OrganizeImports.ReorderComment"
  , "Refactor.OrganizeImports.KeepCtorOfMarshalled"
  , "Refactor.OrganizeImports.KeepHiding"
  , "Refactor.OrganizeImports.KeepPrelude"
  , "Refactor.OrganizeImports.KeepReexported"
  , "Refactor.OrganizeImports.KeepRenamedReexported"
  , "Refactor.OrganizeImports.MakeExplicit.ImportOne"
  , "Refactor.OrganizeImports.MakeExplicit.ImportThree"
  , "Refactor.OrganizeImports.MakeExplicit.ImportClassFun"
  , "Refactor.OrganizeImports.MakeExplicit.ImportCon"
  , "Refactor.OrganizeImports.MakeExplicit.ImportRecordSel"
  , "Refactor.OrganizeImports.MakeExplicit.ImportUnited"
  , "Refactor.OrganizeImports.MakeExplicit.ImportUnitedCount"
  , "Refactor.OrganizeImports.MakeExplicit.ImportFour"
  , "Refactor.OrganizeImports.MakeExplicit.ImportFunHiddenClass"
  , "Refactor.OrganizeImports.MakeExplicit.ImportFunOutOfClass"
  , "Refactor.OrganizeImports.MakeExplicit.Renamed"
  , "Refactor.OrganizeImports.InstanceCarry.ImportOrphan"
  , "Refactor.OrganizeImports.InstanceCarry.ImportNonOrphan"
  , "Refactor.OrganizeImports.NarrowQual"
  , "Refactor.OrganizeImports.NarrowSpec"
  , "Refactor.OrganizeImports.StandaloneDeriving"
  , "Refactor.OrganizeImports.TemplateHaskell"
  , "Refactor.OrganizeImports.NarrowType"
  , "CPP.BetweenImports"
  , "CPP.ConditionalImport"
  , "CPP.ConditionalImportBegin"
  , "CPP.ConditionalImportEnd"
  , "CPP.ConditionalImportHalfRemoved"
  , "CPP.ConditionalImportMulti"
  , "CPP.ConditionalImportOrder"
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
  , ("Refactor.GenerateTypeSignature.CanCaptureVariable", "8:10-8:10")
  , ("Refactor.GenerateTypeSignature.CanCaptureVariableHasOtherDef", "8:10-8:10")
  ]

wrongGenerateSigTests =
  [ ("Refactor.GenerateTypeSignature.CannotCaptureVariable", "7:10-7:10")
  , ("Refactor.GenerateTypeSignature.CannotCaptureVariableNoExt", "8:10-8:10")
  , ("Refactor.GenerateTypeSignature.ComplexLhs", "3:1")
  , ("Refactor.GenerateTypeSignature.ComplexLhs", "4:1")
  , ("Refactor.GenerateTypeSignature.ComplexLhs", "5:1")
  , ("Refactor.GenerateTypeSignature.ComplexLhs", "6:1")
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
  , ("Refactor.RenameDefinition.AccentName", "3:1-3:2", "á")
  , ("Refactor.RenameDefinition.QualName", "3:1-3:2", "q")
  , ("Refactor.RenameDefinition.BacktickName", "3:1-3:2", "g")
  , ("Refactor.RenameDefinition.ParenName", "4:3-4:5", "<->")
  , ("Refactor.RenameDefinition.RecordWildcards", "4:32-4:33", "yy")
  , ("Refactor.RenameDefinition.RecordPatternSynonyms", "4:16-4:17", "xx")
  , ("Refactor.RenameDefinition.ClassMember", "7:3-7:4", "q")
  , ("Refactor.RenameDefinition.LocalFunction", "4:5-4:6", "g")
  , ("Refactor.RenameDefinition.LayoutAware", "3:1-3:2", "main")
  , ("Refactor.RenameDefinition.FormattingAware", "3:1-3:2", "aa")
  , ("Refactor.RenameDefinition.Arg", "4:3-4:4", "y")
  , ("Refactor.RenameDefinition.FunTypeVar", "3:6-3:7", "x")
  , ("Refactor.RenameDefinition.FunTypeVarLocal", "5:10-5:11", "b")
  , ("Refactor.RenameDefinition.ClassTypeVar", "3:9-3:10", "f")
  , ("Refactor.RenameDefinition.TypeOperators", "4:13-4:15", "x1")
  , ("Refactor.RenameDefinition.NoPrelude", "4:1-4:2", "map")
  , ("Refactor.RenameDefinition.UnusedDef", "3:1-3:2", "map")
  , ("Refactor.RenameDefinition.SameCtorAndType", "3:6-3:13", "P2D")
  , ("Refactor.RenameDefinition.RoleAnnotation", "4:11-4:12", "AA")
  , ("Refactor.RenameDefinition.TypeBracket", "6:6-6:7", "B")
  , ("Refactor.RenameDefinition.ValBracket", "8:11-8:12", "B")
  , ("Refactor.RenameDefinition.FunnyDo", "3:1-3:2", "aaa")
  , ("Refactor.RenameDefinition.RenameModuleAlias", "3:21-3:23", "L")
  , ("Refactor.RenameDefinition.MergeFields", "3:14-3:15", "y")
  , ("Refactor.RenameDefinition.MergeFields_RenameY", "3:34-3:35", "x")
  , ("Refactor.RenameDefinition.PatternSynonym", "6:9", "ArrowAppl")
  , ("Refactor.RenameDefinition.PatternSynonymTypeSig", "6:9", "ArrowAppl")
  ]

wrongRenameDefinitionTests =
  [ ("Refactor.RenameDefinition.LibraryFunction", "4:5-4:7", "identity")
  , ("Refactor.RenameDefinition.NameClash", "5:9-5:10", "h")
  , ("Refactor.RenameDefinition.NameClash", "3:1-3:2", "map")
  , ("Refactor.RenameDefinition.WrongName", "4:1-4:2", "F")
  , ("Refactor.RenameDefinition.WrongName", "4:1-4:2", "++")
  , ("Refactor.RenameDefinition.WrongName", "7:6-7:7", "x")
  , ("Refactor.RenameDefinition.WrongName", "7:6-7:7", ":+:")
  , ("Refactor.RenameDefinition.WrongName", "7:10-7:11", "x")
  , ("Refactor.RenameDefinition.WrongName", "9:6-9:7", "A")
  , ("Refactor.RenameDefinition.WrongName", "9:19-9:19", ".+++.")
  , ("Refactor.RenameDefinition.WrongName", "11:3-11:3", ":+++:")
  , ("Refactor.RenameDefinition.IllegalQualRename", "4:30-4:34", "Bl")
  , ("Refactor.RenameDefinition.CrossRename", "4:1-4:2", "g")
  , ("Refactor.RenameDefinition.MergeFields", "5:16-5:18", "y2") -- fld in the same ctor
  , ("Refactor.RenameDefinition.MergeFields", "5:30-5:32", "x2") -- fld in the same ctor
  , ("Refactor.RenameDefinition.MergeFields", "5:16-5:18", "y") -- fld belongs to other type
  , ("Refactor.RenameDefinition.MergeFields", "7:16-7:18", "y3") -- types does not match
  , ("Refactor.RenameDefinition.MergeFields", "7:38-7:40", "x3") -- types does not match
  ]

extractBindingTests =
  [ ("Refactor.ExtractBinding.Simple", "3:19-3:27", "exaggerate")
  , ("Refactor.ExtractBinding.Parentheses", "3:23-3:62", "sqDistance")
  , ("Refactor.ExtractBinding.AddToExisting", "3:10-3:12", "b")
  , ("Refactor.ExtractBinding.LocalDefinition", "4:13-4:16", "y")
  , ("Refactor.ExtractBinding.ClassInstance", "6:30-6:35", "g")
  , ("Refactor.ExtractBinding.ListComprehension", "5:25-5:39", "notDivisible")
  , ("Refactor.ExtractBinding.Records", "5:5-5:39", "plus")
  , ("Refactor.ExtractBinding.RecordWildcards", "6:5-6:27", "plus")
  , ("Refactor.ExtractBinding.ExistingLocalDef", "3:5-3:10", "a")
  , ("Refactor.ExtractBinding.Indentation", "3:12-3:18", "extracted")
  , ("Refactor.ExtractBinding.IndentationMultiLine", "3:12-3:18", "extracted")
  , ("Refactor.ExtractBinding.IndentationOperator", "3:13-3:20", "extracted")
  , ("Refactor.ExtractBinding.ExtractedFormatting", "4:5-5:7", "extracted")
  , ("Refactor.ExtractBinding.LeftSection", "3:5-3:8", "f")
  , ("Refactor.ExtractBinding.RightSection", "3:7-3:10", "f")
  , ("Refactor.ExtractBinding.SectionWithLocals", "6:13-6:24", "f")
  , ("Refactor.ExtractBinding.SectionInfix", "3:5-3:12", "f")
  , ("Refactor.ExtractBinding.AssocOp", "3:9-3:14", "b")
  , ("Refactor.ExtractBinding.AssocOpRightAssoc", "3:5-3:12", "g")
  , ("Refactor.ExtractBinding.AssocOpMiddle", "3:9-3:14", "b")
  , ("Refactor.ExtractBinding.SiblingDefs", "7:9-7:10", "a")
  ]

wrongExtractBindingTests =
  [ ("Refactor.ExtractBinding.TooSimple", "3:19-3:20", "x")
  , ("Refactor.ExtractBinding.NameConflict", "3:19-3:27", "stms")
  , ("Refactor.ExtractBinding.ViewPattern", "4:4-4:11", "idid")
  ]

inlineBindingTests =
  [ ("Refactor.InlineBinding.Simplest", "4:1-4:2")
  , ("Refactor.InlineBinding.Nested", "4:1-4:2")
  , ("Refactor.InlineBinding.Local", "4:9-4:10")
  , ("Refactor.InlineBinding.LocalNested", "5:17-5:18")
  , ("Refactor.InlineBinding.WithLocals", "4:1-4:2")
  , ("Refactor.InlineBinding.MultiMatch", "4:1-4:2")
  , ("Refactor.InlineBinding.SimpleMultiMatch", "4:1-4:2")
  , ("Refactor.InlineBinding.AlreadyApplied", "4:1-4:2")
  , ("Refactor.InlineBinding.PatternMatched", "4:1-4:2")
  , ("Refactor.InlineBinding.MultiApplied", "4:1-4:2")
  , ("Refactor.InlineBinding.Operator", "4:1-4:2")
  , ("Refactor.InlineBinding.MultiMatchGuarded", "4:1-4:2")
  , ("Refactor.InlineBinding.RemoveSignatures", "5:1-5:2")
  , ("Refactor.InlineBinding.FilterSignatures", "5:1-5:2")
  , ("Refactor.InlineBinding.LetBind", "3:9-3:10")
  , ("Refactor.InlineBinding.LetStmt", "3:12-3:13")
  , ("Refactor.InlineBinding.LetGuard", "3:9-3:10")
  ]

wrongInlineBindingTests =
  [ ("Refactor.InlineBinding.Recursive", "4:1-4:2")
  , ("Refactor.InlineBinding.InExportList", "4:1-4:2")
  , ("Refactor.InlineBinding.NotOccurring", "3:1")
  ]

floatOutTests =
  [ ("Refactor.FloatOut.ToTopLevel", "4:10")
  , ("Refactor.FloatOut.FloatLocals", "5:18")
  , ("Refactor.FloatOut.MoveSignature", "5:10")
  , ("Refactor.FloatOut.MoveFixity", "5:11-5:14")
  , ("Refactor.FloatOut.NoCollosion", "5:18")
  ]

wrongFloatOutTests =
  [ ("Refactor.FloatOut.NameCollosion", "4:10")
  , ("Refactor.FloatOut.NameCollosionWithImport", "4:11")
  , ("Refactor.FloatOut.NameCollosionWithLocal", "5:18")
  , ("Refactor.FloatOut.SharedSignature", "5:10")
  , ("Refactor.FloatOut.ImplicitLocal", "4:10")
  , ("Refactor.FloatOut.ImplicitParam", "4:10")
  ]

multiModuleTests =
  [ ("RenameDefinition 5:5-5:6 bb", "A", "Refactor" </> "RenameDefinition" </> "MultiModule", [])
  , ("RenameDefinition 1:8-1:9 C", "B", "Refactor" </> "RenameDefinition" </> "RenameModule", ["B"])
  , ("RenameDefinition 3:8-3:9 C", "A", "Refactor" </> "RenameDefinition" </> "RenameModule", ["B"])
  , ("RenameDefinition 6:1-6:9 hello", "Use", "Refactor" </> "RenameDefinition" </> "SpliceDecls", [])
  , ("RenameDefinition 5:1-5:5 exprSplice", "Define", "Refactor" </> "RenameDefinition" </> "SpliceExpr", [])
  , ("RenameDefinition 6:1-6:4 spliceTyp", "Define", "Refactor" </> "RenameDefinition" </> "SpliceType", [])
  ]

wrongMultiModuleTests =
  [ ("InlineBinding 3:1-3:2", "A", "Refactor" </> "InlineBinding" </> "AppearsInAnother", [])
  ]

makeMultiModuleTest :: ((String, String, String, [String]) -> Either String [(String, Maybe String)] -> IO ())
                         -> (String, String, String, [String]) -> TestTree
makeMultiModuleTest checker test@(refact, mod, root, removed)
  = testCase (root ++ ":" ++ mod)
      $ do res <- performRefactors refact (rootDir </> root) [] mod
           checker test res

checkMultiResults :: (String, String, String, [String]) -> Either String [(String, Maybe String)] -> IO ()
checkMultiResults _ (Left err) = assertFailure $ "The transformation failed : " ++ err
checkMultiResults test@(_,_,root,removed) (Right ((name, Just mod):rest)) =
  do expected <- loadExpected False ((rootDir </> root) ++ "_res") name
     assertEqual "The transformed result is not what is expected" (standardizeLineEndings expected)
                                                                  (standardizeLineEndings mod)
     checkMultiResults test (Right rest)
checkMultiResults (r,m,root,removed) (Right ((name, Nothing) : rest)) = checkMultiResults (r,m,root,delete name removed) (Right rest)
checkMultiResults (_,_,_,[]) (Right []) = return ()
checkMultiResults (_,_,_,removed) (Right [])
  = assertFailure $ "Modules has not been marked as removed: " ++ concat (intersperse ", " removed)

checkMultiFail :: (String, String, String, [String]) -> Either String [(String, Maybe String)] -> IO ()
checkMultiFail _ (Left _) = return ()
checkMultiFail _ (Right _) = assertFailure "The transformation should fail."

createTest :: String -> [String] -> String -> TestTree
createTest refactoring args mod
  = testCase mod $ checkCorrectlyTransformed (refactoring ++ (concatMap (" "++) args)) rootDir mod

createFailTest :: String -> [String] -> String -> TestTree
createFailTest refactoring args mod
  = testCase mod $ checkTransformFails (refactoring ++ (concatMap (" "++) args)) rootDir mod

makeOrganizeImportsTest :: String -> TestTree
makeOrganizeImportsTest = createTest "OrganizeImports" []

makeGenerateSignatureTest :: (String, String) -> TestTree
makeGenerateSignatureTest (mod, rng) = createTest "GenerateSignature" [rng] mod

makeGenerateExportsTest :: String -> TestTree
makeGenerateExportsTest mod = createTest "GenerateExports" [] mod

makeRenameDefinitionTest :: (String, String, String) -> TestTree
makeRenameDefinitionTest (mod, rng, newName) = createTest "RenameDefinition" [rng, newName] mod

makeWrongRenameDefinitionTest :: (String, String, String) -> TestTree
makeWrongRenameDefinitionTest (mod, rng, newName) = createFailTest "RenameDefinition" [rng, newName] mod

makeWrongGenerateSigTest :: (String, String) -> TestTree
makeWrongGenerateSigTest (mod, rng) = createFailTest "GenerateSignature" [rng] mod

makeExtractBindingTest :: (String, String, String) -> TestTree
makeExtractBindingTest (mod, rng, newName) = createTest "ExtractBinding" [rng, newName] mod

makeWrongExtractBindingTest :: (String, String, String) -> TestTree
makeWrongExtractBindingTest (mod, rng, newName) = createFailTest "ExtractBinding" [rng, newName] mod

makeInlineBindingTest :: (String, String) -> TestTree
makeInlineBindingTest (mod, rng) = createTest "InlineBinding" [rng] mod

makeWrongInlineBindingTest :: (String, String) -> TestTree
makeWrongInlineBindingTest (mod, rng) = createFailTest "InlineBinding" [rng] mod

makeFloatOutTest :: (String, String) -> TestTree
makeFloatOutTest (mod, rng) = createTest "FloatOut" [rng] mod

makeWrongFloatOutTest :: (String, String) -> TestTree
makeWrongFloatOutTest (mod, rng) = createFailTest "FloatOut" [rng] mod

checkCorrectlyTransformed :: String -> String -> String -> IO ()
checkCorrectlyTransformed command workingDir moduleName
  = do expected <- loadExpected True workingDir moduleName
       res <- performRefactor command workingDir [] moduleName
       assertEqual "The transformed result is not what is expected" (Right (standardizeLineEndings expected))
                                                                    (mapRight standardizeLineEndings res)

testRefactor :: (UnnamedModule IdDom -> LocalRefactoring IdDom) -> String -> IO (Either String String)
testRefactor refact moduleName
  = runGhc (Just libdir) $ do
      initGhcFlags
      useDirs [rootDir]
      mod <- loadModule rootDir moduleName >>= parseTyped
      res <- runRefactor (SourceFileKey (rootDir </> moduleSourceFile moduleName) moduleName, mod) [] (localRefactoring $ refact mod)
      case res of Right r -> return $ Right $ prettyPrint $ snd $ fromContentChanged $ head r
                  Left err -> return $ Left err

checkTransformFails :: String -> String -> String -> IO ()
checkTransformFails command workingDir moduleName
  = do res <- performRefactor command workingDir [] moduleName
       assertBool "The transform should fail for the given input" (isLeft res)

loadExpected :: Bool -> String -> String -> IO String
loadExpected resSuffix workingDir moduleName =
  do -- need to use binary or line endings will be translated
     expectedHandle <- openBinaryFile (workingDir </> map (\case '.' -> pathSeparator; c -> c) moduleName ++ (if resSuffix then "_res" else "") ++ ".hs") ReadMode
     hSetEncoding expectedHandle utf8
     hGetContents expectedHandle

standardizeLineEndings = filter (/= '\r')

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
       (actual, actual', actual'') <- runGhc (Just libdir) $ do
         parsed <- loadModule workingDir moduleName
         actual <- prettyPrint <$> parseAST parsed
         actual' <- prettyPrint <$> parseRenamed parsed
         actual'' <- prettyPrint <$> parseTyped parsed
         return (actual, actual', actual'')
       assertEqual "Parsed: The original and the transformed source differ" expected actual
       assertEqual "Renamed: The original and the transformed source differ" expected actual'
       assertEqual "Typechecked: The original and the transformed source differ" expected actual''

performRefactors :: String -> String -> [String] -> String -> IO (Either String [(String, Maybe String)])
performRefactors command workingDir flags target = do
    sourceFiles <- filter ((== ".hs") . takeExtension) <$> listDirectory workingDir
    let sourceModules = map dropExtension sourceFiles
    runGhc (Just libdir) $ do
      initGhcFlagsForTest
      useFlags flags
      useDirs [workingDir]
      setTargets $ map (\mod -> (Target (TargetModule (GHC.mkModuleName mod)) True Nothing)) sourceModules
      load LoadAllTargets
      allMods <- getModuleGraph
      selectedMod <- getModSummary (GHC.mkModuleName target)
      let otherModules = filter (not . (\ms -> ms_mod ms == ms_mod selectedMod && ms_hsc_src ms == ms_hsc_src selectedMod)) allMods
      targetMod <- parseTyped selectedMod
      otherMods <- mapM parseTyped otherModules
      res <- performCommand builtinRefactorings (splitOn " " command)
                            (Right (SourceFileKey (workingDir </> moduleSourceFile target) target, targetMod))
                            (zip (map keyFromMS otherModules) otherMods)
      return $ (\case Right r -> Right $ (map (\case ContentChanged (n,m) -> (n ^. sfkModuleName, Just $ prettyPrint m)
                                                     ModuleCreated n m _ -> (n, Just $ prettyPrint m)
                                                     ModuleRemoved m -> (m, Nothing)
                                              )) r
                      Left l -> Left l)
             $ res

type ParsedModule = Ann AST.UModule (Dom RdrName) SrcTemplateStage

parseAST :: ModSummary -> Ghc ParsedModule
parseAST modSum = do
  let hasStaticFlags = StaticPointers `xopt` ms_hspp_opts modSum
      hasCppExtension = Cpp `xopt` ms_hspp_opts modSum
      ms = if hasStaticFlags then forceAsmGen modSum else modSum
  p <- parseModule ms
  sourceOrigin <- if hasCppExtension then liftIO $ hGetStringBuffer (getModSumOrig ms)
                                     else return (fromJust $ ms_hspp_buf $ pm_mod_summary p)
  let annots = pm_annotations p
  (if hasCppExtension then prepareASTCpp else prepareAST) sourceOrigin . placeComments (fst annots) (getNormalComments $ snd annots)
     <$> (runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule ms $ pm_parsed_source p)

type RenamedModule = Ann AST.UModule (Dom GHC.Name) SrcTemplateStage

parseRenamed :: ModSummary -> Ghc RenamedModule
parseRenamed modSum = do
  let hasStaticFlags = StaticPointers `xopt` ms_hspp_opts modSum
      hasCppExtension = Cpp `xopt` ms_hspp_opts modSum
      ms = if hasStaticFlags then forceAsmGen modSum else modSum
  p <- parseModule ms
  sourceOrigin <- if hasCppExtension then liftIO $ hGetStringBuffer (getModSumOrig ms)
                                     else return (fromJust $ ms_hspp_buf $ pm_mod_summary p)
  tc <- typecheckModule p
  let annots = pm_annotations p
  (if hasCppExtension then prepareASTCpp else prepareAST) sourceOrigin . placeComments (fst annots) (getNormalComments $ snd annots)
    <$> (do parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule ms (pm_parsed_source p)
            runTrf (fst annots) (getPragmaComments $ snd annots)
              $ trfModuleRename ms parseTrf
                  (fromJust $ tm_renamed_source tc)
                  (pm_parsed_source p))

performRefactor :: String -> FilePath -> [String] -> String -> IO (Either String String)
performRefactor command workingDir flags target =
  runGhc (Just libdir) $ do
    useFlags flags
    ((\case Right r -> Right (newContent r); Left l -> Left l) <$> (refact =<< parseTyped =<< loadModule workingDir target))
  where refact m = performCommand builtinRefactorings (splitOn " " command)
                                  (Right (SourceFileKey (workingDir </> moduleSourceFile target) target,m)) []
        newContent (ContentChanged (_, newContent) : ress) = prettyPrint newContent
        newContent ((ModuleCreated _ newContent _) : ress) = prettyPrint newContent
        newContent (_ : ress) = newContent ress
