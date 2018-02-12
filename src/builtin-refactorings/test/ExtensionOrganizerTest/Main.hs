module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (assertEqual, testCase)

import GHC hiding (loadModule, ModuleName)
import GHC.Paths (libdir)
import Language.Haskell.TH.LanguageExtensions (Extension)
import SrcLoc (SrcSpan(..), srcSpanEndLine)

import Data.List (sort)
import Control.Monad (unless)
import qualified Data.Map.Strict as SMap (Map, map)
import System.FilePath (FilePath, addExtension, (</>))

import ExtensionOrganizerTest.AnnotationParser
import Language.Haskell.Tools.Refactor hiding (ModuleName)
import Language.Haskell.Tools.Refactor.Builtin.OrganizeExtensions

import Control.Reference (_1, (.-))


{- NOTE:
  Exhaustive checks for Pattern type AST nodes are not given for each check.
  We only give exhaustive test cases for nested patterns in bangPatternsTest.
-}

main :: IO ()
main = defaultMain extensionOrganizerTestGroup

extensionOrganizerTestGroup = testGroup "ExtensionOrganizerTest"
  [ mkTests recordWildCardsTest
  , mkTests flexibleInstancesTest
  , mkTests derivingsTest
  , mkTests patternSynonymsTest
  , mkTests bangPatternsTest
  , mkTests templateHaskellTest
  , mkTests viewPatternsTest
  , mkTests lambdaCaseTest
  , mkTests tupleSectionsTest
  , mkNestedTests magicHashTest
  , mkTests functionalDependenciesTest
  , mkTests defaultSignaturesTest
  , mkTests recursiveDoTest
  , mkTests arrowsTest
  , mkTests parallelListCompTest
  , mkTests typeFamiliesTest
  , mkTests multiParamTypeClassesTest
  , mkTests constraintKindsTest
  , mkTests kindSignaturesTest
  , mkTests explicitNamespacesTest
  , mkTests overloadedStringsTest
  , mkTests gadtsTest
  , mkTests existentialQuantificationTest
  , mkTests constrainedClassMethodsTest
  , mkTests multiWayIfTest
  ]

testRoot = "test/ExtensionOrganizerTest"

mkModulePath :: FilePath -> ModuleName -> FilePath
mkModulePath testDir testName = testRoot </> testDir </> testName

type NestedTestSuite = (FilePath, [TestSuite])
type TestSuite       = (FilePath, [TestName])
type TestName        = String
type ModuleName      = String
type Line            = Int
type SimpleMap       = SMap.Map (LogicalRelation Extension) [Line]

spanToLine :: SrcSpan -> Line
spanToLine (RealSrcSpan s) = srcSpanEndLine s

simplifyExtMap :: ExtMap -> SimpleMap
simplifyExtMap = SMap.map (map spanToLine)

getExtensionsFrom :: FilePath -> ModuleName -> IO SimpleMap
getExtensionsFrom dir moduleName = runGhc (Just libdir) $ do
  modAST <- loadModuleAST (testRoot </> dir) moduleName
  exts <- collectExtensions modAST
  return $! simplifyExtMap exts

getLclExtAnnotsFrom :: FilePath -> ModuleName -> IO SimpleMap
getLclExtAnnotsFrom dir moduleName = do
  s <- readFile $ addExtension (mkModulePath dir moduleName) ".hs"
  return $! getLocalExtensionAnnotations s

getGlbExtAnnotsFrom :: FilePath -> ModuleName -> IO [Extension]
getGlbExtAnnotsFrom dir moduleName = do
  s <- readFile $ addExtension (mkModulePath dir moduleName) ".hs"
  return $! getGlobalExtensionAnnotations s

mkTest :: FilePath -> ModuleName -> TestTree
mkTest dir moduleName = testCase moduleName $ mkAssertion dir moduleName

-- | Compares the local annotations with the result of the checker.
-- Also compares the global expected result with minimized extension set.
mkAssertion :: FilePath -> ModuleName -> IO ()
mkAssertion dir moduleName = do
  global <- getGlbExtAnnotsFrom dir moduleName
  local  <- getLclExtAnnotsFrom dir moduleName
  result <- getExtensionsFrom   dir moduleName
  let minimalExts = determineExtensions result
      mapSort = SMap.map sort

  assertEqual "Failure" (mapSort local) (mapSort result)
  unless (null global) $ assertEqual "Failure" (sort global) (sort minimalExts)

mkTests :: TestSuite -> TestTree
mkTests (testDir, tests) = testGroup testDir (map (mkTest testDir) tests)

mkNestedTests :: NestedTestSuite -> TestTree
mkNestedTests (parentDir, suites) = testGroup parentDir nestedTests
  where nestedSuites = map (_1 .- (parentDir </>)) suites
        nestedTests  = map mkTests nestedSuites




recordWildCardsTest :: TestSuite
recordWildCardsTest = (recordWildCardsRoot, recordWildCardsModules)
recordWildCardsRoot = "RecordWildCardsTest"
recordWildCardsModules = [ "InExpression"
                         , "InPattern"
                         ]

flexibleInstancesTest :: TestSuite
flexibleInstancesTest = (flexibleInstancesRoot, flexibleInstancesModules)
flexibleInstancesRoot = "FlexibleInstancesTest"
flexibleInstancesModules = [ "Combined"
                           , "NestedTypes"
                           , "NestedUnitTyCon"
                           , "NestedWiredInType"
                           , "NoOccurence"
                           , "SameTyVars"
                           , "TopLevelTyVar"
                           , "TopLevelWiredInType"
                           , "SynonymCombined"
                           , "SynonymNestedTypes"
                           , "SynonymNestedUnitTyCon"
                           , "SynonymNestedWiredInType"
                           , "SynonymNoOccurence"
                           , "SynonymSameTyVars"
                           , "SynonymTopLevelTyVar"
                           ]

derivingsTest :: TestSuite
derivingsTest = (derivingsRoot, derivingsModules)
derivingsRoot = "DerivingsTest"
derivingsModules = [ "DataDeriving"
                   , "DataDerivingStrategies"
                   , "NewtypeDeriving"
                   , "NewtypeDerivingStrategies"
                   , "StandaloneData"
                   , "StandaloneDataStrategies"
                   , "StandaloneDataSynonyms"
                   , "StandaloneDataSynonymsStrategies"
                   , "StandaloneNewtype"
                   , "StandaloneNewtypeStrategies"
                   , "StandaloneNewtypeAny"
                   , "StandaloneNewtypeSynonyms"
                   , "StandaloneNewtypeSynonymsStrategies"
                   , "StandaloneNewtypeSynonymsAny"
                   ]

patternSynonymsTest :: TestSuite
patternSynonymsTest = (patSynRoot, patSynModules)
patSynRoot = "PatternSynonymsTest"
patSynModules = [ "UniDirectional"
                , "BiDirectional"
                ]

bangPatternsTest :: TestSuite
bangPatternsTest = (bangPatternsRoot, bangPatternsModules)
bangPatternsRoot = "BangPatternsTest"
bangPatternsModules = [ "Combined"
                      , "InAlt"
                      , "InExpr"
                      , "InMatchLhs"
                      , "InPatSynRhs"
                      , "InPattern"
                      , "InRhsGuard"
                      , "InStmt"
                      , "InValueBind"
                      ]

templateHaskellTest :: TestSuite
templateHaskellTest = (thRoot, thModules)
thRoot = "TemplateHaskellTest"
thModules = [ "Quote"
            , "Splice"
            ]

viewPatternsTest :: TestSuite
viewPatternsTest = (vpRoot, vpModules)
vpRoot = "ViewPatternsTest"
vpModules = [ "InAlt"
            , "InExpr"
            , "InMatchLhs"
            , "InMatchLhsNested"
            ]

lambdaCaseTest :: TestSuite
lambdaCaseTest = (lcRoot, lcModules)
lcRoot = "LambdaCaseTest"
lcModules = [ "InCaseRhs"
            , "InCompStmt"
            , "InExpr"
            , "InFieldUpdate"
            , "InPattern"
            , "InRhs"
            , "InRhsGuard"
            , "InStmt"
            , "InTupSecElem"
            ]

tupleSectionsTest :: TestSuite
tupleSectionsTest = (tsRoot, tsModules)
tsRoot = "TupleSectionsTest"
tsModules = [ "InCaseRhs"
            , "InCompStmt"
            , "InExpr"
            , "InFieldUpdate"
            , "InPattern"
            , "InRhs"
            , "InRhsGuard"
            , "InStmt"
            , "InTupSecElem"
            , "NoTupleSections"
            ]


magicHashTest :: NestedTestSuite
magicHashTest = (mhRoot, [magicHashLiteralTest, magicHashNameTest])
mhRoot = "MagicHashTest"


magicHashNameTest :: TestSuite
magicHashNameTest = (mhNameRoot, mhNameModules)
mhNameRoot = "Name"
mhNameModules = [ "InAssertion"
                , "InClassElement"
                , "InDecl"
                , "InDeclHead"
                , "InExpr"
                , "InFieldDecl"
                , "InFieldUpdate"
                , "InFunDeps"
                , "InInstanceHead"
                --, "InKind"        NO PARSE
                , "InMatchLhs"
                , "InPatSynLhs"
                , "InPattern"
                , "InPatternField"
                , "InType"
                --, "InTypeFamily"  NO PARSE
                , "InTypeSig"
                ]

magicHashLiteralTest :: TestSuite
magicHashLiteralTest = (mhLiteralRoot, mhLiteralModules)
mhLiteralRoot = "Literal"
mhLiteralModules = [ "InExpr" ]


functionalDependenciesTest :: TestSuite
functionalDependenciesTest = (fdRoot, fdModules)
fdRoot = "FunctionalDependenciesTest"
fdModules = [ "Basic" ]


defaultSignaturesTest :: TestSuite
defaultSignaturesTest = (dsRoot, dsModules)
dsRoot = "DefaultSignaturesTest"
dsModules = [ "Basic" ]


recursiveDoTest :: TestSuite
recursiveDoTest = (rdRoot, rdModules)
rdRoot = "RecursiveDoTest"
rdModules = [ "MDo"
            , "RecStmt"
            ]


arrowsTest :: TestSuite
arrowsTest = (arrRoot, arrModules)
arrRoot = "ArrowsTest"
arrModules = [ "Basic" ]


parallelListCompTest :: TestSuite
parallelListCompTest = (pcRoot, pcModules)
pcRoot = "ParallelListCompTest"
pcModules = [ "InCaseRhs"
            , "InCompStmt"
            , "InExpr"
            , "InFieldUpdate"
            , "InPattern"
            , "InRhs"
            , "InRhsGuard"
            , "InStmt"
            , "InTupSecElem"
            ]

typeFamiliesTest :: TestSuite
typeFamiliesTest = (tfRoot, tfModules)
tfRoot = "TypeFamiliesTest"
tfModules = [ "AssocDataFamily"
            , "AssocGDataFamily"
            , "AssocTypeFamily"
            , "ClosedTypeFamilyDecl"
            , "DataFamilyDecl"
            , "GDataFamilyDecl"
            , "NestedTypeEqualityContextSynonyms"
            , "NestedTypeEqualitySynonyms"
            , "TypeEqualityContext"
            , "TypeEqualityContextSynonyms"
            , "TypeEquality"
            , "TypeEqualitySynonyms"
            , "TypeEqualityInName"
            , "TypeFamilyDecl"
            ]

multiParamTypeClassesTest :: TestSuite
multiParamTypeClassesTest = (mptcRoot, mptcModules)
mptcRoot = "MultiParamTypeClassesTest"
mptcModules = [ "MultipleTyVars"
              , "ZeroTyVars"
              ]

constraintKindsTest :: TestSuite
constraintKindsTest = (ckRoot, ckModules)
ckRoot = "ConstraintKindsTest"
ckModules = [ "ClassConstraints"
            , "ComplexConstraints"
            , "NotClassConstraints"
            ]

kindSignaturesTest :: TestSuite
kindSignaturesTest = (ksRoot, ksModules)
ksRoot = "KindSignaturesTest"
ksModules = [ "InClassDecl"
            , "InDataDecl"
            , "InForAll"
            , "InTypeFamily"
            , "InTypeSynonym"
            ]

explicitNamespacesTest :: TestSuite
explicitNamespacesTest = (esRoot, esModules)
esRoot = "ExplicitNamespacesTest"
esModules = [ "Import"
            , "Export"
            ]

overloadedStringsTest :: TestSuite
overloadedStringsTest = (osRoot, osModules)
osRoot = "OverloadedStringsTest"
osModules = [ "Synonym"
            , "Newtype"
            , "Other"
            ]

gadtsTest :: TestSuite
gadtsTest = (gadtRoot, gadtModules)
gadtRoot = "GADTsTest"
gadtModules = [ "AssocGDataFamily"
              , "GDataFamilyDecl"
              , "GDataInstDecl"
              , "NormalContext"
              , "NormalExistential"
              , "NormalOnlySyntax"
              , "NormalSpecResultType"
              , "RecordContext"
              , "RecordExistential"
              , "RecordOnlySyntax"
              , "RecordSpecResultType"
              , "TypeEquality"
              , "TypeEqualityWithOnlySyntax"
              ]

existentialQuantificationTest :: TestSuite
existentialQuantificationTest = (eqRoot, eqModules)
eqRoot = "ExistentialQuantificationTest"
eqModules = [ "WithGADT"
            , "WithGADTSyntax"
            , "WithoutGADTSyntax"
            ]

constrainedClassMethodsTest :: TestSuite
constrainedClassMethodsTest = (ccmRoot, ccmModules)
ccmRoot = "ConstrainedClassMethodsTest"
ccmModules = [ "MixedTyVars"
             , "NullaryConstraint"
             , "OnlyClassTyVars"
             ]

multiWayIfTest :: TestSuite
multiWayIfTest = (mwiRoot, mwiModules)
mwiRoot = "MultiWayIfTest"
mwiModules = [ "InCaseRhs"
             , "InCompStmt"
             , "InExpr"
             , "InFieldUpdate"
             , "InPattern"
             , "InRhs"
             , "InRhsGuard"
             , "InStmt"
             , "InTupSecElem"
             ]
