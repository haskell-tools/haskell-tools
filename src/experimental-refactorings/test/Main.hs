{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (assertEqual, testCase)

import GHC hiding (loadModule, ParsedModule)
import GHC.Paths ( libdir )

import Control.Monad (Monad(..))
import Data.Either.Combinators (mapRight)
import System.FilePath (pathSeparator, (</>))
import System.IO

import Language.Haskell.Tools.AST as AST (IdDom)
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.DataToNewtype (dataToNewtype)
import Language.Haskell.Tools.Refactor.Builtin.DollarApp (dollarApp)
import Language.Haskell.Tools.Refactor.Builtin.IfToGuards (ifToGuards)

main :: IO ()
main = defaultMain $ testGroup "experimental refactor tests" functionalTests

functionalTests :: [TestTree]
functionalTests = map makeMiscRefactorTest miscRefactorTests

rootDir = "examples"

miscRefactorTests =
  [ ("Refactor.DataToNewtype.Cases", \_ -> dataToNewtype)
  , ("Refactor.IfToGuards.Simple", \m -> ifToGuards (correctRefactorSpan m $ readSrcSpan "3:11-3:33"))
  , ("Refactor.DollarApp.FirstSingle", \m -> dollarApp (correctRefactorSpan m $ readSrcSpan "5:5-5:12"))
  , ("Refactor.DollarApp.FirstMulti", \m -> dollarApp (correctRefactorSpan m $ readSrcSpan "5:5-5:16"))
  , ("Refactor.DollarApp.InfixOperator", \m -> dollarApp (correctRefactorSpan m $ readSrcSpan "5:5-5:16"))
  , ("Refactor.DollarApp.AnotherOperator", \m -> dollarApp (correctRefactorSpan m $ readSrcSpan "5:5-5:15"))
  -- , ("Refactor.DollarApp.ImportDollar", \m -> dollarApp (correctRefactorSpan m $ readSrcSpan "6:5-6:12"))
  -- IGNORED FOR NOW
  ]

makeMiscRefactorTest :: (String, UnnamedModule -> LocalRefactoring) -> TestTree
makeMiscRefactorTest (moduleName, refact)
  = testCase moduleName $
      do expected <- loadExpected True rootDir moduleName
         res <- testRefactor refact moduleName
         assertEqual "The transformed result is not what is expected" (Right (standardizeLineEndings expected))
                                                                      (mapRight standardizeLineEndings res)

testRefactor :: (UnnamedModule -> LocalRefactoring) -> String -> IO (Either String String)
testRefactor refact moduleName
  = runGhc (Just libdir) $ do
      initGhcFlags
      useDirs [rootDir]
      mod <- loadModule rootDir moduleName >>= parseTyped
      res <- runRefactor (SourceFileKey (rootDir </> moduleSourceFile moduleName) moduleName, mod) [] (localRefactoring $ refact mod)
      case res of Right r -> return $ Right $ prettyPrint $ snd $ fromContentChanged $ head r
                  Left err -> return $ Left err

loadExpected :: Bool -> String -> String -> IO String
loadExpected resSuffix workingDir moduleName =
  do -- need to use binary or line endings will be translated
     expectedHandle <- openBinaryFile (workingDir </> map (\case '.' -> pathSeparator; c -> c) moduleName ++ (if resSuffix then "_res" else "") ++ ".hs") ReadMode
     hGetContents expectedHandle

standardizeLineEndings = filter (/= '\r')
