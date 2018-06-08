module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.CPPChecker where

import GHC hiding (Module)
import StringBuffer

import Data.List (isSubsequenceOf)
import Data.String (fromString)
import Text.PortableLines.ByteString.Lazy (lines8)

import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

gblChkCPP :: CheckNode Module
gblChkCPP = conditional gblChkCPP' Cpp

-- TODO (after GHC upgrade): Rewrite this with mgLookupModule
gblChkCPP' :: CheckNode Module
gblChkCPP' m = do
  ms <- getModSummary . moduleName . semanticsModule $ m
  let cppSrc = preprocessedSrc ms
  case ml_hs_file . ms_location $ ms of
    Just fp -> do
      let lines'  = lines8 . fromString
          cppSrc' = lines' . rmDefaultIncludes fp $ cppSrc
          origSrc = lines' . prettyPrint $ m
      when (cppSrc' /= origSrc) (addEvidenceLoc Cpp noSrcSpan)
    _ -> addEvidenceLoc Cpp noSrcSpan
  return m

-- CPP inserts a line before and after the default includes,
-- that contain the filepath of the module.
-- We want to remove these lines, and everything between them.
rmDefaultIncludes :: FilePath -> String -> String
rmDefaultIncludes fp = unlines . tail
                     . dropWhile (not . (fp `isSubsequenceOf`))
                     . tail . lines

-- | Returns the preprocessed source code.
-- If it is not present, it returns an empty string.
preprocessedSrc :: ModSummary -> String
preprocessedSrc = maybe "" strBufToStr . ms_hspp_buf

strBufToStr :: StringBuffer -> String
strBufToStr sb@(StringBuffer _ len _) = lexemeToString sb len
