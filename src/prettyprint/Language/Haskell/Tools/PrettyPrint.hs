{-# LANGUAGE FlexibleContexts, FlexibleInstances, NamedFieldPuns, UndecidableInstances #-}

-- | Pretty printing the AST
module Language.Haskell.Tools.PrettyPrint (prettyPrint, toRoseTree, PrettyPrintProblem(..)) where

import FastString (fsLit)
import SrcLoc

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint.Prepare.SourceTemplate
import Language.Haskell.Tools.PrettyPrint.RoseTree

import Control.Monad.State
import Control.Reference ((^.))
import Data.Foldable (Foldable(..), concat)
import Data.List as List
import Data.List.Split (splitOn)
import Data.Sequence hiding (null, replicate)

-- | Pretty prints an AST by using source templates stored as node info
prettyPrint :: (SourceInfoTraversal node) => node dom SrcTemplateStage -> String
prettyPrint = toList . printRose . toRoseTree

printRose :: RoseTree SrcTemplateStage -> Seq Char
printRose rt = evalState (printRose' startLoc rt) startLoc
  where startLoc = mkRealSrcLoc (fsLit "") 1 1

type PPState = State RealSrcLoc

-- | Pretty prints a rose tree according to the source templates remaining from the original AST
printRose' :: RealSrcLoc -> RoseTree SrcTemplateStage -> PPState (Seq Char)
-- simple implementation could be optimized a bit
-- warning: the length of the file should not exceed maxbound::Int
printRose' parent (RoseTree (RoseSpan (SourceTemplateNode rng elems minInd relInd)) children)
  = do slide <- calculateSlide rng
       let printTemplateElems :: [SourceTemplateElem] -> [RoseTree SrcTemplateStage] -> PPState (Seq Char)
           printTemplateElems (TextElem txtElems _ : rest) children = putString slide min txt >+< printTemplateElems rest children
             where txt = concatMap (^. sourceTemplateText) txtElems
           printTemplateElems (ChildElem : rest) (child : children) = printRose' parent child >+< printTemplateElems rest children
           printTemplateElems [] [] = return empty
           printTemplateElems _ []
             = pprProblem $ "More child elem in template than actual children in: " 
                              ++ shortShowSpanWithFile (srcLocSpan $ RealSrcLoc parent)
           printTemplateElems [] _
             = pprProblem $ "Not all children are used to pretty printing in: " 
                              ++ shortShowSpanWithFile (srcLocSpan $ RealSrcLoc parent)

           min = minInd `max` getPosByRelative parent relInd

       printTemplateElems elems children

printRose' _ (RoseTree (RoseList (SourceTemplateList {})) []) = return empty
printRose' parent (RoseTree (RoseList (SourceTemplateList rng bef aft defSep indented seps minInd relInd)) children)
    = do slide <- calculateSlide rng
         actRng <- get
         let min = minInd `max` getPosByRelative parent relInd
         putString slide min bef
           >+< (maybe printListWithSeps printListWithSepsIndented indented) actRng slide min actualSeps children
           >+< putString slide min aft
  where stringSeps :: [String]
        stringSeps = map (concatMap (^. sourceTemplateText)) (map fst seps)
        actualSeps = case stringSeps of [] -> repeat defSep
                                        _  -> stringSeps ++ repeat (last stringSeps)

printRose' _ (RoseTree (RoseOptional (SourceTemplateOpt {})) []) = return empty
printRose' parent (RoseTree (RoseOptional (SourceTemplateOpt rng bef aft minInd relInd)) [child])
  = do slide <- calculateSlide rng
       actRng <- get
       let min = minInd `max` getPosByRelative parent relInd
       putString slide min bef >+< printRose' actRng child >+< putString slide min aft
printRose' _ (RoseTree (RoseOptional _) _) = pprProblem "More than one child element in an optional node."

getPosByRelative :: RealSrcLoc -> Maybe Int -> Int
getPosByRelative sp (Just i) = srcLocCol sp + i - 1
getPosByRelative _ _ = 0

calculateSlide :: SrcSpan -> PPState Int
calculateSlide (RealSrcSpan originalSpan) = do
  actualSpan <- get
  return $ srcLocCol actualSpan - srcLocCol (realSrcSpanStart originalSpan)
calculateSlide _ = return 0

putString :: Int -> Int -> String -> PPState (Seq Char)
putString slide minInd s
  = do modify $ advanceStr newStr
       return (fromList newStr)
  where start:rest = splitOn "\n" s
        newStr = concat $ List.intersperse ("\n" ++ replicate slide ' ') (start : map (extendToNSpaces minInd) rest)
        extendToNSpaces n str = replicate n ' ' ++ (List.dropWhile (== ' ') $ List.take n str) ++ List.drop n str

advanceStr :: String -> RealSrcLoc -> RealSrcLoc
advanceStr s loc = foldl advanceSrcLoc loc s

untilReaches :: String -> RealSrcLoc -> RealSrcLoc -> (String, Int)
untilReaches s start end
  = let ls = splitOn "\n" s
     in case ls of _:_:_ -> (unlines (init ls) ++)
                              `mapFst` untilReaches' (last ls) (advanceSrcLoc start '\n') end
                   _ -> (s, srcLocCol $ foldl advanceSrcLoc start s)
  where
    untilReaches' [] curr _ = ([], srcLocCol curr)
    untilReaches' (c:rest) curr until | srcLocCol advancedLoc <= srcLocCol until
      = (c:) `mapFst` untilReaches' rest advancedLoc until
      where advancedLoc = advanceSrcLoc curr c
    untilReaches' _ curr _ = ([], srcLocCol curr)

mapFst :: (a -> b) -> (a, x) -> (b, x)
mapFst f (a, x) = (f a, x)

(>+<) :: PPState (Seq Char) -> PPState (Seq Char) -> PPState (Seq Char)
(>+<) = liftM2 (><)

printListWithSeps :: RealSrcLoc -> Int -> Int -> [String] -> [RoseTree SrcTemplateStage] -> PPState (Seq Char)
printListWithSeps = printListWithSeps' (const putString) 0

-- | Prints the elements of a list where the elements must be printed in the same line (do stmts, case alts, let binds, ...)
printListWithSepsIndented :: [Bool] -> RealSrcLoc -> Int -> Int -> [String] -> [RoseTree SrcTemplateStage] -> PPState (Seq Char)
printListWithSepsIndented indentedChildren parent slide minInd seps children
  = do base <- get
       let putCorrectSep i _ min s | isIndented i
              = do curr <- get
                   let (shortened, currCol) = untilReaches s curr base
                   putString 0 min $ shortened ++ replicate (srcLocCol base - currCol) ' '
           putCorrectSep _ slide minInd s = putString slide minInd s
       printListWithSeps' putCorrectSep 0 parent slide minInd seps children
  where -- the ith separator is before the ith element
        isIndented i = case List.drop i indentedChildren of False:_ -> False; _ -> True

printListWithSeps' :: (Int -> Int -> Int -> String -> PPState (Seq Char)) -> Int -> RealSrcLoc -> Int -> Int -> [String] -> [RoseTree SrcTemplateStage] -> PPState (Seq Char)
printListWithSeps' _ _ _ _ _ _ [] = return empty
printListWithSeps' _ _ parent _ _ _ [child] = printRose' parent child
printListWithSeps' putCorrectSep i parent slide minInd (sep:seps) (child:children)
  = printRose' parent child >+< putCorrectSep i slide minInd sep >+< printListWithSeps' putCorrectSep (i+1) parent slide minInd seps children
printListWithSeps' _ _ _ _ _ [] _ = pprProblem "printListWithSeps': the number of elements and separators does not match"
