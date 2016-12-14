{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , NamedFieldPuns 
           #-}

-- | Pretty printing the AST
module Language.Haskell.Tools.PrettyPrint (prettyPrint) where

import SrcLoc
import FastString

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint.RoseTree
import Language.Haskell.Tools.Transform.SourceTemplate

import Control.Monad.State
import Control.Reference
import Data.Maybe
import Data.List as List
import Data.List.Split
import Data.Foldable
import Data.Sequence hiding (null, replicate)
import Language.Haskell.Tools.AST

import Debug.Trace

-- | Pretty prints an AST by using source templates stored as node info
prettyPrint :: (SourceInfoTraversal node) => node dom SrcTemplateStage -> String
prettyPrint = toList . printRose . toRoseTree

printRose :: RoseTree SrcTemplateStage -> Seq Char      
printRose rt = evalState (printRose' rt) (mkRealSrcLoc (fsLit "") 1 1)
       
type PPState = State RealSrcLoc

-- | Pretty prints a rose tree according to the source templates remaining from the original AST
printRose' :: RoseTree SrcTemplateStage -> PPState (Seq Char)
-- simple implementation could be optimized a bit
-- warning: the length of the file should not exceed maxbound::Int
printRose' (RoseTree (RoseSpan (SourceTemplateNode rng elems minInd)) children) 
  = do slide <- calculateSlide rng
       printTemplateElems slide elems children
  where printTemplateElems :: Int -> [SourceTemplateElem] -> [RoseTree SrcTemplateStage] -> PPState (Seq Char)
        printTemplateElems slide (TextElem txt : rest) children = putString slide minInd txt >+< printTemplateElems slide rest children
        printTemplateElems slide (ChildElem : rest) (child : children) = printRose' child >+< printTemplateElems slide rest children
        printTemplateElems _ [] [] = return empty
        printTemplateElems _ _ [] = error $ "More child elem in template than actual children (elems: " ++ show elems ++ ", children: " ++ show children ++ ")"
        printTemplateElems _ [] _ = error $ "Not all children are used to pretty printing. (elems: " ++ show elems ++ ", children: " ++ show children ++ ")"
  
printRose' (RoseTree (RoseList (SourceTemplateList {})) []) = return empty
printRose' (RoseTree (RoseList (SourceTemplateList rng bef aft defSep indented seps minInd)) children) 
    = do slide <- calculateSlide rng
         putString slide minInd bef >+< (if indented then printListWithSepsIndented else printListWithSeps) slide minInd actualSeps children >+< putString slide minInd aft
  where actualSeps = case seps of [] -> repeat defSep
                                  _  -> seps ++ repeat (last seps)

printRose' (RoseTree (RoseOptional (SourceTemplateOpt {})) []) = return empty
printRose' (RoseTree (RoseOptional (SourceTemplateOpt rng bef aft minInd)) [child]) 
  = do slide <- calculateSlide rng
       putString slide minInd bef >+< printRose' child >+< putString slide minInd aft
printRose' (RoseTree (RoseOptional _) _) = error "More than one child element in an optional node."
    
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
        newStr = concat $ intersperse ("\n" ++ replicate slide ' ') (start : map (extendToNSpaces minInd) rest)
        extendToNSpaces n str = replicate n ' ' ++ (List.dropWhile (== ' ') $ List.take n str) ++ List.drop n str
                  
advanceStr :: String -> RealSrcLoc -> RealSrcLoc
advanceStr s loc = foldl advanceSrcLoc loc s

untilReaches :: String -> RealSrcLoc -> RealSrcLoc -> (String, Int)
untilReaches s start end 
  = let ls = splitOn "\n" s 
     in case ls of _:_:_ -> (unlines (init ls) ++) 
                              `mapFst` untilReaches' (last ls) (advanceSrcLoc start '\n') end 
                   _ -> (s, srcLocCol start)
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
    
printListWithSeps :: Int -> Int -> [String] -> [RoseTree SrcTemplateStage] -> PPState (Seq Char)
printListWithSeps = printListWithSeps' putString

-- | Prints the elements of a list where the elements must be printed in the same line (do stmts, case alts, let binds, ...)
printListWithSepsIndented :: Int -> Int -> [String] -> [RoseTree SrcTemplateStage] -> PPState (Seq Char)
printListWithSepsIndented slide minInd seps children
  = do base <- get
       let putCorrectSep _ min s = do curr <- get
                                      let (shortened, currCol) = untilReaches s curr base
                                      putString 0 min $ shortened ++ replicate (srcLocCol base - currCol) ' '
       printListWithSeps' putCorrectSep slide minInd seps children
    
printListWithSeps' :: (Int -> Int -> String -> PPState (Seq Char)) -> Int -> Int -> [String] -> [RoseTree SrcTemplateStage] -> PPState (Seq Char)
printListWithSeps' _ _ _ _ [] = return empty
printListWithSeps' putCorrectSep _ _ _ [child] = printRose' child
printListWithSeps' putCorrectSep slide minInd (sep:seps) (child:children) 
  = printRose' child >+< putCorrectSep slide minInd sep >+< printListWithSeps' putCorrectSep slide minInd seps children
    
