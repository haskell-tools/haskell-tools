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
import Language.Haskell.Tools.AnnTrf.SourceTemplate

import Control.Monad.State
import Control.Reference
import Data.Maybe
import Data.List.Split
import Data.Foldable
import Data.StructuralTraversal
import Data.Sequence hiding (null, replicate)

-- | Pretty prints an AST by using source templates stored as node info
prettyPrint :: (StructuralTraversable node) => node (NodeInfo sema SourceTemplate) -> String
prettyPrint = toList . printRose . toRoseTree

printRose :: RoseTree (NodeInfo sema SourceTemplate) -> Seq Char      
printRose rt = evalState (printRose' $ fmap (^. sourceInfo&sourceTemplateElems) rt) (mkRealSrcLoc (fsLit "") 1 1)
       
type PPState = State RealSrcLoc
       
-- | Pretty prints a rose tree according to the source templates remaining from the original AST
printRose' :: RoseTree [SourceTemplateElem] -> PPState (Seq Char)
-- simple implementation could be optimized a bit
-- warning: the length of the file should not exceed maxbound::Int
printRose' (RoseTree (TextElem txt : rest) children) 
  = putString txt >+< printRose' (RoseTree rest children) 
printRose' (RoseTree (ChildElem : rest) (child : children)) 
  = printRose' child >+< printRose' (RoseTree rest children) 
  
printRose' (RoseTree [ChildListElem {}] []) = return empty
printRose' (RoseTree [ChildListElem bef aft defSep indented []] children) 
  = putString bef >+< (if indented then printListWithSepsIndented else printListWithSeps) (repeat defSep) children >+< putString aft
printRose' (RoseTree [ChildListElem bef aft _ indented seps] children) 
  = putString bef >+< (if indented then printListWithSepsIndented else printListWithSeps) (seps ++ repeat (last seps)) children >+< putString aft
      
printRose' (RoseTree [OptionalChildElem _ _] []) = return empty
printRose' (RoseTree [OptionalChildElem bef aft] [child]) = putString bef >+< printRose' child >+< putString aft
printRose' (RoseTree [OptionalChildElem _ _] _) = error "More than one child element in an optional node."
printRose' (RoseTree [] []) = return empty 
printRose' r@(RoseTree (ChildElem : rest) []) = error ("More child elem in template than actual children. In: " ++ show r)
printRose' r@(RoseTree [] children) = error ("Not all children are used to pretty printing. In: " ++ show r) 
printRose' r = error ("printRose': unexpected input: " ++ show r) 
    
putString :: String -> PPState (Seq Char)
putString s = do modify $ advanceStr s
                 return (fromList s)
                  
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
    
printListWithSeps :: [String] -> [RoseTree [SourceTemplateElem]] -> PPState (Seq Char)
printListWithSeps = printListWithSeps' putString

printListWithSepsIndented :: [String] -> [RoseTree [SourceTemplateElem]] -> PPState (Seq Char)
printListWithSepsIndented seps children
  = do base <- get
       let putCorrectSep s = do curr <- get
                                let (shortened, currCol) = untilReaches s curr base
                                putString $ shortened ++ replicate (srcLocCol base - currCol) ' '
       printListWithSeps' putCorrectSep seps children
    
printListWithSeps' :: (String -> PPState (Seq Char)) -> [String] -> [RoseTree [SourceTemplateElem]] -> PPState (Seq Char)
printListWithSeps' _ _ [] = return empty
printListWithSeps' putCorrectSep _ [child] = printRose' child
printListWithSeps' putCorrectSep (sep:seps) (child:children) 
  = printRose' child >+< putCorrectSep sep >+< printListWithSeps' putCorrectSep seps children
    
