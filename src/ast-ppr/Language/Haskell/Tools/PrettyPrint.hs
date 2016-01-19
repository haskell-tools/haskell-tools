{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, NamedFieldPuns #-}

-- | Pretty printing the AST
module Language.Haskell.Tools.PrettyPrint where

import Language.Haskell.Tools.PrettyPrint.RoseTree
import Language.Haskell.Tools.AnnTrf.SourceTemplate

import Data.Maybe
import Data.Foldable
import Data.StructuralTraversal
import Data.Sequence

-- | Pretty prints an AST by using source templates stored as node info
prettyPrint :: (StructuralTraversable node) => node SourceTemplate -> String
prettyPrint = toList . printRose . toRoseTree
      
-- | Pretty prints a rose tree according to the source templates remainig from the original AST
printRose :: RoseTree SourceTemplate -> Seq Char
-- simple implementation could be optimized a bit
-- warning: the length of the file should not exceed maxbound::Int
printRose (RoseTree (TextElem txt : rest) children) 
  = fromList txt >< printRose (RoseTree rest children) 
printRose (RoseTree (ChildElem _ : rest) (child : children)) 
  = printRose child >< printRose (RoseTree rest children) 
printRose (RoseTree [] []) = empty 
printRose r@(RoseTree (ChildElem _ : rest) []) = error ("More child elem in template than actual children. In: " ++ show r)
printRose r@(RoseTree [] children) = error ("Not all children are used to pretty printing. In: " ++ show r) 
    

