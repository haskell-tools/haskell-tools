{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, NamedFieldPuns #-}

-- | Pretty printing the AST
module Language.Haskell.Tools.PrettyPrint where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint.RoseTree
import Language.Haskell.Tools.AnnTrf.SourceTemplate

import Control.Lens
import Data.Maybe
import Data.Foldable
import Data.StructuralTraversal
import Data.Sequence

-- | Pretty prints an AST by using source templates stored as node info
prettyPrint :: (StructuralTraversable node, Show sema) => node (NodeInfo sema SourceTemplate) -> String
prettyPrint = toList . printRose . toRoseTree

printRose :: RoseTree (NodeInfo sema SourceTemplate) -> Seq Char      
printRose = printRose' . fmap (view (sourceInfo.sourceTemplateElems))
      
-- | Pretty prints a rose tree according to the source templates remainig from the original AST
printRose' :: RoseTree [SourceTemplateElem] -> Seq Char
-- simple implementation could be optimized a bit
-- warning: the length of the file should not exceed maxbound::Int
printRose' (RoseTree (TextElem txt : rest) children) 
  = fromList txt >< printRose' (RoseTree rest children) 
printRose' (RoseTree (ChildElem : rest) (child : children)) 
  = printRose' child >< printRose' (RoseTree rest children) 
printRose' (RoseTree [ChildListElem] children) = foldl (><) empty (map printRose' children)
printRose' (RoseTree [OptionalChildElem] children) = foldl (><) empty (map printRose' children)
printRose' (RoseTree [] []) = empty 
printRose' r@(RoseTree (ChildElem : rest) []) = error ("More child elem in template than actual children. In: " ++ show r)
printRose' r@(RoseTree [] children) = error ("Not all children are used to pretty printing. In: " ++ show r) 
    

