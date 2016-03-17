{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, NamedFieldPuns #-}

-- | Pretty printing the AST
module Language.Haskell.Tools.PrettyPrint where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint.RoseTree
import Language.Haskell.Tools.AnnTrf.SourceTemplate

import Control.Reference
import Data.Maybe
import Data.Foldable
import Data.StructuralTraversal
import Data.Sequence hiding (null)

-- | Pretty prints an AST by using source templates stored as node info
prettyPrint :: (StructuralTraversable node) => node (NodeInfo sema SourceTemplate) -> String
prettyPrint = toList . printRose . toRoseTree

printRose :: RoseTree (NodeInfo sema SourceTemplate) -> Seq Char      
printRose = printRose' . fmap (^. sourceInfo&sourceTemplateElems)
      
-- | Pretty prints a rose tree according to the source templates remaining from the original AST
printRose' :: RoseTree [SourceTemplateElem] -> Seq Char
-- simple implementation could be optimized a bit
-- warning: the length of the file should not exceed maxbound::Int
printRose' (RoseTree (TextElem txt : rest) children) 
  = fromList txt >< printRose' (RoseTree rest children) 
printRose' (RoseTree (ChildElem : rest) (child : children)) 
  = printRose' child >< printRose' (RoseTree rest children) 
  
printRose' (RoseTree [ChildListElem _ _] []) = empty
printRose' (RoseTree [ChildListElem _ _] [child]) = printRose' child
printRose' (RoseTree [ChildListElem sep []] (child : children@(_:_))) 
  = printRose' child >< fromList sep >< printRose' (RoseTree [ChildListElem sep []] children)
printRose' (RoseTree [ChildListElem s (sep : rest)] (child : children@(_:_))) 
  = printRose' child >< fromList sep
      >< printRose' (RoseTree [ChildListElem s (if null rest then sep : rest else rest)] children)
      
printRose' (RoseTree [OptionalChildElem _ _] []) = empty
printRose' (RoseTree [OptionalChildElem bef aft] [child]) = fromList bef >< printRose' child >< fromList aft
printRose' (RoseTree [OptionalChildElem _ _] _) = error ("More than one child element in an optional node.")
printRose' (RoseTree [] []) = empty 
printRose' r@(RoseTree (ChildElem : rest) []) = error ("More child elem in template than actual children. In: " ++ show r)
printRose' r@(RoseTree [] children) = error ("Not all children are used to pretty printing. In: " ++ show r) 
printRose' r = error ("printRose': unexpected input: " ++ show r) 
    

