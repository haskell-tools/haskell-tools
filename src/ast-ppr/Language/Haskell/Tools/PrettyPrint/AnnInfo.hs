module Language.Haskell.Tools.PrettyPrint.AnnInfo where

import SrcLoc

-- | Annotation information for an AST
class AnnInfo a where

  -- | Generates an information for a node from the information of the closest ancestor
  --  node with it's own info, and the information of it's children.
  generateInfo :: a -> [a] -> a
  
  -- | Generates a node with no info
  noNodeInfo :: a
  
instance AnnInfo SrcSpan where
  generateInfo ancestor [] = ancestor
  generateInfo _ children  = foldl1 combineSrcSpans children
  noNodeInfo               = noSrcSpan