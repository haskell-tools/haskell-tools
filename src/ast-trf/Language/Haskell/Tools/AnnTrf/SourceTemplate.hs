
module Language.Haskell.Tools.AnnTrf.SourceTemplate where

-- | A pattern that controls how the original source code can be
-- retrieved from the AST. A source template is assigned to each node.
-- It has holes where the content of an other node should be printed.
type SourceTemplate = [SourceTemplateElem]   
 
data SourceTemplateElem = TextElem String 
                        | ChildElem Int
     deriving (Eq, Ord)
     
isTextElem :: SourceTemplateElem -> Bool
isTextElem (TextElem _) = True
isTextElem _ = False

isChildElem :: SourceTemplateElem -> Bool
isChildElem (ChildElem _) = True
isChildElem _ = False
      
instance Show SourceTemplateElem where
  show (TextElem s) = s
  show (ChildElem ni) = "<" ++ show ni ++ ">"
     
getIndexedNode :: SourceTemplateElem -> Maybe Int
getIndexedNode (ChildElem ni) = Just ni
getIndexedNode _ = Nothing
     
steCombine :: SourceTemplateElem -> SourceTemplateElem -> SourceTemplateElem
steCombine (TextElem s1) (TextElem s2) = TextElem (s1 ++ s2)
steCombine (TextElem "") e = e
steCombine e (TextElem "") = e
steCombine e1 e2 = error $ "Source template elements " 
                             ++ show e1 ++ " and " ++ show e2 
                             ++ " cannot be combined."
