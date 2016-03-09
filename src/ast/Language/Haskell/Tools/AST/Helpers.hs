-- | Helper functions for using the AST.
module Language.Haskell.Tools.AST.Helpers where

import Control.Lens hiding (element)
import Data.List
import Data.Maybe
import Data.Function

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Modules
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Lenses

ordByOccurrence :: Name a -> Name a -> Ordering
ordByOccurrence = compare `on` nameElements

-- | The occurrence of the name.
nameString :: Name a -> String
nameString = concat . intersperse "." . nameElements

-- | The qualifiers and the unqualified name
nameElements :: Name a -> [String]
nameElements n = (n ^.. qualifiers.annList.traverse.element.simpleNameStr) 
                    ++ [n ^. unqualifiedName.element.simpleNameStr]

-- | The qualifier of the name
nameQualifier :: Name a -> [String]
nameQualifier n = n ^.. qualifiers.annList.traverse.element.simpleNameStr
         
-- | Does the import declaration import only the explicitly listed elements?
importIsExact :: ImportDecl a -> Bool
importIsExact = isJust . preview (importSpec.annMaybe._Just.element.importSpecList)  
  
-- | Does the import declaration has a 'hiding' clause?
importIsHiding :: ImportDecl a -> Bool
importIsHiding = isJust . preview (importSpec.annMaybe._Just.element.importSpecHiding)
       
-- | All elements that are explicitly listed to be imported in the import declaration
importExacts :: Fold (ImportDecl a) (IESpec a)
importExacts = importSpec.annMaybe._Just.element.importSpecList.annList.traverse.element

-- | All elements that are hidden in an import
importHidings :: Fold (ImportDecl a) (IESpec a)
importHidings = importSpec.annMaybe._Just.element.importSpecList.annList.traverse.element
         
-- | Possible qualifiers to use imported definitions         
importQualifiers :: ImportDecl a -> [[String]]
importQualifiers imp 
  = (if isAnnNothing (imp ^. importQualified) then [[]] else [])
      ++ maybe [] (\n -> [nameElements n]) 
               (imp ^? importAs.annMaybe._Just.element.importRename.element)


                   