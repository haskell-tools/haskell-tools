-- | Helper functions for using the AST
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

nameString :: Name a -> String
nameString = concat . intersperse "." . nameElements

nameElements :: Name a -> [String]
nameElements n = (n ^.. qualifiers.annList.traverse.element.simpleNameStr) 
                    ++ [n ^. unqualifiedName.element.simpleNameStr]
                    
nameQualifier :: Name a -> [String]
nameQualifier n = n ^.. qualifiers.annList.traverse.element.simpleNameStr
         
importIsExact :: ImportDecl a -> Bool
importIsExact = isJust . preview (importSpec.annMaybe._Just.element.importSpecList)  
       
importIsHiding :: ImportDecl a -> Bool
importIsHiding = isJust . preview (importSpec.annMaybe._Just.element.importSpecHiding)
         
importExacts :: Fold (ImportDecl a) (IESpec a)
importExacts = importSpec.annMaybe._Just.element.importSpecList.annList.traverse.element
         
importHidings :: Fold (ImportDecl a) (IESpec a)
importHidings = importSpec.annMaybe._Just.element.importSpecList.annList.traverse.element
         
-- | Possible qualifiers to use imported definitions         
importQualifiers :: ImportDecl a -> [[String]]
importQualifiers imp 
  = (if isAnnNothing (imp ^. importQualified) then [[]] else [])
      ++ maybe [] (\n -> [nameElements n]) 
               (imp ^? importAs.annMaybe._Just.element.importRename.element)


                   