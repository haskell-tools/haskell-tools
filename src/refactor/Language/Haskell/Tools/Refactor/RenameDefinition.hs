{-# LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.Tools.Refactor.RenameDefinition (renameDefinition, renameDefinition') where

import Name hiding (Name)
import GHC (Ghc)
import qualified GHC
import OccName
import SrcLoc

import Control.Reference hiding (element)
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Data
import Data.Char
import Data.Maybe
import Data.Generics.Uniplate.Data
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.Refactor.RefactorBase

import Debug.Trace

renameDefinition' :: forall n . (NamedThing n, Data n) => RealSrcSpan -> String -> Ann Module (STWithNames n) -> RefactoredModule n
renameDefinition' sp str mod
  = case (getNodeContaining sp mod :: Maybe (Ann Name (STWithNames n))) >>= getNameInfo of 
      Just n -> renameDefinition n str mod
      Nothing -> refactError "No name is selected"

renameDefinition :: forall n . (NamedThing n, Data n) => GHC.Name -> String -> Ann Module (STWithNames n) -> RefactoredModule n
renameDefinition toChange newName mod
  = if not (nameValid (getOccName toChange) newName) 
       then refactError "The new name is not valid"
       else do (res,defFound) <- runStateT (biplateRef !~ changeName toChange newName $ mod) False
               if not defFound then refactError "The definition to rename was not found"
                               else return res
  where
    changeName :: GHC.Name -> String -> Ann Name (STWithNames n) -> StateT Bool (Refactor n) (Ann Name (STWithNames n))
    changeName toChange str elem 
      = if getNameInfo elem == Just toChange
          then do modify (|| fromMaybe False (elem ^? semantics&isDefined)) 
                  return $ element & unqualifiedName .= mkSimpleName str $ elem
          else let namesInScope = fromMaybe [] (elem ^? semantics & scopedLocals)
                   actualName = maybe toChange getName (elem ^? semantics & nameInfo)
                in if str == occNameString (getOccName actualName) && sameNamespace toChange actualName && conflicts toChange actualName namesInScope
                      then lift $ refactError "The definition clashes with an existing one"
                      else return elem

conflicts :: GHC.Name -> GHC.Name -> [[GHC.Name]] -> Bool
conflicts overwrites overwritten (scopeBlock : scope) 
  | overwritten `elem` scopeBlock && overwrites `notElem` scopeBlock = False
  | overwrites `elem` scopeBlock = True
  | otherwise = conflicts overwrites overwritten scope
conflicts _ _ [] = False

sameNamespace :: GHC.Name -> GHC.Name -> Bool
sameNamespace n1 n2 = occNameSpace (getOccName n1) == occNameSpace (getOccName n2)

-- TODO: change between operator and normal names
nameValid :: OccName -> String -> Bool
nameValid n "" = False
nameValid n str | str `elem` reservedNames = False
  where -- TODO: names reserved by extensions
        reservedNames = [ "case", "class", "data", "default", "deriving", "do", "else", "if", "import", "in", "infix"
                        , "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_"
                        , "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>", "[]"
                        ]
nameValid n (':' : opCtrNameRest)
  = isDataSymOcc n && all isPunctuation opCtrNameRest
nameValid n (c : optNameRest) | isPunctuation c
  = isSymOcc n && not (isDataSymOcc n) && all isPunctuation optNameRest
nameValid n (c : nameRest) | isUpper c
                           = not (isSymOcc n) && (isTcOcc n || isDataOcc n) && all (\c -> isIdStartChar c || isDigit c) nameRest
nameValid n (c : nameRest) | isIdStartChar c 
                           = not (isSymOcc n) && (isVarOcc n || isTvOcc n) && all (\c -> isIdStartChar c || isDigit c) nameRest
nameValid _ _ = False

isIdStartChar c = isLetter c || c == '\'' || c == '_'

