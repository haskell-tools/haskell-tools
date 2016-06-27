{-# LANGUAGE ScopedTypeVariables
           , LambdaCase
           , MultiWayIf
           #-}
module Language.Haskell.Tools.Refactor.RenameDefinition (renameDefinition, renameDefinition') where

import Name hiding (Name)
import GHC (Ghc, TyThing(..), lookupName)
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
  = case (getNodeContaining sp mod :: Maybe (Ann SimpleName (STWithNames n))) >>= (fmap getName . (^? semantics&nameInfo)) of 
      Just n -> renameDefinition n str mod
      Nothing -> refactError "No name is selected"

renameDefinition :: forall n . (NamedThing n, Data n) => GHC.Name -> String -> Ann Module (STWithNames n) -> RefactoredModule n
renameDefinition toChange newName mod 
    = do nameCls <- classifyName toChange
         (res,defFound) <- runStateT (biplateRef !~ changeName toChange newName $ mod) False
         if | not (nameValid nameCls newName) -> refactError "The new name is not valid"
            | not defFound -> refactError "The definition to rename was not found"
            | otherwise -> return res
  where     
    changeName :: GHC.Name -> String -> Ann SimpleName (STWithNames n) -> StateT Bool (Refactor n) (Ann SimpleName (STWithNames n))
    changeName toChange str elem 
      = if | fmap getName (elem ^? semantics&nameInfo) == Just toChange
               && (elem ^? semantics&isDefined) == Just False
               && any ((str ==) . occNameString . getOccName) (maybe [] head (elem ^? semantics & scopedLocals))
             -> lift $ refactError "The definition clashes with an existing one" -- name clash with an external definition
           | fmap getName (elem ^? semantics&nameInfo) == Just toChange
             -> do modify (|| fromMaybe False (elem ^? semantics&isDefined)) 
                   return $ element & unqualifiedName .= mkNamePart str $ elem
           | let namesInScope = fromMaybe [] (elem ^? semantics & scopedLocals)
                 actualName = maybe toChange getName (elem ^? semantics & nameInfo)
              in str == occNameString (getOccName actualName) && sameNamespace toChange actualName 
                   && conflicts toChange actualName namesInScope
             -> lift $ refactError "The definition clashes with an existing one" -- local name clash
           | otherwise -> return elem

conflicts :: GHC.Name -> GHC.Name -> [[GHC.Name]] -> Bool
conflicts overwrites overwritten (scopeBlock : scope) 
  | overwritten `elem` scopeBlock && overwrites `notElem` scopeBlock = False
  | overwrites `elem` scopeBlock = True
  | otherwise = conflicts overwrites overwritten scope
conflicts _ _ [] = False

sameNamespace :: GHC.Name -> GHC.Name -> Bool
sameNamespace n1 n2 = occNameSpace (getOccName n1) == occNameSpace (getOccName n2)

data NameClass = Variable | Ctor | ValueOperator | DataCtorOperator | SynonymOperator

classifyName :: GHC.Name -> Refactor n NameClass
classifyName n = lookupName n >>= return . \case 
    Just (AnId id) | isop     -> ValueOperator
    Just (AnId id)            -> Variable
    Just (AConLike id) | isop -> DataCtorOperator
    Just (AConLike id)        -> Ctor
    Just (ATyCon id) | isop   -> SynonymOperator
    Just (ATyCon id)          -> Ctor
    Nothing | isop            -> ValueOperator
    Nothing                   -> Variable
  where isop = isSymOcc (getOccName n) 

-- TODO: change between operator and normal names
nameValid :: NameClass -> String -> Bool
nameValid n "" = False
nameValid n str | str `elem` reservedNames = False
  where -- TODO: names reserved by extensions
        reservedNames = [ "case", "class", "data", "default", "deriving", "do", "else", "if", "import", "in", "infix"
                        , "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_"
                        , "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>", "[]"
                        ]
-- Operators that are data constructors (must start with ':')
nameValid DataCtorOperator (':' : nameRest)
  = all isOperatorChar nameRest
-- Type families and synonyms that are operators (can start with ':')
nameValid SynonymOperator (c : nameRest)
  = isOperatorChar c && all isOperatorChar nameRest
-- Normal value operators (cannot start with ':')
nameValid ValueOperator (c : nameRest)
  = isOperatorChar c && c /= ':' && all isOperatorChar nameRest
-- Data and type constructors (start with uppercase)
nameValid Ctor (c : nameRest)
  = isUpper c && isIdStartChar c && all (\c -> isIdStartChar c || isDigit c) nameRest
-- Variables and type variables (start with lowercase)
nameValid Variable (c : nameRest)
  = isLower c && isIdStartChar c && all (\c -> isIdStartChar c || isDigit c) nameRest
nameValid _ _ = False

isIdStartChar c = (isLetter c && isAscii c) || c == '\'' || c == '_'
isOperatorChar c = (isPunctuation c || isSymbol c) && isAscii c

