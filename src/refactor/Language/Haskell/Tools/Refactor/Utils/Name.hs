{-# LANGUAGE LambdaCase #-}

-- | Defines utility operations on Haskell names such as checking if a given identifier is a
-- correct name for a certain kind of Haskell construct.
module Language.Haskell.Tools.Refactor.Utils.Name where

import Data.Char
import Data.List.Split (splitOn)

import GHC hiding (mkModuleName, moduleNameString)
import Name as GHC (NamedThing(..), Name, isSymOcc)

import Language.Haskell.Tools.Refactor.Monad (RefactorMonad(..))

-- | Different classes of definitions that have different kind of names.
data NameClass = Variable         -- ^ Normal value definitions: functions, variables
               | Ctor             -- ^ Data constructors
               | ValueOperator    -- ^ Functions with operator-like names
               | DataCtorOperator -- ^ Constructors with operator-like names
               | SynonymOperator  -- ^ UType definitions with operator-like names

-- | Get which category does a given name belong to
classifyName :: RefactorMonad m => GHC.Name -> m NameClass
classifyName n = liftGhc (lookupName n) >>= return . \case
    Just (AnId {}) | isop     -> ValueOperator
    Just (AnId {})            -> Variable
    Just (AConLike {}) | isop -> DataCtorOperator
    Just (AConLike {})        -> Ctor
    Just (ATyCon {}) | isop   -> SynonymOperator
    Just (ATyCon {})          -> Ctor
    Just (ACoAxiom {})        -> error "classifyName: ACoAxiom"
    Nothing | isop            -> ValueOperator
    Nothing                   -> Variable
  where isop = GHC.isSymOcc (GHC.getOccName n)

-- | Checks if a given name is a valid module name
validModuleName :: String -> Maybe String
validModuleName s = foldl mappend mempty $ map (nameValid Ctor) (splitOn "." s)

-- | Check if a given name is valid for a given kind of definition
nameValid :: NameClass -> String -> Maybe String
nameValid _ "" = Just "An empty name is not valid"
nameValid _ str | str `elem` reservedNames = Just $ "'" ++ str ++ "' is a reserved name"
  where -- TODO: names reserved by extensions
        reservedNames = [ "case", "class", "data", "default", "deriving", "do", "else", "if", "import", "in", "infix"
                        , "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_"
                        , "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>", "[]"
                        ]
-- Operators that are data constructors (must start with ':')
nameValid DataCtorOperator (':' : nameRest) | all isOperatorChar nameRest = Nothing
nameValid DataCtorOperator _ = Just "A constructor operator must start with ':' and only contain operator characters."
-- Type families and synonyms that are operators (can start with ':')
nameValid SynonymOperator name | all isOperatorChar name = Nothing
nameValid SynonymOperator _ = Just "An operator must only contain operator characters."
-- Normal value operators (cannot start with ':')
nameValid ValueOperator (c : nameRest) | isOperatorChar c && c /= ':' && all isOperatorChar nameRest = Nothing
nameValid ValueOperator _ = Just "An operator that is a value must only contain operator characters and cannot start with ':'"
-- Data and type constructors (start with uppercase)
nameValid Ctor (c : nameRest) | isUpper c && isLetter c && all isIdChar nameRest = Nothing
nameValid Ctor _ = Just "A constructor or module name must start with an uppercase letter, and only contain letters, digits, apostrhophe or underscore"
-- Variables and type variables (start with lowercase)
nameValid Variable (c : nameRest) | ((isLower c  && isLetter c) || c == '\'' || c == '_') && all isIdChar nameRest = Nothing
nameValid Variable _ = Just "The name of a value must start with lowercase, and only contain letters, digits, apostrhophe or underscore"

isIdChar :: Char -> Bool
isIdChar c = isLetter c || isDigit c || c == '\'' || c == '_'

isOperatorChar :: Char -> Bool
isOperatorChar c = isPunctuation c || isSymbol c
