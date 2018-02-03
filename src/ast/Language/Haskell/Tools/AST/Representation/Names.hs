
{-# LANGUAGE ExplicitNamespaces, KindSignatures, MonoLocalBinds, TypeSynonymInstances #-}


-- | Simple AST elements of Haskell
module Language.Haskell.Tools.AST.Representation.Names where

import Language.Haskell.Tools.AST.Ann (Ann, AnnListG(..))

data UOperator dom stage
  = UBacktickOp { _operatorName :: Ann UQualifiedName dom stage
                } -- ^ A normal name used as an operator with backticks: @ a `mod` b @
  | UNormalOp { _operatorName :: Ann UQualifiedName dom stage
              } -- ^ A normal operator used as an operator.

data UName dom stage
  = UParenName { _simpleName :: Ann UQualifiedName dom stage
               } -- ^ Parenthesized name: @ foldl (+) 0 @
  | UNormalName { _simpleName :: Ann UQualifiedName dom stage
                } -- ^ A normal, non-operator name.
  | UImplicitName { _simpleName :: Ann UQualifiedName dom stage
                  } -- ^ Implicit name: @ ?var @

-- | Possible qualified names. Contains also implicit names.
-- Linear implicit parameter: @%x@. Non-linear implicit parameter: @?x@.
data UQualifiedName dom stage
  = UQualifiedName { _qualifiers :: AnnListG UNamePart dom stage
                   , _unqualifiedName :: Ann UNamePart dom stage
                   }

nameFromList :: AnnListG UNamePart dom stage -> UQualifiedName dom stage
nameFromList (AnnListG a xs) | not (null xs)
  = UQualifiedName (AnnListG a (init xs)) (last xs)
nameFromList _ = error "nameFromList: empty list"

-- | Parts of a qualified name.
data UNamePart dom stage
  = UNamePart { _simpleNameStr :: String }

-- | Program elements formatted as string literals (import packages, pragma texts)
data UStringNode dom stage
  = UStringNode { _stringNodeStr :: String }
