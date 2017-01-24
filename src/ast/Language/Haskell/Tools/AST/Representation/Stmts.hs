-- | Representation of Haskell statements (both do-notation and comprehensions)
module Language.Haskell.Tools.AST.Representation.Stmts where

import Language.Haskell.Tools.AST.Ann (Ann, AnnListG, AnnMaybeG)
import {-# SOURCE #-} Language.Haskell.Tools.AST.Representation.Binds (ULocalBind)
import {-# SOURCE #-} Language.Haskell.Tools.AST.Representation.Exprs (UExpr, UCmd)
import Language.Haskell.Tools.AST.Representation.Patterns (UPattern)

-- | Normal monadic statements
data UStmt' expr dom stage
  = UBindStmt { _stmtPattern :: Ann UPattern dom stage
              , _stmtExpr :: Ann expr dom stage
              } -- ^ Binding statement (@ x <- action @)
  | UExprStmt { _stmtExpr :: Ann expr dom stage
              } -- ^ Non-binding statement (@ action @)
  | ULetStmt  { _stmtBinds :: AnnListG ULocalBind dom stage
              } -- ^ Let statement (@ let x = 3; y = 4 @)
  | URecStmt  { _cmdStmtBinds :: AnnListG (UStmt' expr) dom stage
              } -- ^ A recursive binding statement with (@ rec b <- f a c; c <- f b a @)
type UStmt = UStmt' UExpr
type UCmdStmt = UStmt' UCmd

-- | Body of a list comprehension: (@ | x <- [1..10] @)
data UListCompBody dom stage
  = UListCompBody { _compStmts :: AnnListG UCompStmt dom stage
                  } 
         
-- | List comprehension statement
data UCompStmt dom stage
  = UCompStmt  { _compStmt :: Ann UStmt dom stage
               } -- ^ Normal monadic statement of a list comprehension
  | UThenStmt  { _thenExpr :: Ann UExpr dom stage
               , _byExpr :: AnnMaybeG UExpr dom stage
               } -- ^ Then statements by @TransformListComp@ (@ then sortWith by (x + y) @)
  | UGroupStmt { _byExpr :: AnnMaybeG UExpr dom stage
               , _usingExpr :: AnnMaybeG UExpr dom stage
               } -- ^ Grouping statements by @TransformListComp@ (@ then group by (x + y) using groupWith @) 
                 -- Note: either byExpr or usingExpr must have a value

-- | Keywords @do@ or @mdo@ to start a do-block
data UDoKind dom stage
  = UDoKeyword
  | UMDoKeyword                 