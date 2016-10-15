-- | Representation of Haskell statements (both do-notation and comprehensions)
module Language.Haskell.Tools.AST.Stmts where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Patterns
import {-# SOURCE #-} Language.Haskell.Tools.AST.Exprs (Expr, Cmd)
import {-# SOURCE #-} Language.Haskell.Tools.AST.Binds (LocalBind)

-- | Normal monadic statements
data Stmt' expr dom stage
  = UBindStmt { _stmtPattern :: Ann Pattern dom stage
              , _stmtExpr :: Ann expr dom stage
              } -- ^ Binding statement (@ x <- action @)
  | UExprStmt { _stmtExpr :: Ann expr dom stage
              } -- ^ Non-binding statement (@ action @)
  | ULetStmt  { _stmtBinds :: AnnList LocalBind dom stage
              } -- ^ Let statement (@ let x = 3; y = 4 @)
  | URecStmt  { _cmdStmtBinds :: AnnList (Stmt' expr) dom stage
              } -- ^ A recursive binding statement with (@ rec b <- f a c; c <- f b a @)
type Stmt = Stmt' Expr

-- | Body of a list comprehension: (@ | x <- [1..10] @)
data ListCompBody dom stage
  = UListCompBody { _compStmts :: AnnList CompStmt dom stage
                  } 
         
-- | List comprehension statement
data CompStmt dom stage
  = UCompStmt  { _compStmt :: Ann Stmt dom stage
               } -- ^ Normal monadic statement of a list comprehension
  | UThenStmt  { _thenExpr :: Ann Expr dom stage
               , _byExpr :: AnnMaybe Expr dom stage
               } -- ^ Then statements by @TransformListComp@ (@ then sortWith by (x + y) @)
  | UGroupStmt { _byExpr :: AnnMaybe Expr dom stage
               , _usingExpr :: AnnMaybe Expr dom stage
               } -- ^ Grouping statements by @TransformListComp@ (@ then group by (x + y) using groupWith @) 
                 -- Note: either byExpr or usingExpr must have a value