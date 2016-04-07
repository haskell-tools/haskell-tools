-- | Representation of Haskell statements (both do-notation and comprehensions)
module Language.Haskell.Tools.AST.Stmts where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Patterns
import {-# SOURCE #-} Language.Haskell.Tools.AST.Exprs (Expr, Cmd)
import {-# SOURCE #-} Language.Haskell.Tools.AST.Binds (LocalBind)

-- | Normal monadic statements
data Stmt' expr a
  = BindStmt { _stmtPattern :: Ann Pattern a
             , _stmtBounded :: Ann expr a
             } -- ^ Binding statement (@ x <- action @)
  | ExprStmt { _stmtExpr :: Ann expr a 
             } -- ^ Non-binding statement (@ action @)
  | LetStmt  { _stmtBinds :: AnnList LocalBind a 
             } -- ^ Let statement (@ let x = 3; y = 4 @)
type Stmt = Stmt' Expr

-- | Statements in a proc
data CmdStmt a
  = RecStmt    { _cmdStmtBinds :: AnnList CmdStmt a 
               } -- ^ A recursive binding group for arrows (@ rec b <- f a c; c <- f b a @)
  | NonRecStmt { _cmdStmtNonrec :: Ann (Stmt' Cmd) a
               }
        
-- | Body of a list comprehension: (@ | x <- [1..10] @)
data ListCompBody a
  = ListCompBody { _compStmts :: AnnList CompStmt a 
                 } 
         
-- | List comprehension statement
data CompStmt a
  = CompStmt   { _compStmt :: Ann Stmt a 
               } -- ^ Normal monadic statement of a list comprehension
  | ThenStmt   { _thenExpr :: Ann Expr a 
               , _byExpr :: AnnMaybe Expr a
               } -- ^ Then statements by @TransformListComp@ (@ then sortWith by (x + y) @)
  | GroupStmt  { _byExpr :: AnnMaybe Expr a
               , _usingExpr :: AnnMaybe Expr a
               } -- ^ Grouping statements by @TransformListComp@ (@ then group by (x + y) using groupWith @) 
                 -- Note: either byExpr or usingExpr must have a value