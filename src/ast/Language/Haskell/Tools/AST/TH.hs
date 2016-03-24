-- | Representation of Template Haskell AST elements
module Language.Haskell.Tools.AST.TH where
              
import Language.Haskell.Tools.AST.Decls
import Language.Haskell.Tools.AST.Binds
import Language.Haskell.Tools.AST.Exprs
import Language.Haskell.Tools.AST.Patterns
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Ann
              
-- | A template haskell splice          
data Splice a
  = IdSplice    { _spliceId :: Ann Name a 
                } -- ^ A simple name splice: @$generateX@
  | ParenSplice { _spliceExpr :: Ann Expr a
                } -- ^ A splice with parentheses: @$(generate input)@
  
-- | Template haskell quasi-quotation: @[quoter|str]@  
data QuasiQuote a 
  = QuasiQuote { _qqExprName :: Ann Name a
               , _qqExprBody :: Ann QQString a
               } 
        
-- | Template Haskell Quasi-quotation content
data QQString a
  = QQString { _qqString :: String 
             } 

          
-- | Template Haskell bracket expressions
data Bracket a
  = ExprBracket    { _bracketExpr :: Ann Expr a 
                   } -- ^ Expression bracket (@ [| x + y |] @)
  | PatternBracket { _bracketPattern :: Ann Pattern a 
                   } -- ^ Pattern bracket (@ [| Point x y |] @)
  | TypeBracket    { _bracketType :: Ann Type a 
                   } -- ^ Pattern bracket (@ [| (Int,Int) |] @)
  | DeclBracket    { _bracketDecl :: Ann Decl a 
                   } -- ^ Declaration bracket (@ [| _f :: Int -> Int; f x = x*x |] @)
            