-- | Representation of Template Haskell AST elements
module Language.Haskell.Tools.AST.Representation.TH where
              
import Language.Haskell.Tools.AST.Representation.Decls
import Language.Haskell.Tools.AST.Representation.Binds
import Language.Haskell.Tools.AST.Representation.Exprs
import Language.Haskell.Tools.AST.Representation.Patterns
import Language.Haskell.Tools.AST.Representation.Types
import Language.Haskell.Tools.AST.Representation.Names
import Language.Haskell.Tools.AST.Ann
              
-- | A template haskell splice          
data USplice dom stage
  = UIdSplice    { _spliceId :: Ann UName dom stage
                 } -- ^ A simple name splice: @$generateX@
  | UParenSplice { _spliceExpr :: Ann UExpr dom stage
                 } -- ^ A splice with parentheses: @$(generate input)@
  
-- | Template haskell quasi-quotation: @[quoter|str]@  
data UQuasiQuote dom stage
  = UQuasiQuote { _qqExprName :: Ann UName dom stage
                , _qqExprBody :: Ann QQString dom stage
                } 
        
-- | Template Haskell Quasi-quotation content
data QQString dom stage
  = QQString { _qqString :: String 
             } 

          
-- | Template Haskell bracket expressions
data UBracket dom stage
  = UExprBracket    { _bracketExpr :: Ann UExpr dom stage
                    } -- ^ Expression bracket (@ [| x + y |] @)
  | UPatternBracket { _bracketPattern :: Ann UPattern dom stage
                    } -- ^ Pattern bracket (@ [p| Point x y |] @)
  | UTypeBracket    { _bracketType :: Ann UType dom stage
                    } -- ^ Type bracket (@ [t| (Int,Int) |] @)
  | UDeclsBracket   { _bracketDecl :: AnnListG UDecl dom stage
                    } -- ^ Declaration bracket (@ [d| f :: Int -> Int; f x = x*x |] @)
            