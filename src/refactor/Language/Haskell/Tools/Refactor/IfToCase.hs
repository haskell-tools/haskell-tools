{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Tools.Refactor.IfToCase where

import SrcLoc
import Control.Lens
import Data.String
import Data.Data
import Data.List
import Data.Generics.Uniplate.Data
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.PrettyPrint

type ST = SourceTemplate

ifToCase :: RealSrcSpan -> Ann Module ST -> IO () -- Ann Module SourceTemplate
ifToCase sp mod
  = putStrLn $ prettyPrint $ transformBi (doIfToCase sp) mod

similarSpans :: RealSrcSpan -> SrcSpan -> Bool
similarSpans sp1 (RealSrcSpan sp2) = 
  srcLocLine (realSrcSpanStart sp1) == srcLocLine (realSrcSpanStart sp2)
    && srcLocCol (realSrcSpanStart sp1) == srcLocCol (realSrcSpanStart sp2)
    && srcLocLine (realSrcSpanEnd sp1) == srcLocLine (realSrcSpanEnd sp2)
    && srcLocCol (realSrcSpanEnd sp1) == srcLocCol (realSrcSpanEnd sp2)
similarSpans _ _ = False

doIfToCase :: RealSrcSpan -> Ann Expr ST -> Ann Expr ST
doIfToCase sp (Ann l (If pred thenE elseE)) 
  | similarSpans sp (l ^. sourceTemplateRange)
  = case' pred 
     [ alt (varPat "Data.Bool.True") (unguardedCaseRhs thenE) Nothing
     , alt (varPat "Data.Bool.False") (unguardedCaseRhs elseE) Nothing 
     ]
doIfToCase _ e = e
    
case' :: Ann Expr ST -> [Ann Alt ST] -> Ann Expr ST
case' e alts = Ann ("case " <> (×) <> " of { " <> (×) <> "; " <> (×) <> " }")
                 $ Case e (AnnList alts)
  
alt :: Ann Pattern ST -> Ann CaseRhs ST -> Maybe (Ann LocalBinds ST) -> Ann Alt ST
alt pat rhs Nothing = Ann ( (×) <> (×) ) $ Alt pat rhs (AnnMaybe Nothing)  
alt pat rhs locs@(Just _) = Ann ( (×) <> (×) <> " " <> (×) ) $ Alt pat rhs (AnnMaybe locs)  

varPat :: String -> Ann Pattern ST
varPat str = Ann (×) $ VarPat (Ann (×) (Name (AnnList []) (Ann (fromString str) $ SimpleName str))) 

unguardedCaseRhs :: Ann Expr ST -> Ann CaseRhs ST
unguardedCaseRhs e = Ann ( " -> " <> (×) ) $ UnguardedCaseRhs e



