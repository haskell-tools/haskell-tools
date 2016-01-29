{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           #-}
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

type WithST s = NodeInfo s SourceTemplate

ifToCase :: forall s . (Data s, TemplateAnnot (WithST s)) => RealSrcSpan -> Ann Module (WithST s) -> IO () -- Ann Module SourceTemplate
ifToCase sp mod
  = putStrLn $ prettyPrint $ transformBi (doIfToCase sp :: Ann Expr (WithST s) -> Ann Expr (WithST s)) mod
  where

doIfToCase :: (Data s, TemplateAnnot (WithST s)) => RealSrcSpan -> Ann Expr (WithST s) -> Ann Expr (WithST s)
doIfToCase sp (Ann l (If pred thenE elseE)) 
  | similarSpans sp (l ^. sourceInfo.sourceTemplateRange)
  = case' pred 
     [ alt (varPat "Data.Bool.True") (unguardedCaseRhs thenE) Nothing
     , alt (varPat "Data.Bool.False") (unguardedCaseRhs elseE) Nothing 
     ]
doIfToCase _ e = e
    
-- * General utilities

similarSpans :: RealSrcSpan -> SrcSpan -> Bool
similarSpans sp1 (RealSrcSpan sp2) = 
  srcLocLine (realSrcSpanStart sp1) == srcLocLine (realSrcSpanStart sp2)
    && srcLocCol (realSrcSpanStart sp1) == srcLocCol (realSrcSpanStart sp2)
    && srcLocLine (realSrcSpanEnd sp1) == srcLocLine (realSrcSpanEnd sp2)
    && srcLocCol (realSrcSpanEnd sp1) == srcLocCol (realSrcSpanEnd sp2)
similarSpans _ _ = False

class TemplateAnnot annot where
  fromTemplate :: SourceTemplate -> annot
  
instance TemplateAnnot (NodeInfo (Maybe a) SourceTemplate) where
  fromTemplate = NodeInfo Nothing
  
instance TemplateAnnot (NodeInfo () SourceTemplate) where
  fromTemplate = NodeInfo ()
    
-- * AST creation
    
case' :: TemplateAnnot (WithST s) => Ann Expr (WithST s) -> [Ann Alt (WithST s)] -> Ann Expr (WithST s)
case' e alts = Ann (fromTemplate $ "case " <> (×) <> " of { " <> (×) <> "; " <> (×) <> " }")
                 $ Case e (AnnList alts)
  
alt :: TemplateAnnot (WithST s) => Ann Pattern (WithST s) -> Ann CaseRhs (WithST s) -> Maybe (Ann LocalBinds (WithST s)) -> Ann Alt (WithST s)
alt pat rhs Nothing = Ann ( fromTemplate $ (×) <> (×) ) $ Alt pat rhs (AnnMaybe Nothing)  
alt pat rhs locs@(Just _) = Ann ( fromTemplate $ (×) <> (×) <> " " <> (×) ) $ Alt pat rhs (AnnMaybe locs)  

varPat :: TemplateAnnot (WithST s) => String -> Ann Pattern (WithST s)
varPat str = Ann (fromTemplate (×)) $ VarPat (Ann (fromTemplate (×)) (Name (AnnList []) (Ann (fromTemplate $ fromString str) $ SimpleName str))) 

unguardedCaseRhs :: TemplateAnnot (WithST s) => Ann Expr (WithST s) -> Ann CaseRhs (WithST s)
unguardedCaseRhs e = Ann (fromTemplate $ " -> " <> (×) ) $ UnguardedCaseRhs e



