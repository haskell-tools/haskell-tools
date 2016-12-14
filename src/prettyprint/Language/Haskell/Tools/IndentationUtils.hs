-- | Utilities to modify the indentation of AST fragments
module Language.Haskell.Tools.IndentationUtils where

import Control.Reference
import Control.Monad.Identity

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Transform.SourceTemplate

setMinimalIndent :: SourceInfoTraversal elem 
                 => Int -> elem dom SrcTemplateStage -> elem dom SrcTemplateStage
setMinimalIndent ind = runIdentity . 
  sourceInfoTraverse (SourceInfoTrf (Identity . (sourceTemplateMinimalIndent .= ind)) 
                                    (Identity . (srcTmpListMinimalIndent .= ind)) 
                                    (Identity . (srcTmpOptMinimalIndent .= ind)))