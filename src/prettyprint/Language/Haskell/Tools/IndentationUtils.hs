-- | Utilities to modify the indentation of AST fragments
module Language.Haskell.Tools.IndentationUtils where

import Control.Monad.Identity (Identity(..))
import Control.Reference ((.=))

import Language.Haskell.Tools.AST (SrcTemplateStage(..), SourceInfoTrf(..), SourceInfoTraversal(..))
import Language.Haskell.Tools.Transform.SourceTemplate (sourceTemplateMinimalIndent, srcTmpListMinimalIndent, srcTmpOptMinimalIndent)

-- | Set the minimal indentation recursively for a part of the AST
setMinimalIndent :: SourceInfoTraversal elem 
                 => Int -> elem dom SrcTemplateStage -> elem dom SrcTemplateStage
setMinimalIndent ind = runIdentity . 
  sourceInfoTraverse (SourceInfoTrf (Identity . (sourceTemplateMinimalIndent .= ind)) 
                                    (Identity . (srcTmpListMinimalIndent .= ind)) 
                                    (Identity . (srcTmpOptMinimalIndent .= ind)))