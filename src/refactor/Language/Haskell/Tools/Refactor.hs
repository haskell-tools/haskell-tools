-- | Defines the API for refactorings
module Language.Haskell.Tools.Refactor 
    ( module Language.Haskell.Tools.AST.SemaInfoClasses
    , module Language.Haskell.Tools.AST.Rewrite
    , module Language.Haskell.Tools.AST.References
    , module Language.Haskell.Tools.AST.Helpers
    , module Language.Haskell.Tools.Refactor.RefactorBase
  , module Language.Haskell.Tools.Refactor.Helpers
    , module Language.Haskell.Tools.AST.ElementTypes
    , module Language.Haskell.Tools.Refactor.Prepare
    , module Language.Haskell.Tools.Refactor.ListOperations
    , module Language.Haskell.Tools.Refactor.BindingElem
    , module Language.Haskell.Tools.IndentationUtils
  , Ann, HasSourceInfo(..), HasRange(..), annListElems, annListAnnot, annList, annJust, annMaybe, isAnnNothing, Domain
    , shortShowSpan, SrcTemplateStage, SourceInfoTraversal(..)
    -- elements of source templates
    , sourceTemplateNodeRange, sourceTemplateNodeElems
    , sourceTemplateListRange, srcTmpListBefore, srcTmpListAfter, srcTmpDefaultSeparator, srcTmpIndented, srcTmpSeparators
    , sourceTemplateOptRange, srcTmpOptBefore, srcTmpOptAfter
    ) where

-- Important: Haddock doesn't support the rename all exported modules and export them at once hack

import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Helpers
import Language.Haskell.Tools.AST.References
import Language.Haskell.Tools.AST.Rewrite
import Language.Haskell.Tools.AST.SemaInfoClasses
import Language.Haskell.Tools.IndentationUtils
import Language.Haskell.Tools.Refactor.BindingElem
import Language.Haskell.Tools.Refactor.Helpers
import Language.Haskell.Tools.Refactor.ListOperations
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Transform

import Language.Haskell.Tools.AST.Ann
