-- | Defines the API for refactorings
module Language.Haskell.Tools.Refactor
    ( module Language.Haskell.Tools.AST.SemaInfoClasses
    , module Language.Haskell.Tools.Rewrite
    , module Language.Haskell.Tools.AST.References
    , module Language.Haskell.Tools.AST.Helpers
    , module Language.Haskell.Tools.Refactor.Utils.Monadic
  , module Language.Haskell.Tools.Refactor.Utils.Helpers
    , module Language.Haskell.Tools.Rewrite.ElementTypes
    , module Language.Haskell.Tools.Refactor.Prepare
    , module Language.Haskell.Tools.Refactor.Utils.Lists
    , module Language.Haskell.Tools.Refactor.Utils.BindingElem
    , module Language.Haskell.Tools.Refactor.Utils.Indentation
    , module Language.Haskell.Tools.Refactor.Refactoring
    , module Language.Haskell.Tools.Refactor.Utils.Name
    , module Language.Haskell.Tools.Refactor.Representation
    , module Language.Haskell.Tools.Refactor.Monad
  , Ann, HasSourceInfo(..), HasRange(..), annListElems, annListAnnot, annList, annJust, annMaybe, isAnnNothing, Domain, Dom, IdDom
    , shortShowSpan, shortShowSpanWithFile, SrcTemplateStage, SourceInfoTraversal(..)
    -- elements of source templates
    , sourceTemplateNodeRange, sourceTemplateNodeElems
    , sourceTemplateListRange, srcTmpListBefore, srcTmpListAfter, srcTmpDefaultSeparator, srcTmpIndented, srcTmpSeparators
    , sourceTemplateOptRange, srcTmpOptBefore, srcTmpOptAfter
    , SourceTemplateTextElem(..), sourceTemplateText
    , UnsupportedExtension(..), SpliceInsertionProblem(..), ConvertionProblem(..)
    , TransformationProblem(..), BreakUpProblem(..), PrettyPrintProblem(..)
    ) where

-- Important: Haddock doesn't support the rename all exported modules and export them at once hack

import Language.Haskell.Tools.AST.Helpers
import Language.Haskell.Tools.AST.References
import Language.Haskell.Tools.AST.SemaInfoClasses
import Language.Haskell.Tools.BackendGHC (SpliceInsertionProblem(..), ConvertionProblem(..))
import Language.Haskell.Tools.PrettyPrint (PrettyPrintProblem(..))
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Refactor.Monad
import Language.Haskell.Tools.Refactor.Prepare hiding (ModuleName)
import Language.Haskell.Tools.Refactor.Refactoring
import Language.Haskell.Tools.Refactor.Representation
import Language.Haskell.Tools.Refactor.Utils.BindingElem
import Language.Haskell.Tools.Refactor.Utils.Helpers
import Language.Haskell.Tools.Refactor.Utils.Indentation
import Language.Haskell.Tools.Refactor.Utils.Lists
import Language.Haskell.Tools.Refactor.Utils.Monadic
import Language.Haskell.Tools.Refactor.Utils.Name
import Language.Haskell.Tools.Rewrite
import Language.Haskell.Tools.Rewrite.ElementTypes

import Language.Haskell.Tools.AST.Ann
