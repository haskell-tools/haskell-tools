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
    , module Language.Haskell.Tools.Refactor.Querying
    , module Language.Haskell.Tools.Refactor.Refactoring
    , module Language.Haskell.Tools.Refactor.Utils.Name
    , module Language.Haskell.Tools.Refactor.Representation
    , module Language.Haskell.Tools.Refactor.Monad
    , module Language.Haskell.Tools.Refactor.Utils.Type
    , module Language.Haskell.Tools.Refactor.Utils.TypeLookup
    , module Language.Haskell.Tools.Refactor.Utils.NameLookup
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

import Language.Haskell.Tools.AST.Helpers                as X
import Language.Haskell.Tools.AST.References             as X
import Language.Haskell.Tools.AST.SemaInfoClasses        as X
import Language.Haskell.Tools.PrettyPrint.Prepare        as X
import Language.Haskell.Tools.Refactor.Monad             as X
import Language.Haskell.Tools.Refactor.Prepare           as X hiding (ModuleName)
import Language.Haskell.Tools.Refactor.Refactoring       as X
import Language.Haskell.Tools.Refactor.Representation    as X
import Language.Haskell.Tools.Refactor.Querying          as X
import Language.Haskell.Tools.Refactor.Utils.BindingElem as X
import Language.Haskell.Tools.Refactor.Utils.Debug       as X
import Language.Haskell.Tools.Refactor.Utils.Helpers     as X
import Language.Haskell.Tools.Refactor.Utils.Indentation as X
import Language.Haskell.Tools.Refactor.Utils.Lists       as X
import Language.Haskell.Tools.Refactor.Utils.Maybe       as X
import Language.Haskell.Tools.Refactor.Utils.Monadic     as X
import Language.Haskell.Tools.Refactor.Utils.Name        as X
import Language.Haskell.Tools.Refactor.Utils.NameLookup  as X
import Language.Haskell.Tools.Refactor.Utils.Type        as X
import Language.Haskell.Tools.Refactor.Utils.TypeLookup  as X
import Language.Haskell.Tools.Rewrite                    as X
import Language.Haskell.Tools.Rewrite.ElementTypes       as X

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.BackendGHC (SpliceInsertionProblem(..), ConvertionProblem(..))
import Language.Haskell.Tools.PrettyPrint (PrettyPrintProblem(..))
