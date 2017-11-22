{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.RecordWildCardsChecker where

import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Control.Reference ((!~), (&))
import Language.Haskell.Tools.Refactor

-- NOTE: Here we implicitly constrained the type with ExtDomain.
--       but we don't really need any contraint.

chkRecordWildCardsPatField :: CheckNode PatternField
chkRecordWildCardsPatField p@(FieldWildcardPattern x) = addOccurence RecordWildCards p
chkRecordWildCardsPatField p = return p

chkRecordWildCardsFieldUpdate :: CheckNode FieldUpdate
chkRecordWildCardsFieldUpdate e@(FieldWildcard x) = addOccurence RecordWildCards e
chkRecordWildCardsFieldUpdate e = return e
