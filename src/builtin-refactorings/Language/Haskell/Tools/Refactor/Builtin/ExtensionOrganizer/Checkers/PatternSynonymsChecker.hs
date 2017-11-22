{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.PatternSynonymsChecker where

import Language.Haskell.Tools.Refactor (PatternSignature, PatternSynonym)
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

-- NOTE: Here we implicitly constrained the type with ExtDomain.
--       but we don't really need any.

chkPatternSynonymsSyn :: CheckNode PatternSynonym
chkPatternSynonymsSyn = conditional chkPatternSynonymsSyn' PatternSynonyms

chkPatternSynonymsSyn' :: CheckNode PatternSynonym
chkPatternSynonymsSyn' = addOccurence PatternSynonyms

chkPatternSynonymsTypeSig :: CheckNode PatternSignature
chkPatternSynonymsTypeSig = conditional (addOccurence PatternSynonyms) PatternSynonyms
