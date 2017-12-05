{-# LANGUAGE FlexibleContexts, RankNTypes, TypeFamilies #-}

module ExtensionOrganizerBenchmark.UniPlateTraversal where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Instances.Checkable()

import Control.Reference ((!~), biplateRef)


traverseModule :: CheckNode UnnamedModule
traverseModule = biplateRef !~ (check :: CheckNode Decl)
             >=> biplateRef !~ (check :: CheckNode Pattern)
             >=> biplateRef !~ (check :: CheckNode Expr)
             >=> biplateRef !~ (check :: CheckNode Type)
             >=> biplateRef !~ (check :: CheckNode PatternField)
             >=> biplateRef !~ (check :: CheckNode FieldUpdate)
             >=> biplateRef !~ (check :: CheckNode PatternSynonym)
             >=> biplateRef !~ (check :: CheckNode PatternSignature)
             >=> biplateRef !~ (check :: CheckNode Literal)
             >=> biplateRef !~ (check :: CheckNode NamePart)
             >=> biplateRef !~ (check :: CheckNode Kind)
             >=> biplateRef !~ (check :: CheckNode Splice)
             >=> biplateRef !~ (check :: CheckNode QuasiQuote)
             >=> biplateRef !~ (check :: CheckNode Bracket)
