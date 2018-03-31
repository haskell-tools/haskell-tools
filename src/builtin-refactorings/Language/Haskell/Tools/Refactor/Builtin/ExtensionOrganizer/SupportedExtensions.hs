module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.SupportedExtensions where

import Language.Haskell.TH.LanguageExtensions (Extension(..))

isSupported :: Extension -> Bool
isSupported = flip elem fullyHandledExtensions

fullyHandledExtensions :: [Extension]
fullyHandledExtensions = syntacticExtensions
                      ++ derivingExtensions
                      ++ typeClassExtensions
                      ++ typeSystemExtensions
                      ++ thExtensions
                      ++ [Cpp]

syntacticExtensions :: [Extension]
syntacticExtensions = [ RecordWildCards, BangPatterns
                      , PatternSynonyms, TupleSections, LambdaCase
                      , ViewPatterns, MagicHash, UnboxedTuples
                      , FunctionalDependencies, DefaultSignatures
                      , RecursiveDo, Arrows, ParallelListComp
                      , KindSignatures, ExplicitNamespaces
                      , GADTSyntax, ExplicitForAll, MultiWayIf
                      , TypeOperators ]

derivingExtensions :: [Extension]
derivingExtensions = [ DeriveDataTypeable, DeriveGeneric, DeriveFunctor
                     , DeriveFoldable, DeriveTraversable, DeriveLift
                     , DeriveAnyClass, GeneralizedNewtypeDeriving
                     , StandaloneDeriving, DerivingStrategies ]

typeClassExtensions :: [Extension]
typeClassExtensions = [ MultiParamTypeClasses, ConstrainedClassMethods
                      , FlexibleInstances, TypeSynonymInstances
                      , FlexibleContexts
                      ]

typeSystemExtensions :: [Extension]
typeSystemExtensions = [ TypeFamilies, GADTs, ExistentialQuantification
                       , ConstraintKinds, UndecidableInstances ]

thExtensions :: [Extension]
thExtensions = [TemplateHaskell, TemplateHaskellQuotes, QuasiQuotes]
