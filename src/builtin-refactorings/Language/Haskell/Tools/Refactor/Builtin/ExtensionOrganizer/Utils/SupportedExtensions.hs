module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.SupportedExtensions where

import Language.Haskell.TH.LanguageExtensions (Extension(..))

isSupported :: Extension -> Bool
isSupported = flip elem fullyHandledExtensions

fullyHandledExtensions :: [Extension]
fullyHandledExtensions = syntacticExtensions
                      ++ derivingExtensions
                      ++ typeClassExtensions
                      ++ [ ConstraintKinds ]
                      ++ [ TypeFamilies ]
                      -- ++ [FlexibleInstances]

syntacticExtensions :: [Extension]
syntacticExtensions = [ RecordWildCards, TemplateHaskell, BangPatterns
                      , PatternSynonyms, TupleSections, LambdaCase, QuasiQuotes
                      , ViewPatterns, MagicHash, UnboxedTuples
                      , FunctionalDependencies, DefaultSignatures
                      , RecursiveDo, Arrows, ParallelListComp
                      , KindSignatures, ExplicitNamespaces ]

derivingExtensions :: [Extension]
derivingExtensions = [ DeriveDataTypeable, DeriveGeneric, DeriveFunctor
                     , DeriveFoldable, DeriveTraversable, DeriveLift
                     , DeriveAnyClass, GeneralizedNewtypeDeriving
                     , StandaloneDeriving, DerivingStrategies ]

typeClassExtensions :: [Extension]
typeClassExtensions = [ MultiParamTypeClasses ]
