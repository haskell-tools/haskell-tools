 -- all extensions should be matched

module Language.Haskell.Tools.Refactor.Utils.Extensions
  ( module Language.Haskell.Tools.Refactor.Utils.Extensions
  , GHC.Extension(..)
  ) where

import Data.Maybe (fromMaybe)

import Control.Reference ((^.), _1, _2, _3)
import Language.Haskell.Extension (KnownExtension(..))
import qualified Language.Haskell.TH.LanguageExtensions as GHC (Extension(..))


-- | Expands an extension into all the extensions it implies (keeps original as well)
expandExtension :: GHC.Extension -> [GHC.Extension]
expandExtension ext = ext : implied
  where fst' = (^. _1) :: (a,b,c) -> a
        snd' = (^. _2) :: (a,b,c) -> b
        trd' = (^. _3) :: (a,b,c) -> c

        implied = map trd' . filter snd' . filter ((== ext) . fst') $ impliedXFlags

-- | Replaces deprecated extensions with their new counterpart
replaceDeprecated :: GHC.Extension -> GHC.Extension
replaceDeprecated GHC.NullaryTypeClasses = GHC.MultiParamTypeClasses
replaceDeprecated x = x

turnOn  = True
turnOff = False

impliedXFlags :: [(GHC.Extension, Bool, GHC.Extension)]
impliedXFlags
  = [ (GHC.RankNTypes,                turnOn,  GHC.ExplicitForAll)
    , (GHC.ScopedTypeVariables,       turnOn,  GHC.ExplicitForAll)
    , (GHC.LiberalTypeSynonyms,       turnOn,  GHC.ExplicitForAll)
    , (GHC.ExistentialQuantification, turnOn,  GHC.ExplicitForAll)
    , (GHC.FlexibleInstances,         turnOn,  GHC.TypeSynonymInstances)
    , (GHC.FunctionalDependencies,    turnOn,  GHC.MultiParamTypeClasses)
    , (GHC.MultiParamTypeClasses,     turnOn,  GHC.ConstrainedClassMethods)
    , (GHC.TypeFamilyDependencies,    turnOn,  GHC.TypeFamilies)
    , (GHC.RebindableSyntax,          turnOff, GHC.ImplicitPrelude)
    , (GHC.GADTs,                     turnOn,  GHC.GADTSyntax)
    , (GHC.GADTs,                     turnOn,  GHC.MonoLocalBinds)
    , (GHC.TypeFamilies,              turnOn,  GHC.MonoLocalBinds)
    , (GHC.TypeFamilies,              turnOn,  GHC.KindSignatures)
    , (GHC.PolyKinds,                 turnOn,  GHC.KindSignatures)
    , (GHC.TypeInType,                turnOn,  GHC.DataKinds)
    , (GHC.TypeInType,                turnOn,  GHC.PolyKinds)
    , (GHC.TypeInType,                turnOn,  GHC.KindSignatures)
    , (GHC.AutoDeriveTypeable,        turnOn,  GHC.DeriveDataTypeable)
    , (GHC.TypeFamilies,              turnOn,  GHC.ExplicitNamespaces)
    , (GHC.TypeOperators,             turnOn,  GHC.ExplicitNamespaces)
    , (GHC.ImpredicativeTypes,        turnOn,  GHC.RankNTypes)
    , (GHC.RecordWildCards,           turnOn,  GHC.DisambiguateRecordFields)
    , (GHC.ParallelArrays,            turnOn,  GHC.ParallelListComp)
    , (GHC.JavaScriptFFI,             turnOn,  GHC.InterruptibleFFI)
    , (GHC.DeriveTraversable,         turnOn,  GHC.DeriveFunctor)
    , (GHC.DeriveTraversable,         turnOn,  GHC.DeriveFoldable)
    , (GHC.DuplicateRecordFields,     turnOn,  GHC.DisambiguateRecordFields)
    , (GHC.TemplateHaskell,           turnOn,  GHC.TemplateHaskellQuotes)
    , (GHC.Strict,                    turnOn,  GHC.StrictData)
    ]

-- | These extensions' GHC representation name differs from their actual name
irregularExtensions :: [(String,String)]
irregularExtensions = [ ("CPP", "Cpp")
                      , ("Rank2Types", "RankNTypes")
                      , ("NamedFieldPuns", "RecordPuns")
                      , ("GeneralisedNewtypeDeriving", "GeneralizedNewtypeDeriving")
                      ]

-- | Canonicalize extensions.
-- This is a helper function for parsing extensions
-- This way we can say @read . canonExt@ to parse any extension string
canonExt :: String -> String
canonExt x = fromMaybe x (lookup x irregularExtensions)

-- | Serializes the extension's GHC name into its LANGUAGE pragma name.
-- Should be always used in composition with show (@seriealizeExt . show@)
-- when refactoring extensions.
-- This function also replaces depracted extensions with their new versions.
serializeExt :: String -> String
serializeExt "Cpp" = "CPP"
serializeExt "Rank2Types" = "RankNTypes"
serializeExt "RecordPuns" = "NamedFieldPuns"
serializeExt x = x


-- * Mapping of Cabal haskell extensions to their GHC counterpart

-- | Map the cabal extensions to the ones that GHC recognizes
translateExtension AllowAmbiguousTypes = Just GHC.AllowAmbiguousTypes
translateExtension ApplicativeDo = Just GHC.ApplicativeDo
translateExtension Arrows = Just GHC.Arrows
translateExtension AutoDeriveTypeable = Just GHC.AutoDeriveTypeable
translateExtension BangPatterns = Just GHC.BangPatterns
translateExtension BinaryLiterals = Just GHC.BinaryLiterals
translateExtension CApiFFI = Just GHC.CApiFFI
translateExtension ConstrainedClassMethods = Just GHC.ConstrainedClassMethods
translateExtension ConstraintKinds = Just GHC.ConstraintKinds
translateExtension CPP = Just GHC.Cpp
translateExtension DataKinds = Just GHC.DataKinds
translateExtension DatatypeContexts = Just GHC.DatatypeContexts
translateExtension DefaultSignatures = Just GHC.DefaultSignatures
translateExtension DeriveAnyClass = Just GHC.DeriveAnyClass
translateExtension DeriveDataTypeable = Just GHC.DeriveDataTypeable
translateExtension DeriveFoldable = Just GHC.DeriveFoldable
translateExtension DeriveFunctor = Just GHC.DeriveFunctor
translateExtension DeriveGeneric = Just GHC.DeriveGeneric
translateExtension DeriveLift = Just GHC.DeriveLift
translateExtension DeriveTraversable = Just GHC.DeriveTraversable
translateExtension DisambiguateRecordFields = Just GHC.DisambiguateRecordFields
translateExtension DoAndIfThenElse = Just GHC.DoAndIfThenElse
translateExtension DoRec = Just GHC.RecursiveDo
translateExtension DuplicateRecordFields = Just GHC.DuplicateRecordFields
translateExtension EmptyCase = Just GHC.EmptyCase
translateExtension EmptyDataDecls = Just GHC.EmptyDataDecls
translateExtension ExistentialQuantification = Just GHC.ExistentialQuantification
translateExtension ExplicitForAll = Just GHC.ExplicitForAll
translateExtension ExplicitNamespaces = Just GHC.ExplicitNamespaces
translateExtension ExtendedDefaultRules = Just GHC.ExtendedDefaultRules
translateExtension FlexibleContexts = Just GHC.FlexibleContexts
translateExtension FlexibleInstances = Just GHC.FlexibleInstances
translateExtension ForeignFunctionInterface = Just GHC.ForeignFunctionInterface
translateExtension FunctionalDependencies = Just GHC.FunctionalDependencies
translateExtension GADTs = Just GHC.GADTs
translateExtension GADTSyntax = Just GHC.GADTSyntax
translateExtension GeneralizedNewtypeDeriving = Just GHC.GeneralizedNewtypeDeriving
translateExtension GHCForeignImportPrim = Just GHC.GHCForeignImportPrim
translateExtension ImplicitParams = Just GHC.ImplicitParams
translateExtension ImplicitPrelude = Just GHC.ImplicitPrelude
translateExtension ImpredicativeTypes = Just GHC.ImpredicativeTypes
translateExtension IncoherentInstances = Just GHC.IncoherentInstances
translateExtension InstanceSigs = Just GHC.InstanceSigs
translateExtension InterruptibleFFI = Just GHC.InterruptibleFFI
translateExtension JavaScriptFFI = Just GHC.JavaScriptFFI
translateExtension KindSignatures = Just GHC.KindSignatures
translateExtension LambdaCase = Just GHC.LambdaCase
translateExtension LiberalTypeSynonyms = Just GHC.LiberalTypeSynonyms
translateExtension MagicHash = Just GHC.MagicHash
translateExtension MonadComprehensions = Just GHC.MonadComprehensions
translateExtension MonadFailDesugaring = Just GHC.MonadFailDesugaring
translateExtension MonoLocalBinds = Just GHC.MonoLocalBinds
translateExtension MonomorphismRestriction = Just GHC.MonomorphismRestriction
translateExtension MonoPatBinds = Just GHC.MonoPatBinds
translateExtension MultiParamTypeClasses = Just GHC.MultiParamTypeClasses
translateExtension MultiWayIf = Just GHC.MultiWayIf
translateExtension NamedFieldPuns = Just GHC.RecordPuns
translateExtension NamedWildCards = Just GHC.NamedWildCards
translateExtension NegativeLiterals = Just GHC.NegativeLiterals
translateExtension NondecreasingIndentation = Just GHC.NondecreasingIndentation
translateExtension NPlusKPatterns = Just GHC.NPlusKPatterns
translateExtension NullaryTypeClasses = Just GHC.NullaryTypeClasses
translateExtension NumDecimals = Just GHC.NumDecimals
translateExtension OverlappingInstances = Just GHC.OverlappingInstances
translateExtension OverloadedLabels = Just GHC.OverloadedLabels
translateExtension OverloadedLists = Just GHC.OverloadedLists
translateExtension OverloadedStrings = Just GHC.OverloadedStrings
translateExtension PackageImports = Just GHC.PackageImports
translateExtension ParallelArrays = Just GHC.ParallelArrays
translateExtension ParallelListComp = Just GHC.ParallelListComp
translateExtension PartialTypeSignatures = Just GHC.PartialTypeSignatures
translateExtension PatternGuards = Just GHC.PatternGuards
translateExtension PatternSignatures = Just GHC.PatternSynonyms
translateExtension PatternSynonyms = Just GHC.PatternSynonyms
translateExtension PolyKinds = Just GHC.PolyKinds
translateExtension PostfixOperators = Just GHC.PostfixOperators
translateExtension QuasiQuotes = Just GHC.QuasiQuotes
translateExtension RankNTypes = Just GHC.RankNTypes
translateExtension RebindableSyntax = Just GHC.RebindableSyntax
translateExtension RecordPuns = Just GHC.RecordPuns
translateExtension RecordWildCards = Just GHC.RecordWildCards
translateExtension RecursiveDo = Just GHC.RecursiveDo
translateExtension RelaxedPolyRec = Just GHC.RelaxedPolyRec
translateExtension RestrictedTypeSynonyms = Nothing -- flip xopt_unset GHC.LiberalTypeSynonyms
translateExtension RoleAnnotations = Just GHC.RoleAnnotations
translateExtension ScopedTypeVariables = Just GHC.ScopedTypeVariables
translateExtension StandaloneDeriving = Just GHC.StandaloneDeriving
translateExtension StaticPointers = Just GHC.StaticPointers
translateExtension Strict = Just GHC.Strict
translateExtension StrictData = Just GHC.StrictData
translateExtension TemplateHaskell = Just GHC.TemplateHaskell
translateExtension TemplateHaskellQuotes = Just GHC.TemplateHaskellQuotes
translateExtension TraditionalRecordSyntax = Just GHC.TraditionalRecordSyntax
translateExtension TransformListComp = Just GHC.TransformListComp
translateExtension TupleSections = Just GHC.TupleSections
translateExtension TypeApplications = Just GHC.TypeApplications
translateExtension TypeFamilies = Just GHC.TypeFamilies
translateExtension TypeFamilyDependencies = Just GHC.TypeFamilyDependencies
translateExtension TypeInType = Just GHC.TypeInType
translateExtension TypeOperators = Just GHC.TypeOperators
translateExtension TypeSynonymInstances = Just GHC.TypeSynonymInstances
translateExtension UnboxedTuples = Just GHC.UnboxedTuples
translateExtension UndecidableInstances = Just GHC.UndecidableInstances
translateExtension UndecidableSuperClasses = Just GHC.UndecidableSuperClasses
translateExtension UnicodeSyntax = Just GHC.UnicodeSyntax
translateExtension UnliftedFFITypes = Just GHC.UnliftedFFITypes
translateExtension ViewPatterns = Just GHC.ViewPatterns

translateExtension Safe = Nothing -- \df -> df { GHC.safeHaskell = GHC.Sf_Safe }
translateExtension SafeImports = Nothing -- \df -> df { GHC.safeHaskell = GHC.Sf_Safe }
translateExtension Trustworthy = Nothing -- \df -> df { GHC.safeHaskell = GHC.Sf_Trustworthy }
translateExtension Unsafe = Nothing -- \df -> df { GHC.safeHaskell = GHC.Sf_Unsafe }

translateExtension Rank2Types = Just GHC.RankNTypes
translateExtension PolymorphicComponents = Just GHC.RankNTypes
translateExtension Generics = Nothing -- it does nothing, deprecated extension
translateExtension NewQualifiedOperators = Nothing -- it does nothing, deprecated extension
translateExtension ExtensibleRecords = Nothing -- not in GHC
translateExtension XmlSyntax = Nothing -- not in GHC
translateExtension HereDocuments = Nothing -- not in GHC
translateExtension RegularPatterns = Nothing -- not in GHC
