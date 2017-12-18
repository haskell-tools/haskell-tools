-- don't show warnings, Template Haskell generated code contains unused variables
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE TypeApplications, TemplateHaskell, DataKinds, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, TypeFamilies, AllowAmbiguousTypes, ScopedTypeVariables #-}
-- | Generating instances for traversing the semantic information of the Haskell Representation
module Language.Haskell.Tools.AST.Instances.ClassyPlate where

import Data.Generics.ClassyPlate
import Data.Generics.ClassyPlate.TH

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Instances.Data
import Language.Haskell.Tools.AST.Instances.Generic

import Language.Haskell.Tools.AST.Representation.Binds
import Language.Haskell.Tools.AST.Representation.Decls
import Language.Haskell.Tools.AST.Representation.Exprs
import Language.Haskell.Tools.AST.Representation.Kinds
import Language.Haskell.Tools.AST.Representation.Literals
import Language.Haskell.Tools.AST.Representation.Modules
import Language.Haskell.Tools.AST.Representation.Names
import Language.Haskell.Tools.AST.Representation.Patterns
import Language.Haskell.Tools.AST.Representation.Stmts
import Language.Haskell.Tools.AST.Representation.TH
import Language.Haskell.Tools.AST.Representation.Types

makeClassyPlateConfig OnlyDirect [Right '_annotation] ''Ann
makeClassyPlateConfig OnlyDirect [Right '_annMaybeAnnot] ''AnnMaybeG
makeClassyPlateConfig OnlyDirect [Right '_annListAnnot] ''AnnListG

makeClassyPlateConfig OnlyDirect [] ''Maybe
makeClassyPlateConfig OnlyDirect [] ''[]

-- Modules
makeClassyPlateConfig OnlyDirect [] ''UModule
makeClassyPlateConfig OnlyDirect [] ''UModuleHead
makeClassyPlateConfig OnlyDirect [] ''UExportSpecs
makeClassyPlateConfig OnlyDirect [] ''UExportSpec
makeClassyPlateConfig OnlyDirect [] ''UIESpec
makeClassyPlateConfig OnlyDirect [] ''USubSpec
makeClassyPlateConfig OnlyDirect [] ''UModulePragma
makeClassyPlateConfig OnlyDirect [] ''UFilePragma
makeClassyPlateConfig OnlyDirect [] ''UImportDecl
makeClassyPlateConfig OnlyDirect [] ''UImportSpec
makeClassyPlateConfig OnlyDirect [] ''UImportModifier
makeClassyPlateConfig OnlyDirect [] ''UImportQualified
makeClassyPlateConfig OnlyDirect [] ''UImportSource
makeClassyPlateConfig OnlyDirect [] ''UImportSafe
makeClassyPlateConfig OnlyDirect [] ''UTypeNamespace
makeClassyPlateConfig OnlyDirect [] ''UImportRenaming

-- Declarations
makeClassyPlateConfig OnlyDirect [] ''UDecl
makeClassyPlateConfig OnlyDirect [] ''UClassBody
makeClassyPlateConfig OnlyDirect [] ''UClassElement
makeClassyPlateConfig OnlyDirect [] ''UDeclHead
makeClassyPlateConfig OnlyDirect [] ''UInstBody
makeClassyPlateConfig OnlyDirect [] ''UInstBodyDecl
makeClassyPlateConfig OnlyDirect [] ''UGadtConDecl
makeClassyPlateConfig OnlyDirect [] ''UGadtConType
makeClassyPlateConfig OnlyDirect [] ''UFieldWildcard
makeClassyPlateConfig OnlyDirect [] ''UFunDeps
makeClassyPlateConfig OnlyDirect [] ''UFunDep
makeClassyPlateConfig OnlyDirect [] ''UConDecl
makeClassyPlateConfig OnlyDirect [] ''UFieldDecl
makeClassyPlateConfig OnlyDirect [] ''UDeriving
makeClassyPlateConfig OnlyDirect [] ''UDeriveStrategy
makeClassyPlateConfig OnlyDirect [] ''UInstanceRule
makeClassyPlateConfig OnlyDirect [] ''UInstanceHead
makeClassyPlateConfig OnlyDirect [] ''UTypeEqn
makeClassyPlateConfig OnlyDirect [] ''UKindConstraint
makeClassyPlateConfig OnlyDirect [] ''UTyVar
makeClassyPlateConfig OnlyDirect [] ''UType
makeClassyPlateConfig OnlyDirect [] ''UKind
makeClassyPlateConfig OnlyDirect [] ''UContext
makeClassyPlateConfig OnlyDirect [] ''UAssertion
makeClassyPlateConfig OnlyDirect [] ''UExpr
makeClassyPlateConfig OnlyDirect [] ''UCompStmt
makeClassyPlateConfig OnlyDirect [] ''UValueBind
makeClassyPlateConfig OnlyDirect [] ''UPattern
makeClassyPlateConfig OnlyDirect [] ''UPatternField
makeClassyPlateConfig OnlyDirect [] ''USplice
makeClassyPlateConfig OnlyDirect [Right '_qqString] ''QQString
makeClassyPlateConfig OnlyDirect [] ''UMatch
makeClassyPlateConfig OnlyDirect [] ''URhs
makeClassyPlateConfig OnlyDirect [] ''UGuardedRhs
makeClassyPlateConfig OnlyDirect [] ''UFieldUpdate
makeClassyPlateConfig OnlyDirect [] ''UBracket
makeClassyPlateConfig OnlyDirect [] ''UTopLevelPragma
makeClassyPlateConfig OnlyDirect [] ''URule
makeClassyPlateConfig OnlyDirect [] ''URuleVar
makeClassyPlateConfig OnlyDirect [] ''UAnnotationSubject
makeClassyPlateConfig OnlyDirect [] ''UMinimalFormula
makeClassyPlateConfig OnlyDirect [] ''UExprPragma
makeClassyPlateConfig OnlyDirect [] ''USourceRange
makeClassyPlateConfig OnlyDirect [Right '_numberInteger] ''Number
makeClassyPlateConfig OnlyDirect [] ''UQuasiQuote
makeClassyPlateConfig OnlyDirect [] ''URhsGuard
makeClassyPlateConfig OnlyDirect [] ''ULocalBind
makeClassyPlateConfig OnlyDirect [] ''ULocalBinds
makeClassyPlateConfig OnlyDirect [] ''UFixitySignature
makeClassyPlateConfig OnlyDirect [] ''UTypeSignature
makeClassyPlateConfig OnlyDirect [] ''UListCompBody
makeClassyPlateConfig OnlyDirect [] ''UTupSecElem
makeClassyPlateConfig OnlyDirect [] ''UTypeFamily
makeClassyPlateConfig OnlyDirect [] ''UTypeFamilySpec
makeClassyPlateConfig OnlyDirect [] ''UInjectivityAnn
makeClassyPlateConfig OnlyDirect [] ''UPatternSynonym
makeClassyPlateConfig OnlyDirect [] ''UPatSynRhs
makeClassyPlateConfig OnlyDirect [] ''UPatSynLhs
makeClassyPlateConfig OnlyDirect [] ''UPatSynWhere
makeClassyPlateConfig OnlyDirect [] ''UPatternTypeSignature
makeClassyPlateConfig OnlyDirect [] ''URole
makeClassyPlateConfig OnlyDirect [] ''UCmd
makeClassyPlateConfig OnlyDirect [Right '_langExt] ''ULanguageExtension
makeClassyPlateConfig OnlyDirect [] ''UMatchLhs
makeClassyPlateConfig OnlyDirect [] ''UStmt'
makeClassyPlateConfig OnlyDirect [] ''UAlt'
makeClassyPlateConfig OnlyDirect [] ''UCaseRhs'
makeClassyPlateConfig OnlyDirect [] ''UGuardedCaseRhs'
makeClassyPlateConfig OnlyDirect [] ''UInlinePragma
makeClassyPlateConfig OnlyDirect [] ''USpecializePragma
makeClassyPlateConfig OnlyDirect [] ''UUnboxedSumPlaceHolder

-- ULiteral
makeClassyPlateConfig OnlyDirect [ Right '_charLitValue
                        , Right '_stringLitValue
                        , Right '_intLitValue
                        , Right '_fracLitValue
                        , Right '_intLitValue
                        , Right '_intLitValue
                        , Right '_floatLitValue
                        , Right '_floatLitValue
                        , Right '_charLitValue
                        , Right '_stringLitValue
                        ] ''ULiteral
makeClassyPlateConfig OnlyDirect [ Right '_promotedIntValue
                        , Right '_promotedStringValue
                        ] ''UPromoted

-- Base
makeClassyPlateConfig OnlyDirect [] ''UOperator
makeClassyPlateConfig OnlyDirect [] ''UName
makeClassyPlateConfig OnlyDirect [] ''UQualifiedName
makeClassyPlateConfig OnlyDirect [Right '_moduleNameString] ''UModuleName
makeClassyPlateConfig OnlyDirect [Right '_simpleNameStr] ''UNamePart
makeClassyPlateConfig OnlyDirect [Right '_stringNodeStr] ''UStringNode
makeClassyPlateConfig OnlyDirect [] ''UDataOrNewtypeKeyword
makeClassyPlateConfig OnlyDirect [] ''UDoKind
makeClassyPlateConfig OnlyDirect [] ''TypeKeyword
makeClassyPlateConfig OnlyDirect [] ''UOverlapPragma
makeClassyPlateConfig OnlyDirect [] ''UCallConv
makeClassyPlateConfig OnlyDirect [] ''UArrowAppl
makeClassyPlateConfig OnlyDirect [] ''USafety
makeClassyPlateConfig OnlyDirect [] ''UConlikeAnnot
makeClassyPlateConfig OnlyDirect [] ''Assoc
makeClassyPlateConfig OnlyDirect [Right '_precedenceValue] ''Precedence
makeClassyPlateConfig OnlyDirect [Right '_lineNumber] ''LineNumber
makeClassyPlateConfig OnlyDirect [] ''UPhaseControl
makeClassyPlateConfig OnlyDirect [Right '_phaseNum] ''PhaseNumber
makeClassyPlateConfig OnlyDirect [] ''PhaseInvert
