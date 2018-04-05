-- don't show warnings, Template Haskell generated code contains unused variables
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- | Generated references for AST elements.
module Language.Haskell.Tools.AST.References where

import Control.Reference hiding (element)
import Language.Haskell.Tools.AST.MakeASTReferences

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

-- * Modules
$(toASTReferences (makeReferences ''UModule))
$(toASTReferences (makeReferences ''UModuleHead))
$(toASTReferences (makeReferences ''UExportSpecs))
$(toASTReferences (makeReferences ''UExportSpec))
$(toASTReferences (makeReferences ''UIESpec))
$(toASTReferences (makeReferences ''USubSpec))
$(toASTReferences (makeReferences ''UModulePragma))
$(toASTReferences (makeReferences ''UFilePragma))
$(toASTReferences (makeReferences ''UImportDecl))
$(toASTReferences (makeReferences ''UImportSpec))
$(toASTReferences (makeReferences ''UImportQualified))
$(toASTReferences (makeReferences ''UImportSource))
$(toASTReferences (makeReferences ''UImportSafe))
$(toASTReferences (makeReferences ''UTypeNamespace))
$(toASTReferences (makeReferences ''UImportRenaming))
$(toASTReferences (makeReferences ''UModuleName))
$(toASTReferences (makeReferences ''ULanguageExtension))
$(toASTReferences (makeReferences ''TypeKeyword))

-- * Declarations
$(toASTReferences (makeReferences ''UDecl))
$(toASTReferences (makeReferences ''UClassBody))
$(toASTReferences (makeReferences ''UClassElement))
$(toASTReferences (makeReferences ''UDeclHead))
$(toASTReferences (makeReferences ''UInstBody))
$(toASTReferences (makeReferences ''UInstBodyDecl))
$(toASTReferences (makeReferences ''UTypeFamily))
$(toASTReferences (makeReferences ''UTypeFamilySpec))
$(toASTReferences (makeReferences ''UInjectivityAnn))
$(toASTReferences (makeReferences ''UDataOrNewtypeKeyword))
$(toASTReferences (makeReferences ''UGadtConDecl))
$(toASTReferences (makeReferences ''UGadtConType))
$(toASTReferences (makeReferences ''UPatternSynonym))
$(toASTReferences (makeReferences ''UPatSynRhs))
$(toASTReferences (makeReferences ''UPatSynLhs))
$(toASTReferences (makeReferences ''UPatSynWhere))
$(toASTReferences (makeReferences ''UPatternTypeSignature))
$(toASTReferences (makeReferences ''URole))
$(toASTReferences (makeReferences ''UFunDeps))
$(toASTReferences (makeReferences ''UFunDep))
$(toASTReferences (makeReferences ''UConDecl))
$(toASTReferences (makeReferences ''UFieldDecl))
$(toASTReferences (makeReferences ''UDeriving))
$(toASTReferences (makeReferences ''UDeriveStrategy))
$(toASTReferences (makeReferences ''UInstanceRule))
$(toASTReferences (makeReferences ''UInstanceHead))
$(toASTReferences (makeReferences ''UTypeEqn))
$(toASTReferences (makeReferences ''URule))
$(toASTReferences (makeReferences ''URuleVar))
$(toASTReferences (makeReferences ''UOverlapPragma))
$(toASTReferences (makeReferences ''UCallConv))
$(toASTReferences (makeReferences ''USafety))
$(toASTReferences (makeReferences ''UPhaseControl))
$(toASTReferences (makeReferences ''PhaseNumber))
$(toASTReferences (makeReferences ''PhaseInvert))
$(toASTReferences (makeReferences ''UTopLevelPragma))
$(toASTReferences (makeReferences ''UAnnotationSubject))
$(toASTReferences (makeReferences ''UMinimalFormula))
$(toASTReferences (makeReferences ''USourceRange))
$(toASTReferences (makeReferences ''Number))
$(toASTReferences (makeReferences ''UUnboxedSumPlaceHolder))
$(toASTReferences (makeReferences ''USpecializePragma))


-- * Binds
$(toASTReferences (makeReferences ''UMatch))
$(toASTReferences (makeReferences ''URhs))
$(toASTReferences (makeReferences ''UGuardedRhs))
$(toASTReferences (makeReferences ''URhsGuard))
$(toASTReferences (makeReferences ''ULocalBind))
$(toASTReferences (makeReferences ''ULocalBinds))
$(toASTReferences (makeReferences ''UFixitySignature))
$(toASTReferences (makeReferences ''Assoc))
$(toASTReferences (makeReferences ''Precedence))
$(toASTReferences (makeReferences ''UTypeSignature))
$(toASTReferences (makeReferences ''UMatchLhs))

-- * Kinds
$(toASTReferences (makeReferences ''UKindConstraint))

-- * Types
$(toASTReferences (makeReferences ''UValueBind))
$(toASTReferences (makeReferences ''UTyVar))
$(toASTReferences (makeReferences ''UType))
$(toASTReferences (makeReferences ''UKind))
$(toASTReferences (makeReferences ''UContext))
$(toASTReferences (makeReferences ''UAssertion))

-- * Expressions
$(toASTReferences (makeReferences ''UExpr))
$(toASTReferences (makeReferences ''UAlt'))
$(toASTReferences (makeReferences ''UFieldUpdate))
$(toASTReferences (makeReferences ''UTupSecElem))
$(toASTReferences (makeReferences ''UExprPragma))
$(toASTReferences (makeReferences ''UCaseRhs'))
$(toASTReferences (makeReferences ''UGuardedCaseRhs'))
$(toASTReferences (makeReferences ''UArrowAppl))

-- * Statements
$(toASTReferences (makeReferences ''UStmt'))
$(toASTReferences (makeReferences ''UCompStmt))
$(toASTReferences (makeReferences ''UListCompBody))
$(toASTReferences (makeReferences ''UDoKind))
$(toASTReferences (makeReferences ''UCmd))

-- * Patterns
$(toASTReferences (makeReferences ''UPattern))
$(toASTReferences (makeReferences ''UPatternField))

-- * Template Haskell
$(toASTReferences (makeReferences ''USplice))
$(toASTReferences (makeReferences ''UQuasiQuote))
$(toASTReferences (makeReferences ''QQString))
$(toASTReferences (makeReferences ''UBracket))

-- * Literals
$(toASTReferences (makeReferences ''ULiteral))
$(toASTReferences (makeReferences ''UPromoted))

-- * Names
$(toASTReferences (makeReferences ''UOperator))
$(toASTReferences (makeReferences ''UName))
$(toASTReferences (makeReferences ''UQualifiedName))
$(toASTReferences (makeReferences ''UNamePart))
$(toASTReferences (makeReferences ''UStringNode))
