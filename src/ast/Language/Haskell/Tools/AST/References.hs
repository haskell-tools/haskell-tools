{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
-- Generated references for handling the custom AST
module Language.Haskell.Tools.AST.References where

import Control.Reference hiding (element)
import Language.Haskell.Tools.AST.MakeASTReferences

import Language.Haskell.Tools.AST.Representation.Modules
import Language.Haskell.Tools.AST.Representation.TH
import Language.Haskell.Tools.AST.Representation.Decls
import Language.Haskell.Tools.AST.Representation.Binds
import Language.Haskell.Tools.AST.Representation.Exprs
import Language.Haskell.Tools.AST.Representation.Stmts
import Language.Haskell.Tools.AST.Representation.Patterns
import Language.Haskell.Tools.AST.Representation.Types
import Language.Haskell.Tools.AST.Representation.Kinds
import Language.Haskell.Tools.AST.Representation.Literals
import Language.Haskell.Tools.AST.Representation.Names
import Language.Haskell.Tools.AST.Ann

-- Modules
$(toASTReferences (makeReferences ''UModule))
$(toASTReferences (makeReferences ''UModuleHead))
$(toASTReferences (makeReferences ''UExportSpecs))
$(toASTReferences (makeReferences ''UExportSpec))
$(toASTReferences (makeReferences ''UIESpec))
$(toASTReferences (makeReferences ''USubSpec))
$(toASTReferences (makeReferences ''UModulePragma))
$(toASTReferences (makeReferences ''UImportDecl))
$(toASTReferences (makeReferences ''UImportSpec))
$(toASTReferences (makeReferences ''UImportQualified))
$(toASTReferences (makeReferences ''UImportSource))
$(toASTReferences (makeReferences ''UImportSafe))
$(toASTReferences (makeReferences ''UTypeNamespace))
$(toASTReferences (makeReferences ''UImportRenaming))

-- Declarations
$(toASTReferences (makeReferences ''UDecl))
$(toASTReferences (makeReferences ''UClassBody))
$(toASTReferences (makeReferences ''UClassElement))
$(toASTReferences (makeReferences ''UDeclHead))
$(toASTReferences (makeReferences ''UInstBody))
$(toASTReferences (makeReferences ''UInstBodyDecl))
$(toASTReferences (makeReferences ''UGadtConDecl))
$(toASTReferences (makeReferences ''UGadtConType))
$(toASTReferences (makeReferences ''UFunDeps))
$(toASTReferences (makeReferences ''UFunDep))
$(toASTReferences (makeReferences ''UConDecl))
$(toASTReferences (makeReferences ''UFieldDecl))
$(toASTReferences (makeReferences ''UDeriving))
$(toASTReferences (makeReferences ''UInstanceRule))
$(toASTReferences (makeReferences ''UInstanceHead))
$(toASTReferences (makeReferences ''UTypeEqn))
$(toASTReferences (makeReferences ''UKindConstraint))
$(toASTReferences (makeReferences ''UTyVar))
$(toASTReferences (makeReferences ''UType))
$(toASTReferences (makeReferences ''UKind))
$(toASTReferences (makeReferences ''UContext))
$(toASTReferences (makeReferences ''UAssertion))
$(toASTReferences (makeReferences ''UExpr))
$(toASTReferences (makeReferences ''UStmt'))
$(toASTReferences (makeReferences ''UCompStmt))
$(toASTReferences (makeReferences ''UValueBind))
$(toASTReferences (makeReferences ''UPattern))
$(toASTReferences (makeReferences ''UPatternField))
$(toASTReferences (makeReferences ''USplice))
$(toASTReferences (makeReferences ''QQString))
$(toASTReferences (makeReferences ''UMatch))
$(toASTReferences (makeReferences ''UAlt'))
$(toASTReferences (makeReferences ''URhs))
$(toASTReferences (makeReferences ''UGuardedRhs))
$(toASTReferences (makeReferences ''UFieldUpdate))
$(toASTReferences (makeReferences ''UBracket))
$(toASTReferences (makeReferences ''UTopLevelPragma))
$(toASTReferences (makeReferences ''URule))
$(toASTReferences (makeReferences ''UAnnotationSubject))
$(toASTReferences (makeReferences ''UMinimalFormula))
$(toASTReferences (makeReferences ''UExprPragma))
$(toASTReferences (makeReferences ''USourceRange))
$(toASTReferences (makeReferences ''Number))
$(toASTReferences (makeReferences ''UQuasiQuote))
$(toASTReferences (makeReferences ''URhsGuard))
$(toASTReferences (makeReferences ''ULocalBind))
$(toASTReferences (makeReferences ''ULocalBinds))
$(toASTReferences (makeReferences ''UFixitySignature))
$(toASTReferences (makeReferences ''UTypeSignature))
$(toASTReferences (makeReferences ''UListCompBody))
$(toASTReferences (makeReferences ''UTupSecElem))
$(toASTReferences (makeReferences ''UTypeFamily))
$(toASTReferences (makeReferences ''UTypeFamilySpec))
$(toASTReferences (makeReferences ''UInjectivityAnn))
$(toASTReferences (makeReferences ''UCaseRhs'))
$(toASTReferences (makeReferences ''UGuardedCaseRhs'))
$(toASTReferences (makeReferences ''UPatternSynonym))
$(toASTReferences (makeReferences ''UPatSynRhs))
$(toASTReferences (makeReferences ''UPatSynLhs))
$(toASTReferences (makeReferences ''UPatSynWhere))
$(toASTReferences (makeReferences ''UPatternTypeSignature))
$(toASTReferences (makeReferences ''URole))
$(toASTReferences (makeReferences ''ULanguageExtension))
$(toASTReferences (makeReferences ''UMatchLhs))

-- ULiteral
$(toASTReferences (makeReferences ''ULiteral))
$(toASTReferences (makeReferences ''UPromoted))

-- Base
$(toASTReferences (makeReferences ''UOperator))
$(toASTReferences (makeReferences ''UName))
$(toASTReferences (makeReferences ''UQualifiedName))
$(toASTReferences (makeReferences ''UModuleName))
$(toASTReferences (makeReferences ''UNamePart))
$(toASTReferences (makeReferences ''UStringNode))
$(toASTReferences (makeReferences ''UDataOrNewtypeKeyword))
$(toASTReferences (makeReferences ''UDoKind))
$(toASTReferences (makeReferences ''TypeKeyword))
$(toASTReferences (makeReferences ''UOverlapPragma))
$(toASTReferences (makeReferences ''UCallConv))
$(toASTReferences (makeReferences ''UArrowAppl))
$(toASTReferences (makeReferences ''USafety))
$(toASTReferences (makeReferences ''Assoc))
$(toASTReferences (makeReferences ''Precedence))
$(toASTReferences (makeReferences ''UPhaseControl))
$(toASTReferences (makeReferences ''PhaseNumber))
$(toASTReferences (makeReferences ''PhaseInvert))
