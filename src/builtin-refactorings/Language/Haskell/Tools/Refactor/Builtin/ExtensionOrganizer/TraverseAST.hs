{-# LANGUAGE FlexibleContexts, RankNTypes, TypeFamilies #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.TraverseAST
  ( module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.TraverseAST
  , module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
  ) where

import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor as Refact

import Control.Reference ((!~), (&), (&+&))

{-
   NOTE: We need Decl level checking for deriving clausese
         in order to gain extra information from the newtype and data keywords.

         We need Decl level checking for instance heads, in order to distinguish
         class instances from data and type family instances.
-}
chkDecl :: CheckNode Decl
chkDecl = chkFlexibleInstances >=> chkDerivings

chkPattern :: CheckNode Pattern
chkPattern = chkBangPatterns
         >=> chkViewPatterns
         >=> chkUnboxedTuplesPat

chkExpr :: CheckNode Expr
chkExpr = chkTupleSections
      >=> chkUnboxedTuplesExpr
      >=> chkLambdaCase

chkType :: CheckNode Type
chkType = chkUnboxedTuplesType

chkPatternField :: CheckNode PatternField
chkPatternField = chkRecordWildCardsPatField

chkFieldUpdate :: CheckNode FieldUpdate
chkFieldUpdate = chkRecordWildCardsFieldUpdate

chkPatternSynonym :: CheckNode PatternSynonym
chkPatternSynonym = chkPatternSynonymsSyn

chkPatternSignature :: CheckNode PatternSignature
chkPatternSignature = chkPatternSynonymsTypeSig

chkLiteral :: CheckNode Literal
chkLiteral = chkMagicHashLiteral

chkNamePart :: CheckNode NamePart
chkNamePart = chkMagicHashNamePart
          >=> chkTemplateHaskellhNamePart

chkKind :: CheckNode Kind
chkKind = chkMagicHashKind


traverseModule :: CheckNode UnnamedModule
traverseModule = (modDecl & annList !~ traverseDecl)
             >=> (modHead & annJust !~ traverseModuleHead)
             -- more

traverseModuleHead :: CheckNode ModuleHead
traverseModuleHead = mhExports & annJust !~ traverseExportSpecs
                    -- more

traverseExportSpecs :: CheckNode ExportSpecs
traverseExportSpecs = espExports & annList !~ traverseExportSpec

traverseExportSpec :: CheckNode ExportSpec
traverseExportSpec = exportDecl !~ traverseIESpec

traverseIESpec :: CheckNode IESpec
traverseIESpec = (ieName !~ traverseName)
             >=> (ieSubspec & annJust !~ traverseSubSpec)

traverseSubSpec :: CheckNode SubSpec
traverseSubSpec = essList & annList !~ traverseName

traverseDecl :: CheckNode Decl
traverseDecl = chkDecl
               >=> (declValBind !~ traverseValueBind)
               >=> (declBody & annJust !~ traverseClassBody)
               >=> (declSplice !~ traverseSplice)
               >=> (innerType !~ traverseType)
               >=> (declTypeSig !~ traverseTypeSignature)
               >=> (declTypeFamily !~ traverseTypeFamily) --
               >=> (declSpec & annJust !~ traverseTypeFamilySpec)
               >=> (declSafety & annJust !~ traverseSafety)
               >=> (declRoles & annList !~ traverseRole)
               >=> (declPragma !~ traverseTopLevelPragma)
               >=> (declPatTypeSig !~ traversePatternSignature)
               >=> (declPatSyn !~ traversePatternSynonym)
               >=> (declOverlap & annJust !~ traverseOverlapPragma)
               >=> (declKind & annJust !~ traverseKindContraint)
               >=> (innerInstanceRule !~ traverseInstanceRule)
               >=> (declInstDecl & annJust !~ traverseInstBody)
               >=> (declHead !~ traverseDeclHead)
               >=> (declGadt & annList !~ traverseGadtConDecl)
               >=> (declFunDeps & annJust !~ traverseFunDeps)
               >=> (declForeignType !~ traverseType)
               >=> (declFixity !~ traverseFixitySignature)
               >=> (declDeriving & annList !~ traverseDeriving)
               >=> (declDecl & annList !~ traverseTypeEqn)
               >=> (declCtx & annJust !~ traverseContext)
               >=> (declCons & annList !~ traverseConDecl)
               >=> (declCallConv !~ traverseCallConv)
               >=> (declName !~ traverseName)
               >=> (declRoleType !~ traverseQualifiedName)

  where innerType = (declTypes & annList)
                &+& declType
                &+& declAssignedType

        innerInstanceRule = declInstance &+& declInstRule

traverseTypeFamily :: CheckNode TypeFamily
traverseTypeFamily = (tfHead !~ traverseDeclHead)
                 >=> (tfSpec & annJust !~ traverseTypeFamilySpec)
                 >=> (tfKind & annJust !~ traverseKindContraint)

-- tfTypeVar is from 0.9
traverseTypeFamilySpec :: CheckNode TypeFamilySpec
traverseTypeFamilySpec = (tfSpecKind !~ traverseKindContraint)
                     -- >=> (tfTypeVar !~ traverseTyVar)
                     >=> (tfInjectivity !~ traverseInjectivityAnn)

traverseInjectivityAnn :: CheckNode InjectivityAnn
traverseInjectivityAnn = (injAnnRes !~ traverseTyVar)
                     >=> (injAnnDeps & annList !~ traverseName)

-- DONE
traverseSafety :: CheckNode Safety
traverseSafety = return

-- DONE
traverseRole :: CheckNode Role
traverseRole = return

traverseTopLevelPragma :: CheckNode TopLevelPragma
traverseTopLevelPragma = (pragmaRule & annList !~ traverseRule)
                     >=> (pragmaObjects & annList !~ traverseName)
                     >=> (annotationSubject !~ traverseAnnotationSubject)
                     >=> (pragmaInline !~ traverseInlinePragma)
                     >=> (specializePragma !~ traverseSpecializePragma)

-- no references for this
traverseSpecializePragma :: CheckUNode USpecializePragma
traverseSpecializePragma = return {- (specializeDef !~ traverseName)
                       >=> (specializeType & annList !~ traverseType) -}

traverseAnnotationSubject :: CheckNode AnnotationSubject
traverseAnnotationSubject = annotateName !~ traverseName

traverseRule :: CheckNode Rule
traverseRule = (ruleBounded & annList !~ traverseRuleVar)
           >=> (innerExpr !~ traverseExpr)
           -- some more

  where innerExpr = ruleLhs &+& ruleRhs

traverseRuleVar :: CheckNode RuleVar
traverseRuleVar = (ruleVarName !~ traverseName)
              >=> (ruleVarType !~ traverseType)

traversePatternSignature :: CheckNode PatternSignature
traversePatternSignature = chkPatternSignature
                       >=> (patSigName & annList !~ traverseName)
                       >=> (patSigType !~ traverseType)

-- DONE
traverseOverlapPragma :: CheckNode OverlapPragma
traverseOverlapPragma = return

-- weird structure of nodes (Maybe (Ann AnnListG dom stage))
traverseInstanceRule :: CheckNode InstanceRule
traverseInstanceRule = (irVars & annJust & element & annList !~ traverseTyVar)
                   >=> (irCtx & annJust !~ traverseContext)
                   >=> (irHead !~ traverseInstanceHead)

traverseInstanceHead :: CheckNode InstanceHead
traverseInstanceHead = (ihConName !~ traverseName)
                   >=> (ihOperator !~ traverseOperator)
                   >=> (innerType !~ traverseType)
                   >=> (innerIHead !~ traverseInstanceHead)

  where innerType = ihLeftOp &+& ihType
        innerIHead = ihHead &+& ihFun

traverseDeclHead :: CheckNode DeclHead
traverseDeclHead = (dhName !~ traverseName)
               >=> (dhOperator !~ traverseOperator)
               >=> (innerDHead !~ traverseDeclHead)
               >=> (innerTyVar !~ traverseTyVar)

  where innerDHead = dhBody &+& dhAppFun
        innerTyVar = dhAppOperand &+& dhLeft &+& dhRight

traverseGadtConDecl :: CheckNode GadtConDecl
traverseGadtConDecl = (gadtConNames & annList !~ traverseName)
                  >=> (gadtConTypeArgs & annList !~ traverseTyVar)
                  >=> (gadtConTypeCtx & annJust !~ traverseContext)
                  >=> (gadtConType !~ traverseGadtConType)

traverseGadtConType :: CheckNode GadtConType
traverseGadtConType = (innerType !~ traverseType)
                  >=> (gadtConRecordFields & annList !~ traverseFieldDecl)

  where innerType = gadtConNormalType &+& gadtConResultType

traverseFieldDecl :: CheckNode FieldDecl
traverseFieldDecl = (fieldNames & annList !~ traverseName)
                >=> (fieldType !~ traverseType)

traverseFunDeps :: CheckNode FunDeps
traverseFunDeps = funDeps & annList !~ traverseFunDep

traverseFunDep :: CheckNode FunDep
traverseFunDep = innerName !~ traverseName
  where innerName = (funDepLhs & annList) &+& (funDepRhs & annList)

traverseDeriving :: CheckNode Deriving
traverseDeriving = innerIHead !~ traverseInstanceHead
  where innerIHead = oneDerived &+& (allDerived & annList)

traverseConDecl :: CheckNode ConDecl
traverseConDecl = (conTypeArgs & annList !~ traverseTyVar)
              >=> (conTypeCtx & annJust !~ traverseContext)
              >=> (conDeclName !~ traverseName)
              >=> (innerType !~ traverseType)
              >=> (conDeclFields & annList !~ traverseFieldDecl)

  where innerType = (conDeclArgs & annList)
                &+& conDeclLhs
                &+& conDeclRhs

-- DONE
traverseCallConv :: CheckNode CallConv
traverseCallConv = return

traverseMinimalFormula :: CheckNode MinimalFormula
traverseMinimalFormula = (minimalName !~ traverseName)
                     >=> (innerFormula !~ traverseMinimalFormula)

  where innerFormula = minimalInner
                   &+& (minimalOrs & annList)
                   &+& (minimalAnds & annList)

-- there is no wrapper type for InlinePragma
-- no references generated
traverseInlinePragma :: CheckUNode UInlinePragma
traverseInlinePragma = return {- nnerName !~ traverseName
  where innerName = inlineDef &+& noInlineDef &+& inlinableDef -}




traversePatternSynonym :: CheckNode PatternSynonym
traversePatternSynonym = chkPatternSynonym
                         >=> (patRhs !~ traverseInnerRhs)
                         >=> (patLhs !~ traverseInnerLhs)

  where traverseInnerRhs = (patRhsPat !~ traversePattern)
          >=> (patRhsOpposite & annJust & patOpposite & annList !~ traverseMatch)

        traverseInnerLhs = (innerName !~ traverseName)
                           >=> (patSynOp !~ traverseOperator)

        innerName = patName &+& (patArgs & annList) &+& patSynLhs &+& patSynRhs



traverseMatch :: CheckNode Match
traverseMatch = (matchLhs !~ traverseMatchLhs)
                >=> (matchRhs !~ traverseRhs)
                >=> (matchBinds & annJust !~ traverseLocalBinds)

traverseMatchLhs :: CheckNode MatchLhs
traverseMatchLhs = (matchLhsName !~ traverseName)
                   >=> (matchLhsOperator !~ traverseOperator)
                   >=> (innerPattern !~ traversePattern)

  where innerPattern = matchLhsArgs & annList
                       &+& matchLhsLhs
                       &+& matchLhsRhs

traverseRhs :: CheckNode Rhs
traverseRhs = (rhsExpr !~ traverseExpr)
              >=> (rhsGuards & annList !~ traverseGuardedRhs)

traverseGuardedRhs :: CheckNode GuardedRhs
traverseGuardedRhs = (guardStmts & annList !~ traverseRhsGuard)
                     >=> (guardExpr !~ traverseExpr)

traverseRhsGuard :: CheckNode RhsGuard
traverseRhsGuard = (guardPat !~ traversePattern)
                   >=> (guardRhs &+& guardCheck !~ traverseExpr)
                   >=> (guardBinds & annList !~ traverseLocalBind)

traverseLocalBinds :: CheckNode LocalBinds
traverseLocalBinds = localBinds & annList !~ traverseLocalBind

traverseLocalBind :: CheckNode LocalBind
traverseLocalBind = (localVal !~ traverseValueBind)
                    >=> (localSig !~ traverseTypeSignature)
                    >=> (localFixity !~ traverseFixitySignature)
                    -- >=> (localInline !~ ...)


traverseInstBody :: CheckNode InstBody
traverseInstBody = instBodyDecls & annList !~ traverseInstBodyDecl

traverseInstBodyDecl :: CheckNode InstBodyDecl
traverseInstBodyDecl = (instBodyDeclFunbind !~ traverseValueBind)
                       >=> (instBodyTypeSig !~ traverseTypeSignature)
                       >=> (instBodyTypeEqn !~ traverseTypeEqn)
                       >=> (specializeInstanceType !~ traverseType)
                       -- and many more ...

traverseTypeEqn :: CheckNode TypeEqn
traverseTypeEqn = teLhs &+& teRhs !~ traverseType

traverseClassBody :: CheckNode ClassBody
traverseClassBody = cbElements & annList !~ traverseClassElem

traverseClassElem :: CheckNode ClassElement
traverseClassElem = (ceTypeSig !~ traverseTypeSignature)
                    >=> (clsFixity !~ traverseFixitySignature)
                    >=> (ceBind !~ traverseValueBind)
                    >=> (ceName !~ traverseName)
                    >=> (ceKind &+& ceType !~ traverseType)

                    -- some more, but are not important


traverseValueBind :: CheckNode ValueBind
traverseValueBind = (valBindPat !~ traversePattern)
                    >=> (valBindRhs !~ traverseRhs)
                    >=> (valBindLocals & annJust !~ traverseLocalBinds)
                    >=> (funBindMatches & annList !~ traverseMatch)


traversePattern :: CheckNode Pattern
traversePattern = chkPattern
                  >=> (innerPattern !~ traversePattern)
                  >=> (innerLiteral !~ traverseLiteral)
                  >=> (patternOperator !~ traverseOperator)
                  >=> (patternFields & annList !~ traversePatternField)
                  >=> (patternName !~ traverseName)
                  >=> (patternExpr !~ traverseExpr)
                  >=> (patternType !~ traverseType)
                  >=> (patternSplice !~ traverseSplice)
                  >=> (patQQ !~ traverseQuasiQuote)

  where
  innerPattern = patternLhs
                 &+& patternRhs
                 &+& patternInner
                 &+& (patternElems & annList)
                 &+& (patternArgs & annList)

  innerLiteral = patternLiteral &+& patternLit

traversePatternField :: CheckNode PatternField
traversePatternField = chkPatternField
                       >=> (fieldPatternName !~ traverseName)
                       >=> (fieldPattern !~ traversePattern)

-- TODO: TemplateHaskell?
traverseExpr :: CheckNode Expr
traverseExpr = chkExpr
              >=> (innerExpressions !~ traverseExpr)
              >=> (innerPatterns !~ traversePattern)
              >=> (exprFunBind & annList !~ traverseLocalBind)
              >=> (exprIfAlts & annList !~ traverseGuardedCaseRhs traverseExpr)
              >=> (exprAlts & annList !~ traverseAlt traverseExpr)
              >=> (exprOperator !~ traverseOperator)
              >=> (exprLit !~ traverseLiteral)
              >=> (innerNames !~ traverseName)
              >=> (exprStmts & annList !~ traverseStmt traverseExpr)
              >=> (tupleSectionElems & annList !~ traverseTupSecElem)
              >=> (exprRecFields & annList !~ traverseFieldUpdate)
              >=> (compBody & annList !~ traverseListCompBody)
              >=> (innerTypes !~ traverseType)
              >=> (exprSplice !~ traverseSplice)
              >=> (exprBracket !~ traverseBracket)
              >=> (exprQQ !~ traverseQuasiQuote)

 where innerExpressions = (tupleElems & annList)
                         &+& (listElems & annList)
                         &+& innerExpr
                         &+& exprCond
                         &+& exprThen
                         &+& exprElse
                         &+& exprRhs
                         &+& exprLhs
                         &+& exprInner
                         &+& exprFun
                         &+& exprCase
                         &+& exprArg
                         &+& enumToFix
                         &+& (enumTo & annJust)
                         &+& (enumThen & annJust)
                         &+& Refact.enumFrom
                         &+& compExpr

       innerPatterns = (exprBindings & annList) &+& procPattern
       innerNames    = exprName &+& exprRecName &+& quotedName
       innerTypes    = exprSig &+& exprType

-- These types are needed to generalize AST traversal for polymorphic nodes.
-- These nodes are polymorphic in their inner nodes, which can be any "UNodes"
-- (UType. UExpr, UDecl etc ...)
type AltG uexpr dom            = Ann (UAlt' uexpr) dom SrcTemplateStage
type StmtG uexpr dom           = Ann (UStmt' uexpr) dom SrcTemplateStage
type CaseRhsG uexpr dom        = Ann (UCaseRhs' uexpr) dom SrcTemplateStage
type GuardedCaseRhsG uexpr dom = Ann (UGuardedCaseRhs' uexpr) dom SrcTemplateStage

type PromotedG t dom = Ann (UPromoted t) dom  SrcTemplateStage

traverseAlt :: CheckUNode uexpr -> CheckNode (AltG uexpr IdDom)
traverseAlt f = (altPattern !~ traversePattern)
               >=> (altRhs !~ traverseCaseRhs f)
               >=> (altBinds & annJust !~ traverseLocalBinds)

traverseCaseRhs :: CheckUNode uexpr -> CheckNode (CaseRhsG uexpr IdDom)
traverseCaseRhs f = (rhsCaseExpr !~ f)
                   >=> (rhsCaseGuards & annList !~ traverseGuardedCaseRhs f)

traverseGuardedCaseRhs :: CheckUNode uexpr -> CheckNode (GuardedCaseRhsG uexpr IdDom)
traverseGuardedCaseRhs f = (caseGuardStmts & annList !~ traverseRhsGuard)
                          >=> (caseGuardExpr !~ f)

traverseStmt :: CheckUNode uexpr -> CheckNode (StmtG uexpr IdDom)
traverseStmt f = (stmtPattern !~ traversePattern)
                >=> (stmtExpr !~ f)
                >=> (stmtBinds & annList !~ traverseLocalBind)
                >=> (cmdStmtBinds & annList !~ traverseStmt f)

traversePromoted :: CheckUNode t -> CheckNode (PromotedG t IdDom)
traversePromoted f = (promotedConName !~ traverseName)
                 >=> (promotedElements & annList !~ f)

traverseTupSecElem :: CheckNode TupSecElem
traverseTupSecElem = tupSecExpr !~ traverseExpr

traverseFieldUpdate :: CheckNode FieldUpdate
traverseFieldUpdate = chkFieldUpdate
                      >=> (fieldName &+& fieldUpdateName !~ traverseName)
                      >=> (fieldValue !~ traverseExpr)

traverseListCompBody :: CheckNode ListCompBody
traverseListCompBody = compStmts & annList !~ traverseCompStmt

traverseCompStmt :: CheckNode CompStmt
traverseCompStmt = (compStmt !~ traverseStmt traverseExpr)
                  >=> (innerExpressions !~ traverseExpr)

 where innerExpressions = thenExpr
                          &+& (byExpr & annJust)
                          &+& (usingExpr & annJust)

traverseCmd :: CheckNode Cmd
traverseCmd = return
{-

References are not generated for UCmd

traverseCmd = (innerExpressions !~ traverseExpr)
             >=> (innerCmds !~ traverseCmd)
             >=> (cmdStmtBinds & annList !~ traversePattern)
             >=> (cmdBinds & annList !~ traverseLocalBind)
             >=> (cmdAlts & annList !~ traverseAlt traverseCmd)
             >=> (cmdStmts & annList !~ traverseStmt traverseCmd)
 where innerExpressions = cmdLhs &+& cmdRhs &+& cmdExpr &+& cmdApplied

       innerCmds = (cmdInnerCmds & annList)
                   &+& cmdInnerCmd
                   &+& cmdLeftCmd
                   &+& cmdRightCmd
                   &+& cmdInner
                   &+& cmdThen
                   &+& cmdElse
-}

traverseSplice :: CheckNode Splice
traverseSplice = chkTemplateHaskellSplice
                 >=> (spliceId !~ traverseName)
                 >=> (spliceExpr !~ traverseExpr)

traverseBracket :: CheckNode Bracket
traverseBracket = chkTemplateHaskellBracket
                  >=> (bracketExpr !~ traverseExpr)
                  >=> (bracketPattern !~ traversePattern)
                  >=> (bracketType !~ traverseType)
                  >=> (bracketDecl & annList !~ traverseDecl)

traverseQuasiQuote :: CheckNode QuasiQuote
traverseQuasiQuote = chkTemplateHaskellQuasiQuote
                     >=> (qqExprName !~ traverseName)

traverseType :: CheckNode Type
traverseType = chkType
              >=> (typeBounded & annList !~ traverseTyVar)
              >=> (innerType !~ traverseType)
              >=> (innerName !~ traverseName)
              >=> (typeCtx !~ traverseContext)
              >=> (typeOperator !~ traverseOperator)
              >=> (typeKind !~ traverseKind)
              >=> (tpPromoted !~ traversePromoted traverseType)
              >=> (tsSplice !~ traverseSplice)
              >=> (typeQQ !~ traverseQuasiQuote)

 where innerType = typeType
                   &+& typeParam
                   &+& typeResult
                   &+& (typeElements & annList)
                   &+& typeElement
                   &+& typeCon
                   &+& typeArg
                   &+& typeInner
                   &+& typeLeft
                   &+& typeRight

       innerName = typeName &+& typeWildcardName

traverseTyVar :: CheckNode TyVar
traverseTyVar = (tyVarName !~ traverseName)
               >=> (tyVarKind & annJust !~ traverseKindContraint)

traverseKindContraint :: CheckNode KindConstraint
traverseKindContraint = kindConstr !~ traverseKind

traverseKind :: CheckNode Kind
traverseKind = chkKind
           >=> (innerKind !~ traverseKind)
           >=> (kindVar !~ traverseName)
           >=> (kindAppOp !~ traverseOperator)
           >=> (kindPromoted !~ traversePromoted traverseKind)

  where innerKind = kindLeft
                &+& kindRight
                &+& kindParen
                &+& kindAppFun
                &+& kindAppArg
                &+& kindLhs
                &+& kindRhs
                &+& kindElem
                &+& (kindElems & annList)

traverseContext :: CheckNode Context
traverseContext = contextAssertion !~ traverseAssertion

traverseAssertion :: CheckNode Assertion
traverseAssertion = (innerType !~ traverseType)
                   >=> (innerName !~ traverseName)
                   >=> (assertOp !~ traverseOperator)
                   >=> (innerAsserts & annList !~ traverseAssertion)

 where innerType = (assertTypes & annList)
                   &+& assertLhs
                   &+& assertRhs
                   &+& assertImplType

       innerName = assertClsName &+& assertImplVar

traverseName :: CheckNode Name
traverseName = simpleName !~ traverseQualifiedName

traverseQualifiedName :: CheckNode QualifiedName
traverseQualifiedName = unqualifiedName !~ traverseNamePart
                   >=> qualifiers & annList !~ traverseNamePart

traverseNamePart :: CheckNode NamePart
traverseNamePart = chkNamePart

traverseLiteral :: CheckNode Literal
traverseLiteral = chkLiteral

traverseOperator :: CheckNode Operator
traverseOperator = operatorName !~ traverseQualifiedName

traverseTypeSignature :: CheckNode TypeSignature
traverseTypeSignature = (tsName & annList !~ traverseName)
                    >=> (tsType !~ traverseType)

traverseFixitySignature :: CheckNode FixitySignature
traverseFixitySignature = fixityOperators & annList !~ traverseOperator
