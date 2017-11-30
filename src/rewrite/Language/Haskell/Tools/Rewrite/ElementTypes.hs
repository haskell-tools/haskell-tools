module Language.Haskell.Tools.Rewrite.ElementTypes where

import Language.Haskell.Tools.AST

type AnnList node = AnnListG node IdDom SrcTemplateStage
type AnnMaybe node = AnnMaybeG node IdDom SrcTemplateStage

-- * Modules

-- | The representation of a haskell module, that is a separate compilation unit.
-- It may or may not have a header.
type Module = Ann UModule IdDom SrcTemplateStage

-- | Module declaration with name and (optional) exports
type ModuleHead = Ann UModuleHead IdDom SrcTemplateStage

-- | A list of export specifications surrounded by parentheses
type ExportSpecs = Ann UExportSpecs IdDom SrcTemplateStage

-- | Export specifier
type ExportSpec = Ann UExportSpec IdDom SrcTemplateStage

-- | Marks a name to be imported or exported with related names (subspecifier)
type IESpec = Ann UIESpec IdDom SrcTemplateStage

-- | Marks how related names will be imported or exported with a given name
type SubSpec = Ann USubSpec IdDom SrcTemplateStage

-- | Pragmas that must be used after the module head
type ModulePragma = Ann UModulePragma IdDom SrcTemplateStage

-- | Pragmas that must be used before defining the module
type FilePragma = Ann UFilePragma IdDom SrcTemplateStage

-- | An import declaration: @import Module.Name@
type ImportDecl = Ann UImportDecl IdDom SrcTemplateStage

-- | Restriction on the imported names
type ImportSpec = Ann UImportSpec IdDom SrcTemplateStage

-- | Marks the import as qualified: @qualified@
type ImportQualified = Ann UImportQualified IdDom SrcTemplateStage

-- | Marks the import as source: @{-\# SOURCE \#-}@
type ImportSource = Ann UImportSource IdDom SrcTemplateStage

-- | Marks the import as safe: @safe@
type ImportSafe = Ann UImportSafe IdDom SrcTemplateStage

-- | Marks an imported name to belong to the type namespace: @type@
type TypeNamespace = Ann UTypeNamespace IdDom SrcTemplateStage

-- | Renaming imports (@ as A @)
type ImportRenaming = Ann UImportRenaming IdDom SrcTemplateStage

-- | The name of a module
type ModuleName = Ann UModuleName IdDom SrcTemplateStage

-- | The name of the enabled language extension, for example (@ LambdaCase @)
type LanguageExtension = Ann ULanguageExtension IdDom SrcTemplateStage

-- * Declarations

-- | Haskell declaration
type Decl = Ann UDecl IdDom SrcTemplateStage

-- | The list of declarations that can appear in a typeclass
type ClassBody = Ann UClassBody IdDom SrcTemplateStage

-- | Members of a class declaration
type ClassElement = Ann UClassElement IdDom SrcTemplateStage

-- The declared (possibly parameterized) type (@ A x :+: B y @).
type DeclHead = Ann UDeclHead IdDom SrcTemplateStage

-- | Instance body is the implementation of the class functions (@ where a x = 1; b x = 2 @)
type InstBody = Ann UInstBody IdDom SrcTemplateStage

-- | Declarations inside an instance declaration.
type InstBodyDecl = Ann UInstBodyDecl IdDom SrcTemplateStage

-- | GADT constructor declaration (@ D1 :: { val :: Int } -> T String @)
type GadtConDecl = Ann UGadtConDecl IdDom SrcTemplateStage

-- | Type of GADT constructors (can be record types: @{ val :: Int }@)
type GadtConType = Ann UGadtConType IdDom SrcTemplateStage

-- | Marker for a field wildcard. Only needed to attach semantic information in a type-safe way.
type FieldWildcard = Ann UFieldWildcard IdDom SrcTemplateStage

-- | A list of functional dependencies: @ | a -> b, c -> d @ separated by commas
type FunDeps = Ann UFunDeps IdDom SrcTemplateStage

-- | A functional dependency, given on the form @l1 ... ln -> r1 ... rn@
type FunDep = Ann UFunDep IdDom SrcTemplateStage

-- | A constructor declaration for a datatype
type ConDecl = Ann UConDecl IdDom SrcTemplateStage

-- | The @data@ or the @newtype@ keyword to define ADTs.
type DataOrNewtypeKeyword = Ann UDataOrNewtypeKeyword IdDom SrcTemplateStage

-- | Field declaration (@ fld :: Int @)
type FieldDecl = Ann UFieldDecl IdDom SrcTemplateStage

-- | A deriving clause following a data type declaration. (@ deriving Show @ or @ deriving (Show, Eq) @)
type Deriving = Ann UDeriving IdDom SrcTemplateStage

-- | A deriving strategy (@stock@, @newtype@ or @anyclass@)
type DeriveStrategy = Ann UDeriveStrategy IdDom SrcTemplateStage

-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the where keyword.
type InstanceRule = Ann UInstanceRule IdDom SrcTemplateStage

-- | The specification of the class instance declaration
type InstanceHead = Ann UInstanceHead IdDom SrcTemplateStage

-- | Specialize pragma (@ {-# SPECIALISE f :: Int -> b -> b #-} @)
type SpecializePragma = Ann USpecializePragma IdDom SrcTemplateStage

-- | Overlap pragmas. Can be applied to class declarations and class instance declarations.
type OverlapPragma = Ann UOverlapPragma IdDom SrcTemplateStage

-- | Type equations as found in closed type families (@ T A = S @)
type TypeEqn = Ann UTypeEqn IdDom SrcTemplateStage

-- | Top level pragmas
type TopLevelPragma = Ann UTopLevelPragma IdDom SrcTemplateStage

-- | A rewrite rule (@ "map/map" forall f g xs. map f (map g xs) = map (f.g) xs @)
type Rule = Ann URule IdDom SrcTemplateStage

-- | A variable for a rewrite rule. With or without type signature.
type RuleVar = Ann URuleVar IdDom SrcTemplateStage

-- | Annotation allows you to connect an expression to any declaration.
type AnnotationSubject = Ann UAnnotationSubject IdDom SrcTemplateStage

-- | Formulas of minimal annotations declaring which functions should be defined.
type MinimalFormula = Ann UMinimalFormula IdDom SrcTemplateStage

-- | In-AST source ranges (for generated pragmas)
type SourceRange = Ann USourceRange IdDom SrcTemplateStage

-- | Open type and data families
type TypeFamily = Ann UTypeFamily IdDom SrcTemplateStage

-- | Type family specification with kinds specification and injectivity.
type TypeFamilySpec = Ann UTypeFamilySpec IdDom SrcTemplateStage

-- | Injectivity annotation for type families (@ = r | r -> a @)
type InjectivityAnn = Ann UInjectivityAnn IdDom SrcTemplateStage

-- | Pattern synonyms: @ pattern Arrow t1 t2 = App "->" [t1, t2] @
type PatternSynonym = Ann UPatternSynonym IdDom SrcTemplateStage

-- | Right-hand side of pattern synonym
type PatSynRhs = Ann UPatSynRhs IdDom SrcTemplateStage

-- | Left hand side of a pattern synonym
type PatSynLhs = Ann UPatSynLhs IdDom SrcTemplateStage

-- | Where clause of pattern synonym (explicit expression direction)
type PatSynWhere = Ann UPatSynWhere IdDom SrcTemplateStage

-- | Pattern type signature declaration (@ pattern Succ :: Int -> Int @)
type PatternSignature = Ann UPatternTypeSignature IdDom SrcTemplateStage

-- | Role annotations for types
type Role = Ann URole IdDom SrcTemplateStage

-- | Call conventions of foreign functions
type CallConv = Ann UCallConv IdDom SrcTemplateStage

-- | Safety annotations for foreign calls
type Safety = Ann USafety IdDom SrcTemplateStage

-- | A @CONLIKE@ modifier for an @INLINE@ pragma.
type ConlikeAnnot = Ann UConlikeAnnot IdDom SrcTemplateStage

-- | Controls the activation of a rewrite rule (@ [1] @)
type PhaseControl = Ann UPhaseControl IdDom SrcTemplateStage

-- * Binds

-- | Value binding for top-level and local bindings
type ValueBind = Ann UValueBind IdDom SrcTemplateStage

-- | Clause of function binding
type Match = Ann UMatch IdDom SrcTemplateStage

-- | Something on the left side of the match
type MatchLhs = Ann UMatchLhs IdDom SrcTemplateStage

-- | Right hand side of a value binding (possible with guards): (@ = 3 @ or @ | x == 1 = 3; | otherwise = 4 @)
type Rhs = Ann URhs IdDom SrcTemplateStage

-- | A guarded right-hand side of a value binding (@ | x > 3 = 2 @)
type GuardedRhs = Ann UGuardedRhs IdDom SrcTemplateStage

-- | Guards for value bindings and pattern matches (@ Just v <- x, v > 1 @)
type RhsGuard = Ann URhsGuard IdDom SrcTemplateStage

-- | Bindings that are enabled in local blocks (where or let).
type LocalBind = Ann ULocalBind IdDom SrcTemplateStage

-- | Local bindings attached to a declaration (@ where x = 42 @)
type LocalBinds = Ann ULocalBinds IdDom SrcTemplateStage

-- | A fixity signature (@ infixl 5 +, - @).
type FixitySignature = Ann UFixitySignature IdDom SrcTemplateStage

-- | A type signature (@ f :: Int -> Int @)
type TypeSignature = Ann UTypeSignature IdDom SrcTemplateStage

-- * Types

-- | Haskell types
type Type = Ann UType IdDom SrcTemplateStage

-- | Type variable declarations (with possible kind annotation)
type TyVar = Ann UTyVar IdDom SrcTemplateStage

-- One or more assertions
type Context = Ann UContext IdDom SrcTemplateStage

-- | A single assertion in the context
type Assertion = Ann UAssertion IdDom SrcTemplateStage

-- * Kinds

-- | Kind constraint (@ :: * -> * @)
type KindConstraint = Ann UKindConstraint IdDom SrcTemplateStage

-- | Haskell kinds
type Kind = Ann UKind IdDom SrcTemplateStage

-- | Values promoted to the kind level
type PromotedKind = Ann (UPromoted UKind) IdDom SrcTemplateStage

-- * Expressions

-- | Haskell expressions
type Expr = Ann UExpr IdDom SrcTemplateStage

-- | Clause of case expression (@ Just x -> x + 1 @)
type Alt = Ann UAlt IdDom SrcTemplateStage

-- | Right hand side of a match (possible with guards): (@ -> 3 @ or @ | x == 1 -> 3; | otherwise -> 4 @)
type CaseRhs = Ann UCaseRhs IdDom SrcTemplateStage

-- | A guarded right-hand side of pattern matches binding (@ | x > 3 -> 2 @)
type GuardedCaseRhs = Ann UGuardedCaseRhs IdDom SrcTemplateStage

-- | Field update expressions
type FieldUpdate = Ann UFieldUpdate IdDom SrcTemplateStage

-- | An element of a tuple section that can be an expression or missing (indicating a value from a parameter)
type TupSecElem = Ann UTupSecElem IdDom SrcTemplateStage

-- | Pragmas that can be applied to expressions
type ExprPragma = Ann UExprPragma IdDom SrcTemplateStage

-- | Special expressions for arrows
type Cmd = Ann UCmd IdDom SrcTemplateStage

-- | Clause of case expression for commands
type CmdAlt = Ann UCmdAlt IdDom SrcTemplateStage

-- | Arrow directions
type ArrowApp = Ann UArrowAppl IdDom SrcTemplateStage

-- * Statements

-- | A statement in a do-notation
type Stmt = Ann UStmt IdDom SrcTemplateStage

-- | Keywords @do@ or @mdo@ to start a do-block
type DoKind = Ann UDoKind IdDom SrcTemplateStage

-- | List comprehension statement
type CompStmt = Ann UCompStmt IdDom SrcTemplateStage

-- | Body of a list comprehension: (@ | x <- [1..10] @)
type ListCompBody = Ann UListCompBody IdDom SrcTemplateStage

-- | A do-notation for arrows
type CmdStmt = Ann UCmdStmt IdDom SrcTemplateStage

-- * Patterns

-- | Representation of patterns for pattern bindings
type Pattern = Ann UPattern IdDom SrcTemplateStage

-- Field specification of a record pattern
type PatternField = Ann UPatternField IdDom SrcTemplateStage

-- * Template Haskell

-- | A template haskell splice
type Splice = Ann USplice IdDom SrcTemplateStage

-- | Template Haskell bracket expressions
type Bracket = Ann UBracket IdDom SrcTemplateStage

-- | Template haskell quasi-quotation: @[quoter|str]@
type QuasiQuote = Ann UQuasiQuote IdDom SrcTemplateStage

-- * Literals

-- | Haskell literals
type Literal = Ann ULiteral IdDom SrcTemplateStage

-- * Names

-- | A definition that functions as an operator
type Operator = Ann UOperator IdDom SrcTemplateStage

-- | A definition that functions as a name
type Name = Ann UName IdDom SrcTemplateStage

-- | Possible qualified names. Contains also implicit names.
-- Linear implicit parameter: @%x@. Non-linear implicit parameter: @?x@.
type QualifiedName = Ann UQualifiedName IdDom SrcTemplateStage

-- | Parts of a qualified name.
type NamePart = Ann UNamePart IdDom SrcTemplateStage

-- | Program elements formatted as string literals (import packages, pragma texts)
type StringNode = Ann UStringNode IdDom SrcTemplateStage

-- * Optional AST elements

type MaybeContext = AnnMaybe UContext
type MaybeDeriving = AnnMaybe UDeriving
type MaybeDeriveStrategy = AnnMaybe UDeriveStrategy
type MaybeLocalBinds = AnnMaybe ULocalBinds
type MaybeTypeFamilySpec = AnnMaybe UTypeFamilySpec
type MaybeKindConstraint = AnnMaybe UKindConstraint
type MaybeClassBody = AnnMaybe UClassBody
type MaybeInstBody = AnnMaybe UInstBody
type MaybeExpr = AnnMaybe UExpr
type MaybeExportSpecs = AnnMaybe UExportSpecs
type MaybeImportQualified = AnnMaybe UImportQualified
type MaybeImportSource = AnnMaybe UImportSource
type MaybeImportSafe = AnnMaybe UImportSafe
type MaybeImportSpec = AnnMaybe UImportSpec
type MaybeModuleHead = AnnMaybe UModuleHead
type MaybeModulePragma = AnnMaybe UModulePragma
type MaybeSubSpec = AnnMaybe USubSpec
type MaybeStringNode = AnnMaybe UStringNode
type MaybeImportRenaming = AnnMaybe UImportRenaming
type MaybeSafety = AnnMaybe USafety
type MaybePhaseControl = AnnMaybe UPhaseControl
type MaybeConlikeAnnot = AnnMaybe UConlikeAnnot
type MaybeFunDeps = AnnMaybe UFunDeps


-- * AST elements with multiplicity

type MatchList = AnnList UMatch
type DeclList = AnnList UDecl
type PatternList = AnnList UPattern
type OperatorList = AnnList UOperator
type NameList = AnnList UName
type LocalBindList = AnnList ULocalBind
type IESpecList = AnnList UIESpec
type RhsGuardList = AnnList URhsGuard
type GuardedRhsList = AnnList UGuardedRhs
type GuardedCaseRhsList = AnnList UGuardedCaseRhs
type ConDeclList = AnnList UConDecl
type TypeEqnList = AnnList UTypeEqn
type TypeList = AnnList UType
type FieldDeclList = AnnList UFieldDecl
type ExprList = AnnList UExpr
type FieldUpdateList = AnnList UFieldUpdate
type GadtConDeclList = AnnList UGadtConDecl
type ClassElementList = AnnList UClassElement
type InstBodyDeclList = AnnList UInstBodyDecl
type InstanceHeadList = AnnList UInstanceHead
type AltList = AnnList UAlt
type StmtList = AnnList UStmt
type KindList = AnnList UKind
type TyVarList = AnnList UTyVar
type ListCompBodyList = AnnList UListCompBody
type ExportSpecList = AnnList UExportSpec
type FilePragmaList = AnnList UFilePragma
type ImportDeclList = AnnList UImportDecl
type PatternFieldList = AnnList UPatternField
type AssertionList = AnnList UAssertion
type CompStmtList = AnnList UCompStmt
type RuleList = AnnList URule
type RuleVarList = AnnList URuleVar
type RoleList = AnnList URole
type MinimalFormulaList = AnnList UMinimalFormula
type FunDepList = AnnList UFunDep
type TupSecElemList = AnnList UTupSecElem
type CmdList = AnnList UCmd
type CmdAltList = AnnList UCmdAlt
type CmdStmtList = AnnList UCmdStmt
type LanguageExtensionList = AnnList ULanguageExtension
type StringNodeList = AnnList UStringNode
type NamePartList = AnnList UNamePart
type DerivingList = AnnList UDeriving
