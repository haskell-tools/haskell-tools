module Language.Haskell.Tools.AST.ElementTypes where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate

type AnnList node dom = AnnListG node dom SrcTemplateStage 
type AnnMaybe node dom = AnnMaybeG node dom SrcTemplateStage 

-- * Modules

-- | The representation of a haskell module, that is a separate compilation unit.
-- It may or may not have a header.
type Module dom = Ann UModule dom SrcTemplateStage

-- | Module declaration with name and (optional) exports
type ModuleHead dom = Ann UModuleHead dom SrcTemplateStage

-- | A list of export specifications surrounded by parentheses
type ExportSpecs dom = Ann UExportSpecs dom SrcTemplateStage

-- | Export specifier
type ExportSpec dom = Ann UExportSpec dom SrcTemplateStage

-- | Marks a name to be imported or exported with related names (subspecifier)
type IESpec dom = Ann UIESpec dom SrcTemplateStage

-- | Marks how related names will be imported or exported with a given name
type SubSpec dom = Ann USubSpec dom SrcTemplateStage

-- | Pragmas that must be used after the module head  
type ModulePragma dom = Ann UModulePragma dom SrcTemplateStage

-- | Pragmas that must be used before defining the module         
type FilePragma dom = Ann UFilePragma dom SrcTemplateStage

-- | An import declaration: @import Module.Name@         
type ImportDecl dom = Ann UImportDecl dom SrcTemplateStage

-- | Restriction on the imported names
type ImportSpec dom = Ann UImportSpec dom SrcTemplateStage

-- | Marks the import as qualified: @qualified@
type ImportQualified dom = Ann UImportQualified dom SrcTemplateStage

-- | Marks the import as source: @{-# SOURCE #-}@
type ImportSource dom = Ann UImportSource dom SrcTemplateStage

-- | Marks the import as safe: @safe@
type ImportSafe dom = Ann UImportSafe dom SrcTemplateStage

-- | Marks an imported name to belong to the type namespace: @type@
type TypeNamespace dom = Ann UTypeNamespace dom SrcTemplateStage

-- | Renaming imports (@ as A @)
type ImportRenaming dom = Ann UImportRenaming dom SrcTemplateStage

-- * Declarations

-- | Haskell declaration
type Decl dom = Ann UDecl dom SrcTemplateStage

-- | The list of declarations that can appear in a typeclass
type ClassBody dom = Ann UClassBody dom SrcTemplateStage

-- | Members of a class declaration 
type ClassElement dom = Ann UClassElement dom SrcTemplateStage

-- The declared (possibly parameterized) type (@ A x :+: B y @).
type DeclHead dom = Ann UDeclHead dom SrcTemplateStage

-- | Instance body is the implementation of the class functions (@ where a x = 1; b x = 2 @)
type InstBody dom = Ann UInstBody dom SrcTemplateStage

-- | Declarations inside an instance declaration.
type InstBodyDecl dom = Ann UInstBodyDecl dom SrcTemplateStage

-- | GADT constructor declaration (@ D1 :: { val :: Int } -> T String @)
type GadtConDecl dom = Ann UGadtConDecl dom SrcTemplateStage

-- | Type of GADT constructors (can be record types: @{ val :: Int }@)
type GadtConType dom = Ann UGadtConType dom SrcTemplateStage

-- | Marker for a field wildcard. Only needed to attach semantic information in a type-safe way.
type FieldWildcard dom = Ann UFieldWildcard dom SrcTemplateStage

-- | A list of functional dependencies: @ | a -> b, c -> d @ separated by commas  
type FunDeps dom = Ann UFunDeps dom SrcTemplateStage

-- | A functional dependency, given on the form @l1 ... ln -> r1 ... rn@         
type FunDep dom = Ann UFunDep dom SrcTemplateStage

-- | A constructor declaration for a datatype
type ConDecl dom = Ann UConDecl dom SrcTemplateStage

-- | Field declaration (@ fld :: Int @)
type FieldDecl dom = Ann UFieldDecl dom SrcTemplateStage

-- | A deriving clause following a data type declaration. (@ deriving Show @ or @ deriving (Show, Eq) @)
type Deriving dom = Ann UDeriving dom SrcTemplateStage

-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the where keyword.
type InstanceRule dom = Ann UInstanceRule dom SrcTemplateStage

-- | The specification of the class instance declaration
type InstanceHead dom = Ann UInstanceHead dom SrcTemplateStage

-- | Overlap pragmas. Can be applied to class declarations and class instance declarations.    
type OverlapPragma dom = Ann UOverlapPragma dom SrcTemplateStage

-- | Type equations as found in closed type families (@ T A = S @)
type TypeEqn dom = Ann UTypeEqn dom SrcTemplateStage

-- | Kind constraint (@ :: * -> * @)
type KindConstraint dom = Ann UKindConstraint dom SrcTemplateStage

-- | Haskell types
type Type dom = Ann UType dom SrcTemplateStage

-- | Type variable declarations (with possible kind annotation)
type TyVar dom = Ann UTyVar dom SrcTemplateStage

-- | Haskell kinds
type Kind dom = Ann UKind dom SrcTemplateStage

-- One or more assertions
type Context dom = Ann UContext dom SrcTemplateStage

-- | A single assertion in the context
type Assertion dom = Ann UAssertion dom SrcTemplateStage

-- | Haskell expressions
type Expr dom = Ann UExpr dom SrcTemplateStage

-- | List comprehension statement
type CompStmt dom = Ann UCompStmt dom SrcTemplateStage

-- | Value binding for top-level and local bindings
type ValueBind dom = Ann UValueBind dom SrcTemplateStage

-- | Representation of patterns for pattern bindings
type Pattern dom = Ann UPattern dom SrcTemplateStage

-- Field specification of a record pattern
type PatternField dom = Ann UPatternField dom SrcTemplateStage

-- | A template haskell splice          
type Splice dom = Ann USplice dom SrcTemplateStage

-- type QString dom = Ann QQString dom SrcTemplateStage

-- | Clause of function binding   
type Match dom = Ann UMatch dom SrcTemplateStage

-- | Right hand side of a value binding (possible with guards): (@ = 3 @ or @ | x == 1 = 3; | otherwise = 4 @)
type Rhs dom = Ann URhs dom SrcTemplateStage

-- | A guarded right-hand side of a value binding (@ | x > 3 = 2 @)      
type GuardedRhs dom = Ann UGuardedRhs dom SrcTemplateStage

-- | Field update expressions
type FieldUpdate dom = Ann UFieldUpdate dom SrcTemplateStage

-- | Template Haskell bracket expressions
type Bracket dom = Ann UBracket dom SrcTemplateStage

-- | Top level pragmas
type TopLevelPragma dom = Ann UTopLevelPragma dom SrcTemplateStage

-- | A rewrite rule (@ "map/map" forall f g xs. map f (map g xs) = map (f.g) xs @)
type Rule dom = Ann URule dom SrcTemplateStage

-- | Annotation allows you to connect an expression to any declaration. 
type AnnotationSubject dom = Ann UAnnotationSubject dom SrcTemplateStage

-- | Formulas of minimal annotations declaring which functions should be defined.
type MinimalFormula dom = Ann UMinimalFormula dom SrcTemplateStage

-- | Pragmas that can be applied to expressions
type ExprPragma dom = Ann UExprPragma dom SrcTemplateStage

-- | In-AST source ranges (for generated pragmas)
type SourceRange dom = Ann USourceRange dom SrcTemplateStage

-- | Template haskell quasi-quotation: @[quoter|str]@  
type QuasiQuote dom = Ann UQuasiQuote dom SrcTemplateStage

-- | Guards for value bindings and pattern matches (@ Just v <- x, v > 1 @)
type RhsGuard dom = Ann URhsGuard dom SrcTemplateStage

-- | Bindings that are enabled in local blocks (where or let).
type LocalBind dom = Ann ULocalBind dom SrcTemplateStage

-- | Local bindings attached to a declaration (@ where x = 42 @)             
type LocalBinds dom = Ann ULocalBinds dom SrcTemplateStage

-- | A fixity signature (@ infixl 5 +, - @).
type FixitySignature dom = Ann UFixitySignature dom SrcTemplateStage

-- | A type signature (@ f :: Int -> Int @)
type TypeSignature dom = Ann UTypeSignature dom SrcTemplateStage

-- | Body of a list comprehension: (@ | x <- [1..10] @)
type ListCompBody dom = Ann UListCompBody dom SrcTemplateStage

-- | An element of a tuple section that can be an expression or missing (indicating a value from a parameter)
type TupSecElem dom = Ann UTupSecElem dom SrcTemplateStage

-- | Open type and data families
type TypeFamily dom = Ann UTypeFamily dom SrcTemplateStage

-- | Type family specification with kinds specification and injectivity.
type TypeFamilySpec dom = Ann UTypeFamilySpec dom SrcTemplateStage

-- | Injectivity annotation for type families (@ = r | r -> a @)
type InjectivityAnn dom = Ann UInjectivityAnn dom SrcTemplateStage

-- | Pattern synonyms: @ pattern Arrow t1 t2 = App "->" [t1, t2] @
type PatternSynonym dom = Ann UPatternSynonym dom SrcTemplateStage

-- | Right-hand side of pattern synonym
type PatSynRhs dom = Ann UPatSynRhs dom SrcTemplateStage

-- | Left hand side of a pattern synonym
type PatSynLhs dom = Ann UPatSynLhs dom SrcTemplateStage

-- | Where clause of pattern synonym (explicit expression direction)
type PatSynWhere dom = Ann UPatSynWhere dom SrcTemplateStage

-- | Pattern type signature declaration (@ pattern Succ :: Int -> Int @)
type PatternSignature dom = Ann UPatternTypeSignature dom SrcTemplateStage

-- | Role annotations for types
type Role dom = Ann URole dom SrcTemplateStage

-- | Special expressions for arrows
type Cmd dom = Ann UCmd dom SrcTemplateStage

-- | Clause of case expression for commands
type CmdAlt dom = Ann UCmdAlt dom SrcTemplateStage

-- | A do-notation for arrows
type CmdStmt dom = Ann UCmdStmt dom SrcTemplateStage

-- | The name of the enabled language extension, for example (@ LambdaCase @)
type LanguageExtension dom = Ann ULanguageExtension dom SrcTemplateStage

-- | Something on the left side of the match
type MatchLhs dom = Ann UMatchLhs dom SrcTemplateStage

-- | A statement in a do-notation
type Stmt dom = Ann UStmt dom SrcTemplateStage

-- | Clause of case expression (@ Just x -> x + 1 @)
type Alt dom = Ann UAlt dom SrcTemplateStage

-- | Right hand side of a match (possible with guards): (@ -> 3 @ or @ | x == 1 -> 3; | otherwise -> 4 @)
type CaseRhs dom = Ann UCaseRhs dom SrcTemplateStage

-- | A guarded right-hand side of pattern matches binding (@ | x > 3 -> 2 @)      
type GuardedCaseRhs dom = Ann UGuardedCaseRhs dom SrcTemplateStage

-- * Literals

-- | Haskell literals
type Literal dom = Ann ULiteral dom SrcTemplateStage

-- | Values promoted to the kind level
type PromotedKind dom = Ann (UPromoted UKind) dom SrcTemplateStage

-- * Names

-- | A definition that functions as an operator
type Operator dom = Ann UOperator dom SrcTemplateStage

-- | A definition that functions as a name
type Name dom = Ann UName dom SrcTemplateStage

-- | Possible qualified names. Contains also implicit names.
-- Linear implicit parameter: @%x@. Non-linear implicit parameter: @?x@.
type QualifiedName dom = Ann UQualifiedName dom SrcTemplateStage

-- | The name of a module
type ModuleName dom = Ann UModuleName dom SrcTemplateStage

-- | Parts of a qualified name.         
type NamePart dom = Ann UNamePart dom SrcTemplateStage

-- | Program elements formatted as string literals (import packages, pragma texts)
type StringNode dom = Ann UStringNode dom SrcTemplateStage

-- | The @data@ or the @newtype@ keyword to define ADTs.
type DataOrNewtypeKeyword dom = Ann UDataOrNewtypeKeyword dom SrcTemplateStage

-- | Keywords @do@ or @mdo@ to start a do-block
type DoKind dom = Ann UDoKind dom SrcTemplateStage

-- | Call conventions of foreign functions
type CallConv dom = Ann UCallConv dom SrcTemplateStage

-- | Arrow directions
type ArrowApp dom = Ann UArrowAppl dom SrcTemplateStage

-- | Safety annotations for foreign calls
type Safety dom = Ann USafety dom SrcTemplateStage

-- | A @CONLIKE@ modifier for an @INLINE@ pragma.
type ConlikeAnnot dom = Ann UConlikeAnnot dom SrcTemplateStage

-- | Controls the activation of a rewrite rule (@ [1] @)
type PhaseControl dom = Ann UPhaseControl dom SrcTemplateStage

-- * Optional AST elements

type MaybeContext dom = AnnMaybe UContext dom
type MaybeDeriving dom = AnnMaybe UDeriving dom
type MaybeLocalBinds dom = AnnMaybe ULocalBinds dom
type MaybeTypeFamilySpec dom = AnnMaybe UTypeFamilySpec dom
type MaybeKindConstraint dom = AnnMaybe UKindConstraint dom
type MaybeClassBody dom = AnnMaybe UClassBody dom
type MaybeInstBody dom = AnnMaybe UInstBody dom
type MaybeExpr dom = AnnMaybe UExpr dom
type MaybeExportSpecs dom = AnnMaybe UExportSpecs dom
type MaybeImportQualified dom = AnnMaybe UImportQualified dom
type MaybeImportSource dom = AnnMaybe UImportSource dom
type MaybeImportSafe dom = AnnMaybe UImportSafe dom
type MaybeImportSpec dom = AnnMaybe UImportSpec dom
type MaybeModuleHead dom = AnnMaybe UModuleHead dom
type MaybeModulePragma dom = AnnMaybe UModulePragma dom
type MaybeSubSpec dom = AnnMaybe USubSpec dom
type MaybeStringNode dom = AnnMaybe UStringNode dom
type MaybeImportRenaming dom = AnnMaybe UImportRenaming dom
type MaybeSafety dom = AnnMaybe USafety dom
type MaybePhaseControl dom = AnnMaybe UPhaseControl dom
type MaybeConlikeAnnot dom = AnnMaybe UConlikeAnnot dom
type MaybeFunDeps dom = AnnMaybe UFunDeps dom


-- * AST elements with multiplicity

type MatchList dom = AnnList UMatch dom
type DeclList dom = AnnList UDecl dom
type PatternList dom = AnnList UPattern dom
type OperatorList dom = AnnList UOperator dom
type NameList dom = AnnList UName dom
type LocalBindList dom = AnnList ULocalBind dom
type IESpecList dom = AnnList UIESpec dom
type RhsGuardList dom = AnnList URhsGuard dom
type GuardedRhsList dom = AnnList UGuardedRhs dom
type GuardedCaseRhsList dom = AnnList UGuardedCaseRhs dom
type ConDeclList dom = AnnList UConDecl dom
type TypeEqnList dom = AnnList UTypeEqn dom
type TypeList dom = AnnList UType dom
type FieldDeclList dom = AnnList UFieldDecl dom
type ExprList dom = AnnList UExpr dom
type FieldUpdateList dom = AnnList UFieldUpdate dom
type GadtConDeclList dom = AnnList UGadtConDecl dom
type ClassElementList dom = AnnList UClassElement dom
type InstBodyDeclList dom = AnnList UInstBodyDecl dom
type InstanceHeadList dom = AnnList UInstanceHead dom
type AltList dom = AnnList UAlt dom
type StmtList dom = AnnList UStmt dom
type KindList dom = AnnList UKind dom
type TyVarList dom = AnnList UTyVar dom
type ListCompBodyList dom = AnnList UListCompBody dom
type ExportSpecList dom = AnnList UExportSpec dom
type FilePragmaList dom = AnnList UFilePragma dom
type ImportDeclList dom = AnnList UImportDecl dom
type PatternFieldList dom = AnnList UPatternField dom
type AssertionList dom = AnnList UAssertion dom
type CompStmtList dom = AnnList UCompStmt dom
type RuleList dom = AnnList URule dom
type RoleList dom = AnnList URole dom
type MinimalFormulaList dom = AnnList UMinimalFormula dom
type FunDepList dom = AnnList UFunDep dom
type TupSecElemList dom = AnnList UTupSecElem dom
type CmdList dom = AnnList UCmd dom
type CmdAltList dom = AnnList UCmdAlt dom
type CmdStmtList dom = AnnList UCmdStmt dom
type LanguageExtensionList dom = AnnList ULanguageExtension dom
type StringNodeList dom = AnnList UStringNode dom
type NamePartList dom = AnnList UNamePart dom
