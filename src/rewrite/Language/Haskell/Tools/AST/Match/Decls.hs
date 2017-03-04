-- | UPattern matching on declaration-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Decls where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

-- WORKAROUND: nested pattern synonyms don't work in GHC 8.0, so I replaced them with longer but working pattern

-- * Declarations

-- | A type synonym ( @type String = [Char]@ )
pattern TypeDecl :: DeclHead dom -> Type dom -> Decl dom
pattern TypeDecl dh typ <- Ann _ (UTypeDecl dh typ)

-- | Standalone deriving declaration (@ deriving instance X T @)
pattern StandaloneDeriving :: InstanceRule dom -> Decl dom
pattern StandaloneDeriving instRule <- Ann _ (UDerivDecl _ instRule)

-- | Fixity declaration (@ infixl 5 +, - @)
pattern FixityDecl :: FixitySignature dom -> Decl dom
pattern FixityDecl fixity <- Ann _ (UFixityDecl fixity)

-- | Default types (@ default (T1, T2) @)
pattern DefaultDecl :: TypeList dom -> Decl dom
pattern DefaultDecl types <- Ann _ (UDefaultDecl types)

-- | Type signature declaration (@ f :: Int -> Int @)
pattern TypeSigDecl :: TypeSignature dom -> Decl dom
pattern TypeSigDecl typeSig <- Ann _ (UTypeSigDecl typeSig)

-- | Function or value binding (@ f x = 12 @)
pattern ValueBinding :: ValueBind dom -> Decl dom
pattern ValueBinding bind <- Ann _ (UValueBinding bind)

-- | A Template Haskell splice declaration (@ $(generateDecls) @)
pattern SpliceDecl :: Splice dom -> Decl dom
pattern SpliceDecl sp <- Ann _ (USpliceDecl sp)

-- * Data type definitions

-- | A data or newtype declaration. Empty data type declarations without
-- where keyword are always belong to DataDecl.
pattern DataDecl :: DataOrNewtypeKeyword dom -> MaybeContext dom -> DeclHead dom -> ConDeclList dom -> MaybeDeriving dom -> Decl dom
pattern DataDecl keyw ctx dh cons derivs <- Ann _ (UDataDecl keyw ctx dh cons derivs)

-- | A GADT-style data or newtype declaration.
pattern GADTDataDecl :: DataOrNewtypeKeyword dom -> MaybeContext dom -> DeclHead dom -> MaybeKindConstraint dome -> AnnListG UGadtConDecl dom stage -> MaybeDeriving dom -> Decl dom
pattern GADTDataDecl keyw ctx dh kind cons derivs  <- Ann _ (UGDataDecl keyw ctx dh kind cons derivs )

-- | GADT constructor declaration (@ D1 :: Int -> T String @)
pattern GadtConDecl :: NameList dom -> Type dom -> GadtConDecl dom
pattern GadtConDecl names typ <- Ann _ (UGadtConDecl names _ _ (Ann _ (UGadtNormalType typ)))

-- | GADT constructor declaration with record syntax (@ D1 :: { val :: Int } -> T String @)
pattern GadtRecordConDecl :: NameList dom -> FieldDeclList dom -> Type dom -> GadtConDecl dom
pattern GadtRecordConDecl names fields typ <- Ann _ (UGadtConDecl names _ _ (Ann _ (UGadtRecordType fields typ)))

-- | Ordinary data constructor (@ C t1 t2 @)
pattern ConDecl :: Name dom -> TypeList dom -> ConDecl dom
pattern ConDecl name args <- Ann _ (UConDecl _ _ name args)

-- | Creates a record data constructor (@ Point { x :: Double, y :: Double } @)
pattern RecordConDecl :: Name dom -> FieldDeclList dom -> ConDecl dom
pattern RecordConDecl name fields <- Ann _ (URecordDecl _ _ name fields)

-- | Infix data constructor (@ t1 :+: t2 @)
pattern InfixConDecl :: Type dom -> Operator dom -> Type dom -> ConDecl dom
pattern InfixConDecl lhs op rhs <- Ann _ (UInfixConDecl _ _ lhs op rhs)

-- | Field declaration (@ fld :: Int @)
pattern FieldDecl :: NameList dom -> Type dom -> FieldDecl dom
pattern FieldDecl names typ <- Ann _ (UFieldDecl names typ)

-- | A deriving clause without parentheses (@ deriving Show @.
pattern DerivingOne :: InstanceHead dom -> Deriving dom
pattern DerivingOne deriv <- Ann _ (UDerivingOne deriv)

-- | A deriving clause with parentheses @ deriving (Show, Eq) @)
pattern DerivingMulti :: InstanceHeadList dom -> Deriving dom
pattern DerivingMulti derivs <- Ann _ (UDerivings derivs)

pattern DataKeyword :: DataOrNewtypeKeyword dom
pattern DataKeyword <- Ann _ UDataKeyword

pattern NewtypeKeyword :: DataOrNewtypeKeyword dom
pattern NewtypeKeyword <- Ann _ UNewtypeKeyword

-- | A list of functional dependencies: @ | a -> b, c -> d @ separated by commas
pattern FunDeps :: FunDepList dom -> FunDeps dom
pattern FunDeps fds <- Ann _ (UFunDeps fds)

-- | A functional dependency, given on the form @l1 ... ln -> r1 ... rn@
pattern FunDep :: NameList dom -> NameList dom -> FunDep dom
pattern FunDep lhs rhs <- Ann _ (UFunDep lhs rhs)

-- * Type class declarations

-- | Type class declaration (@ class X a [where f = ...] @)
pattern ClassDecl :: MaybeContext dom -> DeclHead dom -> MaybeFunDeps dom -> MaybeClassBody dom -> Decl dom
pattern ClassDecl ctx dh fdeps body <- Ann _ (UClassDecl ctx dh fdeps body)

-- | The list of declarations that can appear in a typeclass
pattern ClassBody :: ClassElementList dom -> ClassBody dom
pattern ClassBody body <- Ann _ (UClassBody body)

-- | Type signature: @ f :: A -> B @ as a class member
pattern ClassElemSig :: TypeSignature dom -> ClassElement dom
pattern ClassElemSig typeSig <- Ann _ (UClsSig typeSig)

-- | Default binding: @ f x = "aaa" @ as a class member
pattern ClassElemDef :: ValueBind dom -> ClassElement dom
pattern ClassElemDef def <- Ann _ (UClsDef def)

-- | Declaration of an associated type synonym: @ type T x :: * @ in a class
pattern ClassElemTypeFam :: DeclHead dom -> MaybeTypeFamilySpec dom -> ClassElement dom
pattern ClassElemTypeFam dh tfSpec <- Ann _ (UClsTypeFam (Ann _ (UTypeFamily dh tfSpec)))

-- | Declaration of an associated data synonym: @ data T x :: * @ in a class
pattern ClassElemDataFam :: DeclHead dom -> MaybeKindConstraint dome -> ClassElement dom
pattern ClassElemDataFam dh kind <- Ann _ (UClsTypeFam (Ann _ (UDataFamily dh kind)))

-- | Default choice for type synonym: @ type T x = TE @ or @ type instance T x = TE @ in a class
pattern ClsDefaultType :: DeclHead dom -> Type dom -> ClassElement dom
pattern ClsDefaultType dh typ <- Ann _ (UClsTypeDef dh typ)

-- | Default signature (by using @DefaultSignatures@): @ default enum :: (Generic a, GEnum (Rep a)) => [a] @
pattern ClsDefaultSig :: Name dom -> Type dom -> ClassElement dom
pattern ClsDefaultSig name typ <- Ann _ (UClsDefSig name typ)

-- | Minimal pragma: @ {-\# MINIMAL (==) | (/=) \#-} @ in a class
pattern ClsMinimal :: MinimalFormula dom -> ClassElement dom
pattern ClsMinimal min <- Ann _ (UClsMinimal min)

pattern MinimalName :: Name dom -> MinimalFormula dom
pattern MinimalName name <- Ann _ (UMinimalName name)

pattern MinimalParen :: MinimalFormula dom -> MinimalFormula dom
pattern MinimalParen min <- Ann _ (UMinimalParen min)

-- | One of the minimal formulas are needed (@ min1 | min2 @)
pattern MinimalOr :: MinimalFormulaList dom -> MinimalFormula dom
pattern MinimalOr mins <- Ann _ (UMinimalOr mins)

-- | Both of the minimal formulas are needed (@ min1 , min2 @)
pattern MinimalAnd :: MinimalFormulaList dom -> MinimalFormula dom
pattern MinimalAnd mins <- Ann _ (UMinimalAnd mins)

-- * Declaration heads

-- | Type or class name as a declaration head
pattern NameDeclHead :: Name dom -> DeclHead dom
pattern NameDeclHead name <- Ann _ (UDeclHead name)

-- | Parenthesized type as a declaration head
pattern ParenDeclHead :: DeclHead dom -> DeclHead dom
pattern ParenDeclHead dh <- Ann _ (UDHParen dh)

-- | Type application as a declaration head
pattern DeclHeadApp :: DeclHead dom -> TyVar dom -> DeclHead dom
pattern DeclHeadApp dh tv <- Ann _ (UDHApp dh tv)

-- | Infix type application as a declaration head
pattern InfixDeclHead :: TyVar dom -> Operator dom -> TyVar dom -> DeclHead dom
pattern InfixDeclHead lhs op rhs <- Ann _ (UDHInfix lhs op rhs)

-- * Type class instance declarations

-- | Instance declaration (@ instance X T [where f = ...] @)
pattern InstanceDecl :: InstanceRule dom -> MaybeInstBody dom -> Decl dom
pattern InstanceDecl instRule body <- Ann _ (UInstDecl _ instRule body)

-- | Instance body is the implementation of the class functions (@ where a x = 1; b x = 2 @)
pattern InstanceBody :: InstBodyDeclList dom -> InstBody dom
pattern InstanceBody defs <- Ann _ (UInstBody defs)

-- | A normal value binding (@ f x = 12 @) inside a class instance
pattern InstanceBind :: ValueBind dom -> InstBodyDecl dom
pattern InstanceBind bind <- Ann _ (UInstBodyNormalDecl bind)

-- | Type signature in instance definition with @InstanceSigs@
pattern InstanceTypeSig :: TypeSignature dom -> InstBodyDecl dom
pattern InstanceTypeSig typeSig <- Ann _ (UInstBodyTypeSig typeSig)

-- | An associated type definition (@ type A X = B @) in a class instance
pattern InstanceTypeFamilyDef :: TypeEqn dom -> InstBodyDecl dom
pattern InstanceTypeFamilyDef typeEq <- Ann _ (UInstBodyTypeDecl typeEq)

-- | An associated data definition (@ data A X = B Int | C @) in a class instance
pattern InstanceDataFamilyDef :: DataOrNewtypeKeyword dom -> InstanceRule dom -> ConDeclList dom -> MaybeDeriving dom -> InstBodyDecl dom
pattern InstanceDataFamilyDef keyw instRule cons derivs  <- Ann _ (UInstBodyDataDecl keyw instRule cons derivs )

-- | An associated data definition as a GADT (@ data A X where B :: Int -> A X @) in a class instance
pattern InstanceDataFamilyGADTDef :: DataOrNewtypeKeyword dom -> InstanceRule dom -> MaybeKindConstraint dome -> AnnListG UGadtConDecl dom stage
                                       -> MaybeDeriving dom -> InstBodyDecl dom
pattern InstanceDataFamilyGADTDef keyw instRule kind cons derivs <- Ann _ (UInstBodyGadtDataDecl keyw instRule kind cons derivs)

-- | Specialize instance pragma in a class instance  (no phase selection is allowed)
pattern InstanceSpecializePragma :: Type dom -> InstBodyDecl dom
pattern InstanceSpecializePragma typ <- Ann _ (USpecializeInstance typ)

-- | Instance head as an instance rule (@ X a => Y a @)
pattern InstanceRule :: AnnMaybeG (AnnListG UTyVar) dom stage -> MaybeContext dom -> InstanceHead dom -> InstanceRule dom
pattern InstanceRule tvs ctx ih <- Ann _ (UInstanceRule tvs ctx ih)

-- | Type or class name as an instance head
pattern InstanceHead :: Name dom -> InstanceHead dom
pattern InstanceHead name <- Ann _ (UInstanceHeadCon name)

-- | Infix application of the type/class name to the left operand as an instance head
pattern InfixInstanceHead :: Type dom -> Operator dom -> InstanceHead dom
pattern InfixInstanceHead typ n <- Ann _ (UInstanceHeadInfix typ n)

-- | Parenthesized instance head
pattern ParenInstanceHead :: InstanceHead dom -> InstanceHead dom
pattern ParenInstanceHead ih <- Ann _ (UInstanceHeadParen ih)

-- | Type application as an instance head
pattern AppInstanceHead :: InstanceHead dom -> Type dom -> InstanceHead dom
pattern AppInstanceHead fun arg <- Ann _ (UInstanceHeadApp fun arg)

-- | @OVERLAP@ pragma
pattern EnableOverlap :: OverlapPragma dom
pattern EnableOverlap <- Ann _ UEnableOverlap

-- | @NO_OVERLAP@ pragma
pattern DisableOverlap :: OverlapPragma dom
pattern DisableOverlap <- Ann _ UDisableOverlap

-- | @OVERLAPPABLE@ pragma
pattern Overlappable :: OverlapPragma dom
pattern Overlappable <- Ann _ UOverlappable

-- | @OVERLAPPING@ pragma
pattern Overlapping :: OverlapPragma dom
pattern Overlapping <- Ann _ UOverlapping

-- | @OVERLAPS@ pragma
pattern Overlaps :: OverlapPragma dom
pattern Overlaps <- Ann _ UOverlaps

-- | @INCOHERENT@ pragma
pattern IncoherentOverlap :: OverlapPragma dom
pattern IncoherentOverlap <- Ann _ UIncoherentOverlap

-- * Type roles

-- | Role annotations (@ type role Ptr representational @)
pattern RoleDecl :: QualifiedName dom -> RoleList dom -> Decl dom
pattern RoleDecl name roles <- Ann _ (URoleDecl name roles)

pattern NominalRole :: Role dom
pattern NominalRole <- Ann _ UNominal

pattern RepresentationalRole :: Role dom
pattern RepresentationalRole <- Ann _ URepresentational

pattern PhantomRole :: Role dom
pattern PhantomRole <- Ann _ UPhantom

-- * Foreign imports and exports

-- | Foreign import (@ foreign import foo :: Int -> IO Int @)
pattern ForeignImport :: CallConv dom -> MaybeSafety dom -> Name dom -> Type dom -> Decl dom
pattern ForeignImport cc safety name typ <- Ann _ (UForeignImport cc safety name typ)

-- | Foreign export (@ foreign export ccall foo :: Int -> IO Int @)
pattern ForeignExport :: CallConv dom -> Name dom -> Type dom -> Decl dom
pattern ForeignExport cc name typ <- Ann _ (UForeignExport cc name typ)

-- | Specifies @stdcall@ calling convention for foreign import/export.
pattern StdCall :: CallConv dom
pattern StdCall <- Ann _ UStdCall

-- | Specifies @ccall@ calling convention for foreign import/export.
pattern CCall :: CallConv dom
pattern CCall <- Ann _ UCCall

-- | Specifies @capi@ calling convention for foreign import/export.
pattern CApi :: CallConv dom
pattern CApi <- Ann _ UCApi

-- | Specifies that the given foreign import is @unsafe@.
pattern Unsafe :: Safety dom
pattern Unsafe <- Ann _ UUnsafe

-- * Pattern synonyms

-- | Pattern synonyms (@ pattern Arrow t1 t2 = App \"->\" [t1, t2] @)
pattern PatternSynonym :: PatSynLhs dom -> PatSynRhs dom -> Decl dom
pattern PatternSynonym lhs rhs <- Ann _ (UPatternSynonymDecl (Ann _ (UPatternSynonym lhs rhs)))

-- | A left hand side with a constructor name and arguments (@ Arrow t1 t2 @)
pattern ConPatSyn :: Name dom -> NameList dom -> PatSynLhs dom
pattern ConPatSyn con args <- Ann _ (UNormalPatSyn con args)

-- | An infix pattern synonym left-hand side (@ t1 :+: t2 @)
pattern InfixPatSyn :: Name dom -> Operator dom -> Name dom -> PatSynLhs dom
pattern InfixPatSyn lhs op rhs <- Ann _ (UInfixPatSyn lhs op rhs)

-- | A record-style pattern synonym left-hand side (@ Arrow { arrowFrom, arrowTo } @)
pattern RecordPatSyn :: Name dom -> NameList dom -> PatSynLhs dom
pattern RecordPatSyn con args <- Ann _ (URecordPatSyn con args)

-- | An automatically two-way pattern synonym (@ = App \"Int\" [] @)
pattern SymmetricPatSyn :: Pattern dom -> PatSynRhs dom
pattern SymmetricPatSyn pat <- Ann _ (UBidirectionalPatSyn pat AnnNothing)

-- | A pattern synonym that can be only used for pattenr matching but not for combining (@ <- App \"Int\" [] @)
pattern OneWayPatSyn :: Pattern dom -> PatSynRhs dom
pattern OneWayPatSyn pat <- Ann _ (UOneDirectionalPatSyn pat)

-- | A pattern synonym with the other direction explicitely specified (@ <- App \"Int\" [] where Int = App \"Int\" [] @)
pattern TwoWayPatSyn :: Pattern dom -> MatchList dom -> PatSynRhs dom
pattern TwoWayPatSyn pat match <- Ann _ (UBidirectionalPatSyn pat (AnnJust (Ann _ (UPatSynWhere match))))

-- | Pattern type signature declaration (@ pattern Succ :: Int -> Int @)
pattern PatternSignatureDecl :: PatternSignature dom -> Decl dom
pattern PatternSignatureDecl patsig <- Ann _ (UPatTypeSigDecl patsig)

pattern PatternSignature :: Name dom -> Type dom -> PatternSignature dom
pattern PatternSignature name typ <- Ann _ (UPatternTypeSignature name typ)

-- * Type families

-- | Type family declaration (@ type family A a :: * -> * @)
pattern TypeFamily :: DeclHead dom -> MaybeTypeFamilySpec dom -> Decl dom
pattern TypeFamily dh famSpec <- Ann _ (UTypeFamilyDecl (Ann _ (UTypeFamily dh famSpec)))

-- | Data family declaration (@ data family A a :: * -> * @)
pattern DataFamily :: DeclHead dom -> MaybeKindConstraint dom -> Decl dom
pattern DataFamily dh kind <- Ann _ (UTypeFamilyDecl (Ann _ (UDataFamily dh kind)))

-- | Type family instance declaration (@ type instance Fam T = AssignedT @)
pattern TypeInstance :: InstanceRule dom -> Type dom -> Decl dom
pattern TypeInstance instRule typ <- Ann _ (UTypeInstDecl instRule typ)

-- | Data instance declaration (@ data instance Fam T = Con1 | Con2 @)
pattern DataInstance :: DataOrNewtypeKeyword dom -> InstanceRule dom -> ConDeclList dom -> MaybeDeriving dom -> Decl dom
pattern DataInstance keyw instRule cons derivs  <- Ann _ (UDataInstDecl keyw instRule cons derivs )

-- | GADT-style data instance declaration (@ data instance Fam T where ... @)
pattern GadtDataInstance :: DataOrNewtypeKeyword dom -> InstanceRule dom -> MaybeKindConstraint dome -> GadtConDeclList dom -> Decl dom
pattern GadtDataInstance keyw instRule kind cons  <- Ann _ (UGDataInstDecl keyw instRule kind cons )

-- | A closed type family declaration
pattern ClosedTypeFamily :: DeclHead dom -> MaybeKindConstraint dom -> TypeEqnList dom -> Decl dom
pattern ClosedTypeFamily dh kind typeqs <- Ann _ (UClosedTypeFamilyDecl dh kind typeqs)

-- | Specifies the kind of a type family (@ :: * -> * @)
pattern TypeFamilyKindSpec :: KindConstraint dom -> TypeFamilySpec dom
pattern TypeFamilyKindSpec kind <- Ann _ (UTypeFamilyKind kind)

-- | Specifies the injectivity of a type family (@ = r | r -> a @)
pattern TypeFamilyInjectivitySpec :: Name dom -> NameList dom -> TypeFamilySpec dom
pattern TypeFamilyInjectivitySpec res dependent <- Ann _ (UTypeFamilyInjectivity (Ann _ (UInjectivityAnn res dependent)))

-- | Type equations as found in closed type families (@ T A = S @)
pattern TypeEqn :: Type dom -> Type dom -> TypeEqn dom
pattern TypeEqn lhs rhs <- Ann _ (UTypeEqn lhs rhs)

-- * Top level pragmas

-- | Top-level pragmas
pattern PragmaDecl :: TopLevelPragma dom -> Decl dom
pattern PragmaDecl pragma <- Ann _ (UPragmaDecl pragma)

-- | A pragma that introduces source rewrite rules (@ {-\# RULES "map/map" [2]  forall f g xs. map f (map g xs) = map (f.g) xs \#-} @)
pattern RulePragma :: RuleList dom -> TopLevelPragma dom
pattern RulePragma rules <- Ann _ (URulePragma rules)

-- | A pragma that marks definitions as deprecated (@ {-\# DEPRECATED f "f will be replaced by g" \#-} @)
pattern DeprPragma :: NameList dom -> String -> TopLevelPragma dom
pattern DeprPragma defs msg <- Ann _ (UDeprPragma defs (AnnList [Ann _ (UStringNode msg)]))

-- | A pragma that marks definitions as deprecated (@ {-\# WARNING unsafePerformIO "you should know what you are doing" \#-} @)
pattern WarningPragma :: NameList dom -> String -> TopLevelPragma dom
pattern WarningPragma defs msg <- Ann _ (UWarningPragma defs (AnnList [Ann _ (UStringNode msg)]))

-- | A pragma that annotates a definition with an arbitrary value (@ {-\# ANN f 42 \#-} @)
pattern AnnPragma :: AnnotationSubject dom -> Expr dom -> TopLevelPragma dom
pattern AnnPragma subj ann <- Ann _ (UAnnPragma subj ann)

-- | A pragma that marks a function for inlining to the compiler (@ {-\# INLINE thenUs \#-} @)
pattern InlinePragma :: MaybeConlikeAnnot dom -> MaybePhaseControl dom -> Name dom -> TopLevelPragma dom
pattern InlinePragma conlike phase name <- Ann _ (UInlinePragmaDecl (Ann _ (UInlinePragma conlike phase name)))

-- | A pragma that forbids a function from being inlined by the compiler (@ {-\# NOINLINE f \#-} @)
pattern NoInlinePragma :: Name dom -> TopLevelPragma dom
pattern NoInlinePragma name <- Ann _ (UInlinePragmaDecl (Ann _ (UNoInlinePragma name)))

-- | A pragma that marks a function that it may be inlined by the compiler (@ {-\# INLINABLE thenUs \#-} @)
pattern InlinablePragma :: MaybePhaseControl dom -> Name dom -> TopLevelPragma dom
pattern InlinablePragma phase name <- Ann _ (UInlinePragmaDecl (Ann _ (UInlinablePragma phase name)))

-- | A pragma for maintaining line numbers in generated sources (@ {-\# LINE 123 \"somefile\" \#-} @)
pattern LinePragma :: Int -> MaybeStringNode dom -> TopLevelPragma dom
pattern LinePragma line filename <- Ann _ (ULinePragma (Ann _ (LineNumber line)) filename)

-- | A pragma that tells the compiler that a polymorph function should be optimized for a given type (@ {-\# SPECIALISE f :: Int -> b -> b \#-} @)
pattern SpecializePragma :: MaybePhaseControl dom -> Name dom -> TypeList dom -> TopLevelPragma dom
pattern SpecializePragma phase def specTypes <- Ann _ (USpecializeDecl (Ann _ (USpecializePragma phase def specTypes)))

-- | Marks that the pragma should be applied from a given compile phase (@ [2] @)
pattern PhaseControlFrom :: Integer -> PhaseControl dom
pattern PhaseControlFrom phaseNum <- Ann _ (UPhaseControl AnnNothing (AnnJust (Ann _ (PhaseNumber phaseNum))))

-- | Marks that the pragma should be applied until a given compile phase (@ [~2] @)
pattern PhaseControlUntil :: Integer -> PhaseControl dom
pattern PhaseControlUntil phaseNum <- Ann _ (UPhaseControl (AnnJust _) (AnnJust (Ann _ (PhaseNumber phaseNum))))

-- | A rewrite rule (@ "map/map" forall f g xs. map f (map g xs) = map (f.g) xs @)
pattern RewriteRule :: String -> MaybePhaseControl dom -> RuleVarList dom -> Expr dom -> Expr dom -> Rule dom
pattern RewriteRule name phase vars lhs rhs <- Ann _ (URule (Ann _ (UStringNode name)) phase vars lhs rhs)

-- | The definition with the given name is annotated
pattern NameAnnotation :: Name dom -> AnnotationSubject dom
pattern NameAnnotation name <- Ann _ (UNameAnnotation name)

-- | A type with the given name is annotated
pattern TypeAnnotation :: Name dom -> AnnotationSubject dom
pattern TypeAnnotation name <- Ann _ (UTypeAnnotation name)

-- | The whole module is annotated
pattern ModuleAnnotation :: AnnotationSubject dom
pattern ModuleAnnotation <- Ann _ UModuleAnnotation

-- | A @CONLIKE@ modifier for an @INLINE@ pragma.
pattern ConlikeAnnotation :: ConlikeAnnot dom
pattern ConlikeAnnotation <- Ann _ UConlikeAnnot
