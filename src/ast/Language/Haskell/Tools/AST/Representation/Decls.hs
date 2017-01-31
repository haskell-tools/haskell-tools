-- | Representation of Haskell AST definitions. These include definition of data types, classes, instances and so on. 
-- The definition of value bindings are in the Binds module.
module Language.Haskell.Tools.AST.Representation.Decls where

import Language.Haskell.Tools.AST.Ann (Ann, AnnListG, AnnMaybeG)
import Language.Haskell.Tools.AST.Representation.Binds
import Language.Haskell.Tools.AST.Representation.Exprs (UExpr)
import Language.Haskell.Tools.AST.Representation.Kinds (UKindConstraint)
import Language.Haskell.Tools.AST.Representation.Names
import Language.Haskell.Tools.AST.Representation.Patterns (UPattern)
import {-# SOURCE #-} Language.Haskell.Tools.AST.Representation.TH (USplice)
import Language.Haskell.Tools.AST.Representation.Types (UContext, UType, UTyVar)

-- * Declarations

-- | Haskell declarationw
data UDecl dom stage
  = UTypeDecl             { _declHead :: Ann UDeclHead dom stage
                          , _declType :: Ann UType dom stage
                          } -- ^ A type synonym ( @type String = [Char]@ )
  | UTypeFamilyDecl       { _declTypeFamily :: Ann UTypeFamily dom stage
                          } -- ^ A type family declaration ( @type family F x@ )
  | UClosedTypeFamilyDecl { _declHead :: Ann UDeclHead dom stage
                          , _declKind :: AnnMaybeG UKindConstraint dom stage
                          , _declDecl :: AnnListG UTypeEqn dom stage -- ^ cannot be empty
                          } -- ^ A closed type family declaration
  | UDataDecl             { _declNewtype :: Ann UDataOrNewtypeKeyword dom stage
                          , _declCtx  :: AnnMaybeG UContext dom stage
                          , _declHead :: Ann UDeclHead dom stage
                          , _declCons :: AnnListG UConDecl dom stage
                          , _declDeriving :: AnnMaybeG UDeriving dom stage
                          } -- ^ A data or newtype declaration. Empty data type declarations without 
                            -- where keyword are always belong to DataDecl.
  | UGDataDecl            { _declNewtype :: Ann UDataOrNewtypeKeyword dom stage
                          , _declCtx  :: AnnMaybeG UContext dom stage
                          , _declHead :: Ann UDeclHead dom stage
                          , _declKind :: AnnMaybeG UKindConstraint dom stage
                          , _declGadt :: AnnListG UGadtConDecl dom stage
                          , _declDeriving :: AnnMaybeG UDeriving dom stage
                          } -- ^ A GADT-style data or newtype declaration.
  | UTypeInstDecl         { _declInstance :: Ann UInstanceRule dom stage
                          , _declAssignedType :: Ann UType dom stage
                          } -- ^ Type family instance declaration (@ type instance Fam T = AssignedT @)
  | UDataInstDecl         { _declNewtype :: Ann UDataOrNewtypeKeyword dom stage
                          , _declInstance :: Ann UInstanceRule dom stage
                          , _declCons :: AnnListG UConDecl dom stage
                          , _declDeriving :: AnnMaybeG UDeriving dom stage
                          } -- ^ Data instance declaration (@ data instance Fam T = Con1 | Con2 @)
  | UGDataInstDecl        { _declNewtype :: Ann UDataOrNewtypeKeyword dom stage
                          , _declInstance :: Ann UInstanceRule dom stage
                          , _declKind :: AnnMaybeG UKindConstraint dom stage
                          , _declGadt :: AnnListG UGadtConDecl dom stage
                          } -- ^ GADT-style data instance declaration (@ data instance Fam T where ... @)
  | UClassDecl            { _declCtx :: AnnMaybeG UContext dom stage
                          , _declHead :: Ann UDeclHead dom stage
                          , _declFunDeps :: AnnMaybeG UFunDeps dom stage
                          , _declBody :: AnnMaybeG UClassBody dom stage
                          } -- ^ Type class declaration (@ class X a [where f = ...] @)
  | UInstDecl             { _declOverlap :: AnnMaybeG UOverlapPragma dom stage
                          , _declInstRule :: Ann UInstanceRule dom stage
                          , _declInstDecl :: AnnMaybeG UInstBody dom stage
                          } -- ^ Instance declaration (@ instance X T [where f = ...] @)
  | UPatternSynonymDecl   { _declPatSyn :: Ann UPatternSynonym dom stage
                          } -- ^ Pattern synonyms (@ pattern Arrow t1 t2 = App "->" [t1, t2] @)
  | UDerivDecl            { _declOverlap :: AnnMaybeG UOverlapPragma dom stage
                          , _declInstRule :: Ann UInstanceRule dom stage
                          } -- ^ Standalone deriving declaration (@ deriving instance X T @)
  | UFixityDecl           { _declFixity :: Ann UFixitySignature dom stage
                          } -- ^ Fixity declaration (@ infixl 5 +, - @)
  | UDefaultDecl          { _declTypes :: AnnListG UType dom stage
                          } -- ^ Default types (@ default (T1, T2) @)
  | UTypeSigDecl          { _declTypeSig :: Ann UTypeSignature dom stage
                          } -- ^ Type signature declaration (@ f :: Int -> Int @)
  | UPatTypeSigDecl       { _declPatTypeSig :: Ann UPatternTypeSignature dom stage
                          } -- ^ Pattern type signature declaration (@ pattern Succ :: Int -> Int @)
  | UValueBinding         { _declValBind :: Ann UValueBind dom stage
                          } -- ^ Function or value binding (@ f x = 12 @)
  | UForeignImport        { _declCallConv :: Ann UCallConv dom stage
                          , _declSafety :: AnnMaybeG USafety dom stage
                          , _declName :: Ann UName dom stage
                          , _declType :: Ann UType dom stage
                          } -- ^ Foreign import (@ foreign import _foo :: Int -> IO Int @)
  | UForeignExport        { _declCallConv :: Ann UCallConv dom stage
                          , _declName :: Ann UName dom stage
                          , _declType :: Ann UType dom stage
                          } -- ^ Foreign export (@ foreign export ccall _foo :: Int -> IO Int @)
  | UPragmaDecl           { _declPragma :: Ann UTopLevelPragma dom stage
                          } -- ^ Top-level pragmas
  | URoleDecl             { _declRoleType :: Ann UQualifiedName dom stage
                          , _declRoles :: AnnListG URole dom stage
                          } -- ^ Role annotations (@ type role Ptr representational @)
  | USpliceDecl           { _declSplice :: Ann USplice dom stage
                          } -- ^ A Template Haskell splice declaration (@ $(generateDecls) @)

-- The declared (possibly parameterized) type (@ A x :+: B y @).
data UDeclHead dom stage
  = UDeclHead { _dhName :: Ann UName dom stage
              } -- ^ Type or class name
  | UDHParen  { _dhBody :: Ann UDeclHead dom stage
              } -- ^ Parenthesized type
  | UDHApp    { _dhAppFun :: Ann UDeclHead dom stage
              , _dhAppOperand :: Ann UTyVar dom stage
              } -- ^ Type application
  | UDHInfix  { _dhLeft :: Ann UTyVar dom stage
              , _dhOperator :: Ann UOperator dom stage
              , _dhRight :: Ann UTyVar dom stage
              } -- ^ Infix application of the type/class name to the left operand

-- * Type class definitions

-- | The list of declarations that can appear in a typeclass
data UClassBody dom stage
  = UClassBody { _cbElements :: AnnListG UClassElement dom stage
               }
                 
-- | Members of a class declaration       
data UClassElement dom stage
  = UClsSig     { _ceTypeSig :: Ann UTypeSignature dom stage
                } -- ^ Signature: @ f :: A -> B @
  | UClsDef     { _ceBind :: Ann UValueBind dom stage
                } -- ^ Default binding: @ f x = "aaa" @
  | UClsTypeFam { _ceTypeFam :: Ann UTypeFamily dom stage
                } -- ^ Declaration of an associated type synonym: @ type T x :: * @ 
  | UClsTypeDef { _ceHead :: Ann UDeclHead dom stage
                , _ceKind :: Ann UType dom stage
                } -- ^ Default choice for type synonym: @ type T x = TE @ or @ type instance T x = TE @ 
  | UClsDefSig  { _ceName :: Ann UName dom stage
                , _ceType :: Ann UType dom stage
                } -- ^ Default signature (by using @DefaultSignatures@): @ default _enum :: (Generic a, GEnum (Rep a)) => [a] @
  | UClsMinimal { _pragmaFormula :: Ann UMinimalFormula dom stage
                } -- ^ Minimal pragma: @ {-# MINIMAL (==) | (/=) #-} @
  | UClsInline  { _clsInline :: Ann UInlinePragma dom stage
                } -- ^ Inline-like pragma in class definition
  -- not supported yet (GHC 8.0.1)
-- | UClsPatSig  { _cePatSig :: Ann UPatternTypeSignature dom stage
  --               } -- ^ Pattern signature in a class declaration (by using @PatternSynonyms@)

-- * Type class instances
  
-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the where keyword.
data UInstanceRule dom stage
  = UInstanceRule  { _irVars :: AnnMaybeG (AnnListG UTyVar) dom stage
                   , _irCtx :: AnnMaybeG UContext dom stage
                   , _irHead :: Ann UInstanceHead dom stage
                   } -- ^ Instance head as an instance rule (@ X a => Y a @)

-- | The specification of the class instance declaration
data UInstanceHead dom stage
  = UInstanceHeadCon   { _ihConName :: Ann UName dom stage
                       } -- ^ Type or class name
  | UInstanceHeadInfix { _ihLeftOp :: Ann UType dom stage
                       , _ihOperator :: Ann UName dom stage
                       } -- ^ Infix application of the type/class name to the left operand
  | UInstanceHeadParen { _ihHead :: Ann UInstanceHead dom stage
                       } -- ^ Parenthesized instance head
  | UInstanceHeadApp   { _ihFun :: Ann UInstanceHead dom stage
                       , _ihType :: Ann UType dom stage
                       } -- ^ Application to one more type

-- | Instance body is the implementation of the class functions (@ where a x = 1; b x = 2 @)
data UInstBody dom stage
  = UInstBody { _instBodyDecls :: AnnListG UInstBodyDecl dom stage
              }

-- | Declarations inside an instance declaration.
data UInstBodyDecl dom stage
  = UInstBodyNormalDecl   { _instBodyDeclFunbind :: Ann UValueBind dom stage
                          } -- ^ A normal value binding (@ f x = 12 @)
  | UInstBodyTypeSig      { _instBodyTypeSig :: Ann UTypeSignature dom stage
                          } -- ^ Type signature in instance definition with @InstanceSigs@
  | UInstBodyTypeDecl     { _instBodyTypeEqn :: Ann UTypeEqn dom stage
                          } -- ^ An associated type definition (@ type A X = B @)
  | UInstBodyDataDecl     { _instBodyDataNew :: Ann UDataOrNewtypeKeyword dom stage
                          , _instBodyLhsType :: Ann UInstanceRule dom stage
                          , _instBodyDataCons :: AnnListG UConDecl dom stage
                          , _instBodyDerivings :: AnnMaybeG UDeriving dom stage
                          } -- ^ An associated data type implementation (@ data A X = C1 | C2 @)
  | UInstBodyGadtDataDecl { _instBodyDataNew :: Ann UDataOrNewtypeKeyword dom stage
                          , _instBodyLhsType :: Ann UInstanceRule dom stage
                          , _instBodyDataKind :: AnnMaybeG UKindConstraint dom stage
                          , _instBodyGadtCons :: AnnListG UGadtConDecl dom stage
                          , _instBodyDerivings :: AnnMaybeG UDeriving dom stage
                          } -- ^ An associated data type implemented using GADT style
  | USpecializeInstance   { _specializeInstanceType :: Ann UType dom stage
                          } -- ^ Specialize instance pragma (no phase selection is allowed)
  | UInlineInstance       { _instanceInline :: Ann UInlinePragma dom stage
                          } -- ^ Inline-like pragma in a class instance
  -- not supported yet
-- | UInstBodyPatSyn       { _instBodyPatSyn :: Ann UPatternSynonym dom stage
  --                         } -- ^ A pattern synonym in a class instance

-- | Overlap pragmas. Can be applied to class declarations and class instance declarations.    
data UOverlapPragma dom stage
  = UEnableOverlap     -- ^ @OVERLAP@ pragma
  | UDisableOverlap    -- ^ @NO_OVERLAP@ pragma
  | UOverlappable      -- ^ @OVERLAPPABLE@ pragma
  | UOverlapping       -- ^ @OVERLAPPING@ pragma
  | UOverlaps          -- ^ @OVERLAPS@ pragma
  | UIncoherentOverlap -- ^ @INCOHERENT@ pragma

-- * Type families

-- | Open type and data families
data UTypeFamily dom stage
  = UTypeFamily { _tfHead :: Ann UDeclHead dom stage
                , _tfSpec :: AnnMaybeG UTypeFamilySpec dom stage
                } -- ^ Type family declaration (@ type family A a :: * -> * @)
  | UDataFamily { _tfHead :: Ann UDeclHead dom stage
                , _tfKind :: AnnMaybeG UKindConstraint dom stage
                } -- ^ Data family declaration (@ data family A a :: * -> * @)

-- | Type family specification with kinds specification and injectivity.
data UTypeFamilySpec dom stage
  = UTypeFamilyKind { _tfSpecKind :: Ann UKindConstraint dom stage
                    } -- ^ Specifies the kind of a type family (@ :: * -> * @)
  | UTypeFamilyInjectivity { _tfInjectivity :: Ann UInjectivityAnn dom stage
                           } -- ^ Specifies the injectivity of a type family (@ = r | r -> a @)

-- | Injectivity annotation for type families (@ = r | r -> a @)
data UInjectivityAnn dom stage
  = UInjectivityAnn { _injAnnRes :: Ann UName dom stage
                    , _injAnnDeps :: AnnListG UName dom stage
                    }

-- | Type equations as found in closed type families (@ T A = S @)
data UTypeEqn dom stage
  = UTypeEqn { _teLhs :: Ann UType dom stage
             , _teRhs :: Ann UType dom stage
             }

-- * Type definitions

-- | GADT constructor declaration (@ D1 :: { val :: Int } -> T String @)
data UGadtConDecl dom stage
  = UGadtConDecl { _gadtConNames :: AnnListG UName dom stage
                 , _gadtConType :: Ann UGadtConType dom stage
                 }
                   
-- | The @data@ or the @newtype@ keyword to define ADTs.
data UDataOrNewtypeKeyword dom stage
  = UDataKeyword
  | UNewtypeKeyword

-- | Type of GADT constructors (can be record types: @{ val :: Int }@)
data UGadtConType dom stage
  = UGadtNormalType { _gadtConNormalType :: Ann UType dom stage
                    }
  | UGadtRecordType { _gadtConRecordFields :: AnnListG UFieldDecl dom stage
                    , _gadtConResultType :: Ann UType dom stage
                    }

-- | A list of functional dependencies: @ | a -> b, c -> d @ separated by commas  
data UFunDeps dom stage
  = UFunDeps { _funDeps :: AnnListG UFunDep dom stage
             } 
         
-- | A functional dependency, given on the form @l1 ... ln -> r1 ... rn@         
data UFunDep dom stage
  = UFunDep { _funDepLhs :: AnnListG UName dom stage
            , _funDepRhs :: AnnListG UName dom stage
            }
  
-- | A constructor declaration for a datatype
data UConDecl dom stage
  = UConDecl      { _conDeclName :: Ann UName dom stage
                  , _conDeclArgs :: AnnListG UType dom stage
                  } -- ^ Ordinary data constructor (@ C t1 t2 @)
  | URecordDecl   { _conDeclName :: Ann UName dom stage
                  , _conDeclFields :: AnnListG UFieldDecl dom stage
                  } -- ^ Record data constructor (@ C { _n1 :: t1, _n2 :: t2 } @)
  | UInfixConDecl { _conDeclLhs :: Ann UType dom stage
                  , _conDeclOp :: Ann UOperator dom stage
                  , _conDeclRhs :: Ann UType dom stage
                  } -- ^ Infix data constructor (@ t1 :+: t2 @)
  
-- | Field declaration (@ fld :: Int @)
data UFieldDecl dom stage
  = UFieldDecl { _fieldNames :: AnnListG UName dom stage
               , _fieldType :: Ann UType dom stage
               }
  
-- | A deriving clause following a data type declaration. (@ deriving Show @ or @ deriving (Show, Eq) @)
data UDeriving dom stage
  = UDerivingOne { _oneDerived :: Ann UInstanceHead dom stage }
  | UDerivings { _allDerived :: AnnListG UInstanceHead dom stage }

-- * Pattern synonyms

-- | Pattern type signature declaration (@ pattern Succ :: Int -> Int @)
data UPatternTypeSignature dom stage
  = UPatternTypeSignature { _patSigName :: Ann UName dom stage
                          , _patSigType :: Ann UType dom stage
                          }   

-- | Pattern synonyms: @ pattern Arrow t1 t2 = App "->" [t1, t2] @
data UPatternSynonym dom stage
  = UPatternSynonym { _patLhs :: Ann UPatSynLhs dom stage
                    , _patRhs :: Ann UPatSynRhs dom stage
                    }

-- | Left hand side of a pattern synonym
data UPatSynLhs dom stage
  = UNormalPatSyn { _patName :: Ann UName dom stage
                  , _patArgs :: AnnListG UName dom stage
                  } -- ^ A left hand side with a constructor name and arguments (@ Arrow t1 t2 @)
  | UInfixPatSyn { _patSynLhs :: Ann UName dom stage
                 , _patSynOp :: Ann UOperator dom stage
                 , _patSynRhs :: Ann UName dom stage
                 } -- ^ An infix pattern synonym left-hand side (@ t1 :+: t2 @)
  | URecordPatSyn { _patName :: Ann UName dom stage
                  , _patArgs :: AnnListG UName dom stage
                  } -- ^ A record-style pattern synonym left-hand side (@ Arrow { arrowFrom, arrowTo } @)

-- | Right-hand side of pattern synonym
data UPatSynRhs dom stage
  -- TODO: this feels bad, changing _patRhsOpposite may switch between <- and =
  = UBidirectionalPatSyn { _patRhsPat :: Ann UPattern dom stage
                         , _patRhsOpposite :: AnnMaybeG UPatSynWhere dom stage
                         } -- ^ @ pattern Int = App "Int" [] @ or @ pattern Int <- App "Int" [] where Int = App "Int" [] @
  | UOneDirectionalPatSyn { _patRhsPat :: Ann UPattern dom stage
                          } -- ^ @ pattern Int <- App "Int" [] @

-- | Where clause of pattern synonym (explicit expression direction)
data UPatSynWhere dom stage
  = UPatSynWhere { _patOpposite :: AnnListG UMatch dom stage }

-- * Foreign imports
  
-- | Call conventions of foreign functions
data UCallConv dom stage
  = UStdCall
  | UCCall
  | UCPlusPlus
  | UDotNet
  | UJvm
  | UJs
  | UJavaScript
  | UCApi

-- | Safety annotations for foreign calls
data USafety dom stage
  = USafe
  | UThreadSafe
  | UUnsafe
  | UInterruptible

-- * Role annotations

-- | Role annotations for types
data URole dom stage
  = UNominal
  | URepresentational
  | UPhantom

-- * Pragmas

-- | Top level pragmas
data UTopLevelPragma dom stage
  = URulePragma       { _pragmaRule :: AnnListG URule dom stage
                      } -- ^ A pragma that introduces source rewrite rules (@ {-# RULES "map/map" [2]  forall f g xs. map f (map g xs) = map (f.g) xs #-} @)
  | UDeprPragma       { _pragmaObjects :: AnnListG UName dom stage
                      , _pragmaMessage :: Ann UStringNode dom stage
                      } -- ^ A pragma that marks definitions as deprecated (@ {-# DEPRECATED f "f will be replaced by g" @)
  | UWarningPragma    { _pragmaObjects :: AnnListG UName dom stage
                      , _pragmaMessage :: Ann UStringNode dom stage
                      } -- ^ A pragma that marks definitions as deprecated (@ {-# WARNING unsafePerformIO "you should know what you are doing" @)
  | UAnnPragma        { _annotationSubject :: Ann UAnnotationSubject dom stage
                      , _annotateExpr :: Ann UExpr dom stage
                      } -- ^ A pragma that annotates a definition with an arbitrary value (@ {-# ANN f 42 @)
  -- TODO: extract pragmas that appear both in top-level and in instances (inline, inlinable, noinline)
  | UInlinePragmaDecl { _pragmaInline :: Ann UInlinePragma dom stage }
  | ULinePragma       { _pragmaLineNum :: Ann LineNumber dom stage
                      , _pragmaFileName :: AnnMaybeG UStringNode dom stage
                      } -- ^ A pragma for maintaining line numbers in generated sources (@ {-# LINE 123 "somefile" #-} @)
  | USpecializePragma { _pragmaPhase :: AnnMaybeG UPhaseControl dom stage
                      , _specializeDef :: Ann UName dom stage
                      , _specializeType :: AnnListG UType dom stage
                      } -- ^ A pragma that tells the compiler that a polymorph function should be optimized for a given type (@ {-# SPECIALISE f :: Int -> b -> b #-} @)

-- | A rewrite rule (@ "map/map" forall f g xs. map f (map g xs) = map (f.g) xs @)
data URule dom stage
  = URule { _ruleName :: Ann UStringNode dom stage -- ^ User name of the rule
          , _rulePhase :: AnnMaybeG UPhaseControl dom stage -- ^ The compilation phases in which the rule can be applied
          , _ruleBounded :: AnnListG UTyVar dom stage -- ^ Variables bound in the rule
          , _ruleLhs :: Ann UExpr dom stage -- ^ The transformed expression
          , _ruleRhs :: Ann UExpr dom stage -- ^ The resulting expression
          }
 
-- | Annotation allows you to connect an expression to any declaration. 
data UAnnotationSubject dom stage
  = UNameAnnotation { _annotateName :: Ann UName dom stage
                    } -- ^ The definition with the given name is annotated
  | UTypeAnnotation { _annotateName :: Ann UName dom stage
                    } -- ^ A type with the given name is annotated
  | UModuleAnnotation -- ^ The whole module is annotated

-- | Formulas of minimal annotations declaring which functions should be defined.
data UMinimalFormula dom stage
  = UMinimalName  { _minimalName :: Ann UName dom stage
                  }
  | UMinimalParen { _minimalInner :: Ann UMinimalFormula dom stage
                  }
  | UMinimalOr    { _minimalOrs :: AnnListG UMinimalFormula dom stage
                  } -- ^ One of the minimal formulas are needed (@ min1 | min2 @)
  | UMinimalAnd   { _minimalAnds :: AnnListG UMinimalFormula dom stage
                  } -- ^ Both of the minimal formulas are needed (@ min1 , min2 @)

-- | A line number for a line pragma.
data LineNumber dom stage
  = LineNumber { _lineNumber :: Int } 
