-- | Representation of Haskell AST definitions
module Language.Haskell.Tools.AST.Decls where

import Language.Haskell.Tools.AST.Binds
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Patterns
import Language.Haskell.Tools.AST.Kinds
import Language.Haskell.Tools.AST.Exprs
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Ann
import {-# SOURCE #-} Language.Haskell.Tools.AST.TH


-- | Haskell declaration
data Decl dom stage
  = TypeDecl             { _declHead :: Ann DeclHead dom stage
                         , _declType :: Ann Type dom stage
                         } -- ^ A type synonym ( @type String = [Char]@ )
  | TypeFamilyDecl       { _declTypeFamily :: Ann TypeFamily dom stage
                         }
  | ClosedTypeFamilyDecl { _declHead :: Ann DeclHead dom stage
                         , _declKind :: AnnMaybe KindConstraint dom stage
                         , _declDecl :: AnnList TypeEqn dom stage -- ^ cannot be empty
                         } -- ^ A closed type family declaration
  | DataDecl             { _declNewtype :: Ann DataOrNewtypeKeyword dom stage
                         , _declCtx  :: AnnMaybe Context dom stage
                         , _declHead :: Ann DeclHead dom stage
                         , _declCons :: AnnList ConDecl dom stage
                         , _declDeriving :: AnnMaybe Deriving dom stage
                         } -- ^ A data or newtype declaration. Empty data type declarations without 
                           -- where keyword are always belong to DataDecl.
  | GDataDecl            { _declNewtype :: Ann DataOrNewtypeKeyword dom stage
                         , _declCtx  :: AnnMaybe Context dom stage
                         , _declHead :: Ann DeclHead dom stage
                         , _declKind :: AnnMaybe KindConstraint dom stage
                         , _declGadt :: AnnList GadtConDecl dom stage
                         , _declDeriving :: AnnMaybe Deriving dom stage
                         } -- ^ A data or newtype declaration.
  | TypeInstDecl         { _declInstance :: Ann InstanceRule dom stage
                         , _declAssignedType :: Ann Type dom stage
                         } -- ^ Type instance declaration (@ type instance Fam T = AssignedT @)
  | DataInstDecl         { _declNewtype :: Ann DataOrNewtypeKeyword dom stage
                         , _declInstance :: Ann InstanceRule dom stage
                         , _declCons :: AnnList ConDecl dom stage
                         , _declDeriving :: AnnMaybe Deriving dom stage
                         } -- ^ Data instance declaration (@ data instance Fam T = Con1 | Con2 @)
  | GDataInstDecl        { _declNewtype :: Ann DataOrNewtypeKeyword dom stage
                         , _declInstance :: Ann InstanceRule dom stage
                         , _declKind :: AnnMaybe KindConstraint dom stage
                         , _declGadt :: AnnList GadtConDecl dom stage
                         } -- ^ Gadt style data instance declaration (@ data instance Fam T where ... @)
  | ClassDecl            { _declCtx :: AnnMaybe Context dom stage
                         , _declHead :: Ann DeclHead dom stage
                         , _declFunDeps :: AnnMaybe FunDeps dom stage
                         , _declBody :: AnnMaybe ClassBody dom stage
                         } -- ^ Type class declaration (@ class X a [where f = ...] @)
  | InstDecl             { _declOverlap :: AnnMaybe OverlapPragma dom stage
                         , _declInstRule :: Ann InstanceRule dom stage
                         , _declInstDecl :: AnnMaybe InstBody dom stage
                         } -- ^ Instance declaration (@ instance X T [where f = ...] @)
  | PatternSynonymDecl   { _declPatSyn :: Ann PatternSynonym dom stage
                         } -- ^ Pattern synonyms (@ pattern Arrow t1 t2 = App "->" [t1, t2] @)
  | DerivDecl            { _declOverlap :: AnnMaybe OverlapPragma dom stage
                         , _declInstRule :: Ann InstanceRule dom stage
                         } -- ^ Standalone deriving declaration (@ deriving instance X T @)
  | FixityDecl           { _declFixity :: Ann FixitySignature dom stage
                         } -- ^ Fixity declaration (@ infixl 5 +, - @)
  | DefaultDecl          { _declTypes :: AnnList Type dom stage
                         } -- ^ Default types (@ default (T1, T2) @)
  | TypeSigDecl          { _declTypeSig :: Ann TypeSignature dom stage
                         } -- ^ Type signature declaration (@ _f :: Int -> Int @)
  | PatTypeSigDecl       { _declPatTypeSig :: Ann PatternTypeSignature dom stage
                         } -- ^ Type signature declaration (@ _f :: Int -> Int @)
  | ValueBinding         { _declValBind :: Ann ValueBind dom stage
                         } -- ^ Function binding (@ f x = 12 @)
  | ForeignImport        { _declCallConv :: Ann CallConv dom stage
                         , _declSafety :: AnnMaybe Safety dom stage
                         , _declName :: Ann Name dom stage
                         , _declType :: Ann Type dom stage
                         } -- ^ Foreign import (@ foreign import _foo :: Int -> IO Int @)
  | ForeignExport        { _declCallConv :: Ann CallConv dom stage
                         , _declName :: Ann Name dom stage
                         , _declType :: Ann Type dom stage
                         } -- ^ foreign export (@ foreign export ccall _foo :: Int -> IO Int @)
  | PragmaDecl           { _declPragma :: Ann TopLevelPragma dom stage
                         } -- ^ top level pragmas
  | RoleDecl             { _declRoleType :: Ann QualifiedName dom stage
                         , _declRoles :: AnnList Role dom stage
                         } -- ^ role annotations (@ type role Ptr representational @)
  | SpliceDecl           { _declSplice :: Ann Splice dom stage
                         } -- ^ A Template Haskell splice declaration (@ $(generateDecls) @)
    
-- | Open type and data families
data TypeFamily dom stage
  = TypeFamily { _tfHead :: Ann DeclHead dom stage
               , _tfSpec :: AnnMaybe TypeFamilySpec dom stage
               } -- ^ A type family declaration (@ type family A _a :: * -> * @)    
  | DataFamily { _tfHead :: Ann DeclHead dom stage
               , _tfKind :: AnnMaybe KindConstraint dom stage
               } -- ^ Data family declaration
                  
data TypeFamilySpec dom stage
  = TypeFamilyKind { _tfSpecKind :: Ann KindConstraint dom stage
                   }
  | TypeFamilyInjectivity { _tfInjectivity :: Ann InjectivityAnn dom stage
                          }

data InjectivityAnn dom stage
  = InjectivityAnn { _injAnnRes :: Ann Name dom stage
                   , _injAnnDeps :: AnnList Name dom stage
                   }

-- | The list of declarations that can appear in a typeclass
data ClassBody dom stage
  = ClassBody { _cbElements :: AnnList ClassElement dom stage
              }
                 
-- | Members of a class declaration       
data ClassElement dom stage
  = ClsSig     { _ceTypeSig :: Ann TypeSignature dom stage
               } -- ^ Signature: @ _f :: A -> B @
  | ClsDef     { _ceBind :: Ann ValueBind dom stage
               } -- ^ Default binding: @ f x = "aaa" @
  | ClsTypeFam { _ceTypeFam :: Ann TypeFamily dom stage
               } -- ^ Declaration of an associated type synonym: @ type T _x :: * @ 
  | ClsTypeDef { _ceHead :: Ann DeclHead dom stage
               , _ceKind :: Ann Type dom stage
               } -- ^ Default choice for type synonym: @ type T x = TE @ or @ type instance T x = TE @ 
  | ClsDefSig  { _ceName :: Ann Name dom stage
               , _ceType :: Ann Type dom stage
               } -- ^ Default signature (by using @DefaultSignatures@): @ default _enum :: (Generic a, GEnum (Rep a)) => [a] @
  | ClsMinimal { _pragmaFormula :: Ann MinimalFormula dom stage
               } -- ^ Minimal pragma: @ {-# MINIMAL (==) | (/=) #-} @

   -- not supported yet (GHC 7.10.3)
  | ClsPatSig  { _cePatSig :: Ann PatternTypeSignature dom stage
               } -- ^ Pattern signature in a class declaration (by using @PatternSynonyms@)
       
-- The declared (possibly parameterized) type (@ A x :+: B y @).
data DeclHead dom stage
  = DeclHead { _dhName :: Ann Name dom stage
             } -- ^ Type or class name
  | DHParen  { _dhBody :: Ann DeclHead dom stage
             } -- ^ Parenthesized type
  | DHApp    { _dhAppFun :: Ann DeclHead dom stage
             , _dhAppOperand :: Ann TyVar dom stage
             } -- ^ Type application
  | DHInfix  { _dhLeft :: Ann TyVar dom stage
             , _dhOperator :: Ann Operator dom stage
             , _dhRight :: Ann TyVar dom stage
             } -- ^ Infix application of the type/class name to the left operand
       
-- | Instance body is the implementation of the class functions (@ where a x = 1; b x = 2 @)
data InstBody dom stage
  = InstBody { _instBodyDecls :: AnnList InstBodyDecl dom stage
             }

-- | Declarations inside an instance declaration.
data InstBodyDecl dom stage
  = InstBodyNormalDecl   { _instBodyDeclFunbind :: Ann ValueBind dom stage
                         } -- ^ A normal declaration (@ f x = 12 @)
  | InstBodyTypeSig      { _instBodyTypeSig :: Ann TypeSignature dom stage
                         } -- ^ Type signature in instance definition with @InstanceSigs@
  | InstBodyTypeDecl     { _instBodyTypeEqn :: Ann TypeEqn dom stage
                         } -- ^ An associated type definition (@ type A X = B @)
  | InstBodyDataDecl     { _instBodyDataNew :: Ann DataOrNewtypeKeyword dom stage
                         , _instBodyLhsType :: Ann InstanceRule dom stage
                         , _instBodyDataCons :: AnnList ConDecl dom stage
                         , _instBodyDerivings :: AnnMaybe Deriving dom stage
                         } -- ^ An associated data type implementation (@ data A X = C1 | C2 @)
  | InstBodyGadtDataDecl { _instBodyDataNew :: Ann DataOrNewtypeKeyword dom stage
                         , _instBodyLhsType :: Ann InstanceRule dom stage
                         , _instBodyDataKind :: AnnMaybe Kind dom stage
                         , _instBodyGadtCons :: AnnList GadtConDecl dom stage
                         , _instBodyDerivings :: AnnMaybe Deriving dom stage
                         } -- ^ An associated data type implemented using GADT style
  | SpecializeInstance   { _specializeInstanceType :: Ann Type dom stage
                         } -- ^ Specialize instance pragma (no phase selection is allowed)
  -- not supported yet
  | InstBodyPatSyn       { _instBodyPatSyn :: Ann PatternSynonym dom stage
                         } -- ^ A pattern synonym in a class instance

-- | GADT constructor declaration (@ _D1 :: { _val :: Int } -> T String @)
data GadtConDecl dom stage
  = GadtConDecl { _gadtConNames :: AnnList Name dom stage
                , _gadtConType :: Ann GadtConType dom stage
                }
             
-- | Type of GADT constructors (can be record types: @{ _val :: Int }@)
data GadtConType dom stage
  = GadtNormalType { _gadtConNormalType :: Ann Type dom stage
                   }
  | GadtRecordType { _gadtConRecordFields :: AnnList FieldDecl dom stage
                   , _gadtConResultType :: Ann Type dom stage
                   }

data GadtField dom stage
  = GadtNormalField { _gadtFieldType :: Ann Type dom stage
                    } -- ^ Normal GADT field type (@ Int @)
  | GadtNamedField  { _gadtFieldName :: Ann Name dom stage
                    , _gadtFieldType :: Ann Type dom stage
                    } -- ^ Named GADT field (@ { _val :: Int } @)
         
-- | A list of functional dependencies: @ | a -> b, c -> d @ separated by commas  
data FunDeps dom stage
  = FunDeps { _funDeps :: AnnList FunDep dom stage
            } 
         
-- | A functional dependency, given on the form @l1 ... ln -> r1 ... rn@         
data FunDep dom stage
  = FunDep { _funDepLhs :: AnnList Name dom stage
           , _funDepRhs :: AnnList Name dom stage
           }
  
data ConDecl dom stage
  = ConDecl      { _conDeclName :: Ann Name dom stage
                 , _conDeclArgs :: AnnList Type dom stage
                 } -- ^ ordinary data constructor (@ C t1 t2 @)
  | RecordDecl   { _conDeclName :: Ann Name dom stage
                 , _conDeclFields :: AnnList FieldDecl dom stage
                 } -- ^ record data constructor (@ C { _n1 :: t1, _n2 :: t2 } @)
  | InfixConDecl { _conDeclLhs :: Ann Type dom stage
                 , _conDeclOp :: Ann Operator dom stage
                 , _conDeclRhs :: Ann Type dom stage
                 } -- ^ infix data constructor (@ t1 :+: t2 @)
  
-- | Field declaration (@ _fld :: Int @)
data FieldDecl dom stage
  = FieldDecl { _fieldNames :: AnnList Name dom stage
              , _fieldType :: Ann Type dom stage
              }
  
-- | A deriving clause following a data type declaration. (@ deriving Show @ or @ deriving (Show, Eq) @)
data Deriving dom stage
  = DerivingOne { _oneDerived :: Ann InstanceHead dom stage }
  | Derivings { _allDerived :: AnnList InstanceHead dom stage }
  
-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the where keyword.
data InstanceRule dom stage
  = InstanceRule  { _irVars :: AnnMaybe (AnnList TyVar) dom stage
                  , _irCtx :: AnnMaybe Context dom stage
                  , _irHead :: Ann InstanceHead dom stage
                  }
  | InstanceParen { _irRule :: Ann InstanceRule dom stage
                  }

-- | The specification of the class instance declaration
data InstanceHead dom stage
  = InstanceHeadCon   { _ihConName :: Ann Name dom stage
                      } -- ^ Type or class name
  | InstanceHeadInfix { _ihLeftOp :: Ann Type dom stage
                      , _ihOperator :: Ann Name dom stage
                      } -- ^ Infix application of the type/class name to the left operand
  | InstanceHeadParen { _ihHead :: Ann InstanceHead dom stage
                      } -- ^ Parenthesized instance head
  | InstanceHeadApp   { _ihFun :: Ann InstanceHead dom stage
                      , _ihType :: Ann Type dom stage
                      } -- ^ Application to one more type
        
-- | Type equations as found in closed type families (@ T A = S @)
data TypeEqn dom stage
  = TypeEqn { _teLhs :: Ann Type dom stage
            , _teRhs :: Ann Type dom stage
            }

-- | A pattern type signature (@ pattern p :: Int -> T @)
data PatternTypeSignature dom stage
  = PatternTypeSignature { _patSigName :: Ann Name dom stage
                         , _patSigType :: Ann Type dom stage
                         }   

-- | Pattern synonyms: @ pattern Arrow t1 t2 = App "->" [t1, t2] @
data PatternSynonym dom stage
  = PatternSynonym { _patLhs :: Ann PatSynLhs dom stage
                   , _patRhs :: Ann PatSynRhs dom stage
                   }

-- | Left hand side of a pattern synonym
data PatSynLhs dom stage
  = NormalPatSyn { _patName :: Ann Name dom stage
                 , _patArgs :: AnnList Name dom stage
                 }
  | InfixPatSyn { _patSynLhs :: Ann Name dom stage
                , _patSynOp :: Ann Operator dom stage
                , _patSynRhs :: Ann Name dom stage
                }
  | RecordPatSyn { _patName :: Ann Name dom stage
                 , _patArgs :: AnnList Name dom stage
                 }

-- | Right-hand side of pattern synonym
data PatSynRhs dom stage
  = BidirectionalPatSyn { _patRhsPat :: Ann Pattern dom stage
                        , _patRhsOpposite :: AnnMaybe PatSynWhere dom stage
                        } -- ^ @ pattern Int = App "Int" [] @ or @ pattern Int <- App "Int" [] where Int = App "Int" [] @
  | OneDirectionalPatSyn { _patRhsPat :: Ann Pattern dom stage
                         } -- ^ @ pattern Int <- App "Int" [] @

-- | Where clause of pattern synonym (explicit expression direction)
data PatSynWhere dom stage
  = PatSynWhere { _patOpposite :: AnnList Match dom stage }
