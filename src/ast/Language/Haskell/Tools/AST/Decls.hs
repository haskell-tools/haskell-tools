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
data Decl a
  = TypeDecl             { _declHead :: Ann DeclHead a
                         , _declType :: Ann Type a
                         } -- ^ A type synonym ( @type String = [Char]@ )
  | TypeFamilyDecl       { _declTypeFamily :: Ann TypeFamily a 
                         }
  | ClosedTypeFamilyDecl { _declHead :: Ann DeclHead a
                         , _declKind :: AnnMaybe KindConstraint a
                         , _declDecl :: AnnList TypeEqn a -- ^ cannot be empty
                         } -- ^ A closed type family declaration
  | DataDecl             { _declNewtype :: Ann DataOrNewtypeKeyword a
                         , _declCtx  :: AnnMaybe Context a
                         , _declHead :: Ann DeclHead a
                         , _declCons :: AnnList ConDecl a
                         , _declDeriving :: AnnMaybe Deriving a
                         } -- ^ A data or newtype declaration. Empty data type declarations without 
                           -- where keyword are always belong to DataDecl.
  | GDataDecl            { _declNewtype :: Ann DataOrNewtypeKeyword a
                         , _declCtx  :: AnnMaybe Context a
                         , _declHead :: Ann DeclHead a
                         , _declKind :: AnnMaybe KindConstraint a
                         , _declGadt :: AnnList GadtConDecl a
                         , _declDeriving :: AnnMaybe Deriving a
                         } -- ^ A data or newtype declaration.
  | TypeInstDecl         { _declInstance :: Ann InstanceRule a
                         , _declAssignedType :: Ann Type a
                         } -- ^ Type instance declaration (@ type instance Fam T = AssignedT @)
  | DataInstDecl         { _declNewtype :: Ann DataOrNewtypeKeyword a
                         , _declInstance :: Ann InstanceRule a
                         , _declCons :: AnnList ConDecl a
                         , _declDeriving :: AnnMaybe Deriving a
                         } -- ^ Data instance declaration (@ data instance Fam T = Con1 | Con2 @)
  | GDataInstDecl        { _declNewtype :: Ann DataOrNewtypeKeyword a
                         , _declInstance :: Ann InstanceRule a
                         , _declKind :: AnnMaybe KindConstraint a
                         , _declGadt :: AnnList GadtConDecl a
                         } -- ^ Gadt style data instance declaration (@ data instance Fam T where ... @)
  | ClassDecl            { _declCtx :: AnnMaybe Context a
                         , _declHead :: Ann DeclHead a
                         , _declFunDeps :: AnnMaybe FunDeps a
                         , _declBody :: AnnMaybe ClassBody a
                         } -- ^ Type class declaration (@ class X a [where f = ...] @)
  | InstDecl             { _declOverlap :: AnnMaybe OverlapPragma a
                         , _declInstRule :: Ann InstanceRule a
                         , _declInstDecl :: AnnMaybe InstBody a
                         } -- ^ Instance declaration (@ instance X T [where f = ...] @)
  | PatternSynonymDecl   { _declPatSyn :: Ann PatternSynonym a
                         } -- ^ Pattern synonyms (@ pattern Arrow t1 t2 = App "->" [t1, t2] @)
  | DerivDecl            { _declOverlap :: AnnMaybe OverlapPragma a
                         , _declInstRule :: Ann InstanceRule a
                         } -- ^ Standalone deriving declaration (@ deriving instance X T @)
  | FixityDecl           { _declFixity :: Ann FixitySignature a 
                         } -- ^ Fixity declaration (@ infixl 5 +, - @)
  | DefaultDecl          { _declTypes :: AnnList Type a
                         } -- ^ Default types (@ default (T1, T2) @)
  | TypeSigDecl          { _declTypeSig :: Ann TypeSignature a 
                         } -- ^ Type signature declaration (@ _f :: Int -> Int @)
  | PatTypeSigDecl       { _declPatTypeSig :: Ann PatternTypeSignature a 
                         } -- ^ Type signature declaration (@ _f :: Int -> Int @)
  | ValueBinding         { _declValBind :: Ann ValueBind a
                         } -- ^ Function binding (@ f x = 12 @)
  | ForeignImport        { _declCallConv :: Ann CallConv a
                         , _declSafety :: AnnMaybe Safety a
                         , _declName :: Ann Name a
                         , _declType :: Ann Type a
                         } -- ^ Foreign import (@ foreign import _foo :: Int -> IO Int @)
  | ForeignExport        { _declCallConv :: Ann CallConv a
                         , _declName :: Ann Name a
                         , _declType :: Ann Type a
                         } -- ^ foreign export (@ foreign export ccall _foo :: Int -> IO Int @)
  | PragmaDecl           { _declPragma :: Ann TopLevelPragma a 
                         } -- ^ top level pragmas
  | RoleDecl             { _declRoleType :: Ann SimpleName a 
                         , _declRoles :: AnnList Role a
                         } -- ^ role annotations (@ type role Ptr representational @)
  | SpliceDecl           { _declSplice :: Ann Splice a 
                         } -- ^ A Template Haskell splice declaration (@ $(generateDecls) @)
    
-- | Open type and data families
data TypeFamily a
  = TypeFamily { _tfHead :: Ann DeclHead a
               , _tfSpec :: AnnMaybe TypeFamilySpec a
               } -- ^ A type family declaration (@ type family A _a :: * -> * @)    
  | DataFamily { _tfHead :: Ann DeclHead a
               , _tfKind :: AnnMaybe KindConstraint a
               } -- ^ Data family declaration
                  
data TypeFamilySpec a 
  = TypeFamilyKind { _tfSpecKind :: Ann KindConstraint a 
                   }
  | TypeFamilyInjectivity { _tfInjectivity :: Ann InjectivityAnn a
                          }

data InjectivityAnn a
  = InjectivityAnn { _injAnnRes :: Ann Name a
                   , _injAnnDeps :: AnnList Name a
                   }

-- | The list of declarations that can appear in a typeclass
data ClassBody a
  = ClassBody { _cbElements :: AnnList ClassElement a 
              }
                 
-- | Members of a class declaration       
data ClassElement a
  = ClsSig     { _ceTypeSig :: Ann TypeSignature a 
               } -- ^ Signature: @ _f :: A -> B @
  | ClsDef     { _ceBind :: Ann ValueBind a
               } -- ^ Default binding: @ f x = "aaa" @
  | ClsTypeFam { _ceTypeFam :: Ann TypeFamily a
               } -- ^ Declaration of an associated type synonym: @ type T _x :: * @ 
  | ClsTypeDef { _ceHead :: Ann DeclHead a
               , _ceKind :: Ann Type a
               } -- ^ Default choice for type synonym: @ type T x = TE @ or @ type instance T x = TE @ 
  | ClsDefSig  { _ceName :: Ann Name a
               , _ceType :: Ann Type a
               } -- ^ Default signature (by using @DefaultSignatures@): @ default _enum :: (Generic a, GEnum (Rep a)) => [a] @
  -- not supported yet (GHC 7.10.3)
  | ClsPatSig  { _cePatSig :: Ann PatternTypeSignature a 
               } -- ^ Pattern signature in a class declaration (by using @PatternSynonyms@)
       
-- The declared (possibly parameterized) type (@ A x :+: B y @).
data DeclHead a
  = DeclHead { _dhName :: Ann Name a 
             } -- ^ Type or class name
  | DHParen  { _dhBody :: Ann DeclHead a 
             } -- ^ Parenthesized type
  | DHApp    { _dhAppFun :: Ann DeclHead a
             , _dhAppOperand :: Ann TyVar a
             } -- ^ Type application
  | DHInfix  { _dhLeft :: Ann TyVar a
             , _dhOperator :: Ann Operator a 
             , _dhRight :: Ann TyVar a
             } -- ^ Infix application of the type/class name to the left operand
       
-- | Instance body is the implementation of the class functions (@ where a x = 1; b x = 2 @)
data InstBody a
  = InstBody { _instBodyDecls :: AnnList InstBodyDecl a 
             }

-- | Declarations inside an instance declaration.
data InstBodyDecl a
  = InstBodyNormalDecl   { _instBodyDeclFunbind :: Ann ValueBind a 
                         } -- ^ A normal declaration (@ f x = 12 @)
  | InstBodyTypeSig      { _instBodyTypeSig :: Ann TypeSignature a 
                         } -- ^ Type signature in instance definition with @InstanceSigs@
  | InstBodyTypeDecl     { _instBodyTypeEqn :: Ann TypeEqn a 
                         } -- ^ An associated type definition (@ type A X = B @)
  | InstBodyDataDecl     { _instBodyDataNew :: Ann DataOrNewtypeKeyword a
                         , _instBodyLhsType :: Ann InstanceRule a
                         , _instBodyDataCons :: AnnList ConDecl a
                         , _instBodyDerivings :: AnnMaybe Deriving a
                         } -- ^ An associated data type implementation (@ data A X = C1 | C2 @)
  | InstBodyGadtDataDecl { _instBodyDataNew :: Ann DataOrNewtypeKeyword a
                         , _instBodyLhsType :: Ann InstanceRule a
                         , _instBodyDataKind :: AnnMaybe Kind a
                         , _instBodyGadtCons :: AnnList GadtConDecl a
                         , _instBodyDerivings :: AnnMaybe Deriving a
                         } -- ^ An associated data type implemented using GADT style
  -- not supported yet
  | InstBodyPatSyn       { _instBodyPatSyn :: Ann PatternSynonym a 
                         } -- ^ A pattern synonym in a class instance

-- | GADT constructor declaration (@ _D1 :: { _val :: Int } -> T String @)
data GadtConDecl a
  = GadtConDecl { _gadtConNames :: AnnList Name a
                , _gadtConType :: Ann GadtConType a
                }
             
-- | Type of GADT constructors (can be record types: @{ _val :: Int }@)
data GadtConType a
  = GadtNormalType { _gadtConNormalType :: Ann Type a 
                   }
  | GadtRecordType { _gadtConRecordFields :: AnnList FieldDecl a
                   , _gadtConResultType :: Ann Type a
                   }

data GadtField a
  = GadtNormalField { _gadtFieldType :: Ann Type a 
                    } -- ^ Normal GADT field type (@ Int @)
  | GadtNamedField  { _gadtFieldName :: Ann Name a
                    , _gadtFieldType :: Ann Type a
                    } -- ^ Named GADT field (@ { _val :: Int } @)
         
-- | A list of functional dependencies: @ | a -> b, c -> d @ separated by commas  
data FunDeps a
  = FunDeps { _funDeps :: AnnList FunDep a 
            } 
         
-- | A functional dependency, given on the form @l1 ... ln -> r1 ... rn@         
data FunDep a
  = FunDep { _funDepLhs :: AnnList Name a
           , _funDepRhs :: AnnList Name a
           }
  
data ConDecl a
  = ConDecl      { _conDeclName :: Ann Name a
                 , _conDeclArgs :: AnnList Type a
                 } -- ^ ordinary data constructor (@ C t1 t2 @)
  | RecordDecl   { _conDeclName :: Ann Name a
                 , _conDeclFields :: AnnList FieldDecl a
                 } -- ^ record data constructor (@ C { _n1 :: t1, _n2 :: t2 } @)
  | InfixConDecl { _conDeclLhs :: Ann Type a
                 , _conDeclName :: Ann Name a
                 , _conDeclRhs :: Ann Type a
                 } -- ^ infix data constructor (@ t1 :+: t2 @)
  
-- | Field declaration (@ _fld :: Int @)
data FieldDecl a
  = FieldDecl { _fieldNames :: AnnList Name a
              , _fieldType :: Ann Type a
              }
  
-- | A deriving clause following a data type declaration. (@ deriving Show @ or @ deriving (Show, Eq) @)
data Deriving a
  = DerivingOne { _oneDerived :: Ann InstanceHead a }
  | Derivings { _allDerived :: AnnList InstanceHead a }
  
-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the where keyword.
data InstanceRule a
  = InstanceRule  { _irVars :: AnnMaybe (AnnList TyVar) a
                  , _irCtx :: AnnMaybe Context a
                  , _irHead :: Ann InstanceHead a
                  }
  | InstanceParen { _irRule :: Ann InstanceRule a 
                  }

-- | The specification of the class instance declaration
data InstanceHead a
  = InstanceHeadCon   { _ihConName :: Ann Name a 
                      } -- ^ Type or class name
  | InstanceHeadInfix { _ihLeftOp :: Ann Type a
                      , _ihOperator :: Ann Name a
                      } -- ^ Infix application of the type/class name to the left operand
  | InstanceHeadParen { _ihHead :: Ann InstanceHead a 
                      } -- ^ Parenthesized instance head
  | InstanceHeadApp   { _ihFun :: Ann InstanceHead a
                      , _ihType :: Ann Type a
                      } -- ^ Application to one more type
        
-- | Type equations as found in closed type families (@ T A = S @)
data TypeEqn a
  = TypeEqn { _teLhs :: Ann Type a
            , _teRhs :: Ann Type a
            }

-- | A pattern type signature (@ pattern p :: Int -> T @)
data PatternTypeSignature a 
  = PatternTypeSignature { _patSigName :: Ann Name a
                         , _patSigType :: Ann Type a
                         }   

-- | Pattern synonyms: @ pattern Arrow t1 t2 = App "->" [t1, t2] @
data PatternSynonym a 
  = PatternSynonym { _patLhs :: Ann PatSynLhs a
                   , _patRhs :: Ann PatSynRhs a
                   }

-- | Left hand side of a pattern synonym
data PatSynLhs a
  = NormalPatSyn { _patName :: Ann Name a
                 , _patArgs :: AnnList Name a
                 }
  | InfixPatSyn { _patSynLhs :: Ann Name a 
                , _patSynOp :: Ann Operator a
                , _patSynRhs :: Ann Name a
                }
  | RecordPatSyn { _patName :: Ann Name a
                 , _patArgs :: AnnList Name a
                 }

-- | Right-hand side of pattern synonym
data PatSynRhs a
  = BidirectionalPatSyn { _patRhsPat :: Ann Pattern a
                        , _patRhsOpposite :: AnnMaybe PatSynWhere a
                        } -- ^ @ pattern Int = App "Int" [] @ or @ pattern Int <- App "Int" [] where Int = App "Int" [] @
  | OneDirectionalPatSyn { _patRhsPat :: Ann Pattern a
                         } -- ^ @ pattern Int <- App "Int" [] @

-- | Where clause of pattern synonym (explicit expression direction)
data PatSynWhere a
  = PatSynWhere { _patOpposite :: AnnList Match a }
