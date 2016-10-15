-- | Representation of Haskell AST definitions. These include definition of data types, classes, instances and so on. 
-- The definition of value bindings are in the Binds module.
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
  = UTypeDecl             { _declHead :: Ann DeclHead dom stage
                          , _declType :: Ann Type dom stage
                          } -- ^ A type synonym ( @type String = [Char]@ )
  | UTypeFamilyDecl       { _declTypeFamily :: Ann TypeFamily dom stage
                          }
  | UClosedTypeFamilyDecl { _declHead :: Ann DeclHead dom stage
                          , _declKind :: AnnMaybe KindConstraint dom stage
                          , _declDecl :: AnnList TypeEqn dom stage -- ^ cannot be empty
                          } -- ^ A closed type family declaration
  | UDataDecl             { _declNewtype :: Ann DataOrNewtypeKeyword dom stage
                          , _declCtx  :: AnnMaybe Context dom stage
                          , _declHead :: Ann DeclHead dom stage
                          , _declCons :: AnnList ConDecl dom stage
                          , _declDeriving :: AnnMaybe Deriving dom stage
                          } -- ^ A data or newtype declaration. Empty data type declarations without 
                            -- where keyword are always belong to DataDecl.
  | UGDataDecl            { _declNewtype :: Ann DataOrNewtypeKeyword dom stage
                          , _declCtx  :: AnnMaybe Context dom stage
                          , _declHead :: Ann DeclHead dom stage
                          , _declKind :: AnnMaybe KindConstraint dom stage
                          , _declGadt :: AnnList GadtConDecl dom stage
                          , _declDeriving :: AnnMaybe Deriving dom stage
                          } -- ^ A data or newtype declaration.
  | UTypeInstDecl         { _declInstance :: Ann InstanceRule dom stage
                          , _declAssignedType :: Ann Type dom stage
                          } -- ^ Type instance declaration (@ type instance Fam T = AssignedT @)
  | UDataInstDecl         { _declNewtype :: Ann DataOrNewtypeKeyword dom stage
                          , _declInstance :: Ann InstanceRule dom stage
                          , _declCons :: AnnList ConDecl dom stage
                          , _declDeriving :: AnnMaybe Deriving dom stage
                          } -- ^ Data instance declaration (@ data instance Fam T = Con1 | Con2 @)
  | UGDataInstDecl        { _declNewtype :: Ann DataOrNewtypeKeyword dom stage
                          , _declInstance :: Ann InstanceRule dom stage
                          , _declKind :: AnnMaybe KindConstraint dom stage
                          , _declGadt :: AnnList GadtConDecl dom stage
                          } -- ^ Gadt style data instance declaration (@ data instance Fam T where ... @)
  | UClassDecl            { _declCtx :: AnnMaybe Context dom stage
                          , _declHead :: Ann DeclHead dom stage
                          , _declFunDeps :: AnnMaybe FunDeps dom stage
                          , _declBody :: AnnMaybe ClassBody dom stage
                          } -- ^ Type class declaration (@ class X a [where f = ...] @)
  | UInstDecl             { _declOverlap :: AnnMaybe OverlapPragma dom stage
                          , _declInstRule :: Ann InstanceRule dom stage
                          , _declInstDecl :: AnnMaybe InstBody dom stage
                          } -- ^ Instance declaration (@ instance X T [where f = ...] @)
  | UPatternSynonymDecl   { _declPatSyn :: Ann PatternSynonym dom stage
                          } -- ^ Pattern synonyms (@ pattern Arrow t1 t2 = App "->" [t1, t2] @)
  | UDerivDecl            { _declOverlap :: AnnMaybe OverlapPragma dom stage
                          , _declInstRule :: Ann InstanceRule dom stage
                          } -- ^ Standalone deriving declaration (@ deriving instance X T @)
  | UFixityDecl           { _declFixity :: Ann FixitySignature dom stage
                          } -- ^ Fixity declaration (@ infixl 5 +, - @)
  | UDefaultDecl          { _declTypes :: AnnList Type dom stage
                          } -- ^ Default types (@ default (T1, T2) @)
  | UTypeSigDecl          { _declTypeSig :: Ann TypeSignature dom stage
                          } -- ^ Type signature declaration (@ _f :: Int -> Int @)
  | UPatTypeSigDecl       { _declPatTypeSig :: Ann PatternTypeSignature dom stage
                          } -- ^ Type signature declaration (@ _f :: Int -> Int @)
  | UValueBinding         { _declValBind :: Ann ValueBind dom stage
                          } -- ^ Function binding (@ f x = 12 @)
  | UForeignImport        { _declCallConv :: Ann CallConv dom stage
                          , _declSafety :: AnnMaybe Safety dom stage
                          , _declName :: Ann Name dom stage
                          , _declType :: Ann Type dom stage
                          } -- ^ Foreign import (@ foreign import _foo :: Int -> IO Int @)
  | UForeignExport        { _declCallConv :: Ann CallConv dom stage
                          , _declName :: Ann Name dom stage
                          , _declType :: Ann Type dom stage
                          } -- ^ foreign export (@ foreign export ccall _foo :: Int -> IO Int @)
  | UPragmaDecl           { _declPragma :: Ann TopLevelPragma dom stage
                          } -- ^ top level pragmas
  | URoleDecl             { _declRoleType :: Ann QualifiedName dom stage
                          , _declRoles :: AnnList Role dom stage
                          } -- ^ role annotations (@ type role Ptr representational @)
  | USpliceDecl           { _declSplice :: Ann Splice dom stage
                          } -- ^ A Template Haskell splice declaration (@ $(generateDecls) @)
    
-- | Open type and data families
data TypeFamily dom stage
  = UTypeFamily { _tfHead :: Ann DeclHead dom stage
                , _tfSpec :: AnnMaybe TypeFamilySpec dom stage
                } -- ^ A type family declaration (@ type family A _a :: * -> * @)    
  | UDataFamily { _tfHead :: Ann DeclHead dom stage
                , _tfKind :: AnnMaybe KindConstraint dom stage
                } -- ^ Data family declaration

-- | Type family specification with kinds specification and injectivity.
data TypeFamilySpec dom stage
  = UTypeFamilyKind { _tfSpecKind :: Ann KindConstraint dom stage
                    }
  | UTypeFamilyInjectivity { _tfInjectivity :: Ann InjectivityAnn dom stage
                           }

-- | Injectivity annotation for type families (@ = r | r -> a @)
data InjectivityAnn dom stage
  = UInjectivityAnn { _injAnnRes :: Ann Name dom stage
                    , _injAnnDeps :: AnnList Name dom stage
                    }

-- | The list of declarations that can appear in a typeclass
data ClassBody dom stage
  = UClassBody { _cbElements :: AnnList ClassElement dom stage
               }
                 
-- | Members of a class declaration       
data ClassElement dom stage
  = UClsSig     { _ceTypeSig :: Ann TypeSignature dom stage
                } -- ^ Signature: @ _f :: A -> B @
  | UClsDef     { _ceBind :: Ann ValueBind dom stage
                } -- ^ Default binding: @ f x = "aaa" @
  | UClsTypeFam { _ceTypeFam :: Ann TypeFamily dom stage
                } -- ^ Declaration of an associated type synonym: @ type T _x :: * @ 
  | UClsTypeDef { _ceHead :: Ann DeclHead dom stage
                , _ceKind :: Ann Type dom stage
                } -- ^ Default choice for type synonym: @ type T x = TE @ or @ type instance T x = TE @ 
  | UClsDefSig  { _ceName :: Ann Name dom stage
                , _ceType :: Ann Type dom stage
                } -- ^ Default signature (by using @DefaultSignatures@): @ default _enum :: (Generic a, GEnum (Rep a)) => [a] @
  | UClsMinimal { _pragmaFormula :: Ann MinimalFormula dom stage
                } -- ^ Minimal pragma: @ {-# MINIMAL (==) | (/=) #-} @

   -- not supported yet (GHC 7.10.3)
  | UClsPatSig  { _cePatSig :: Ann PatternTypeSignature dom stage
                } -- ^ Pattern signature in a class declaration (by using @PatternSynonyms@)
       
-- The declared (possibly parameterized) type (@ A x :+: B y @).
data DeclHead dom stage
  = UDeclHead { _dhName :: Ann Name dom stage
              } -- ^ Type or class name
  | UDHParen  { _dhBody :: Ann DeclHead dom stage
              } -- ^ Parenthesized type
  | UDHApp    { _dhAppFun :: Ann DeclHead dom stage
              , _dhAppOperand :: Ann TyVar dom stage
              } -- ^ Type application
  | UDHInfix  { _dhLeft :: Ann TyVar dom stage
              , _dhOperator :: Ann Operator dom stage
              , _dhRight :: Ann TyVar dom stage
              } -- ^ Infix application of the type/class name to the left operand
       
-- | Instance body is the implementation of the class functions (@ where a x = 1; b x = 2 @)
data InstBody dom stage
  = UInstBody { _instBodyDecls :: AnnList InstBodyDecl dom stage
              }

-- | Declarations inside an instance declaration.
data InstBodyDecl dom stage
  = UInstBodyNormalDecl   { _instBodyDeclFunbind :: Ann ValueBind dom stage
                          } -- ^ A normal declaration (@ f x = 12 @)
  | UInstBodyTypeSig      { _instBodyTypeSig :: Ann TypeSignature dom stage
                          } -- ^ Type signature in instance definition with @InstanceSigs@
  | UInstBodyTypeDecl     { _instBodyTypeEqn :: Ann TypeEqn dom stage
                          } -- ^ An associated type definition (@ type A X = B @)
  | UInstBodyDataDecl     { _instBodyDataNew :: Ann DataOrNewtypeKeyword dom stage
                          , _instBodyLhsType :: Ann InstanceRule dom stage
                          , _instBodyDataCons :: AnnList ConDecl dom stage
                          , _instBodyDerivings :: AnnMaybe Deriving dom stage
                          } -- ^ An associated data type implementation (@ data A X = C1 | C2 @)
  | UInstBodyGadtDataDecl { _instBodyDataNew :: Ann DataOrNewtypeKeyword dom stage
                          , _instBodyLhsType :: Ann InstanceRule dom stage
                          , _instBodyDataKind :: AnnMaybe KindConstraint dom stage
                          , _instBodyGadtCons :: AnnList GadtConDecl dom stage
                          , _instBodyDerivings :: AnnMaybe Deriving dom stage
                          } -- ^ An associated data type implemented using GADT style
  | USpecializeInstance   { _specializeInstanceType :: Ann Type dom stage
                          } -- ^ Specialize instance pragma (no phase selection is allowed)
  -- not supported yet
  | UInstBodyPatSyn       { _instBodyPatSyn :: Ann PatternSynonym dom stage
                          } -- ^ A pattern synonym in a class instance

-- | GADT constructor declaration (@ _D1 :: { _val :: Int } -> T String @)
data GadtConDecl dom stage
  = UGadtConDecl { _gadtConNames :: AnnList Name dom stage
                 , _gadtConType :: Ann GadtConType dom stage
                 }
             
-- | Type of GADT constructors (can be record types: @{ _val :: Int }@)
data GadtConType dom stage
  = UGadtNormalType { _gadtConNormalType :: Ann Type dom stage
                    }
  | UGadtRecordType { _gadtConRecordFields :: AnnList FieldDecl dom stage
                    , _gadtConResultType :: Ann Type dom stage
                    }

-- | A list of functional dependencies: @ | a -> b, c -> d @ separated by commas  
data FunDeps dom stage
  = UFunDeps { _funDeps :: AnnList FunDep dom stage
             } 
         
-- | A functional dependency, given on the form @l1 ... ln -> r1 ... rn@         
data FunDep dom stage
  = UFunDep { _funDepLhs :: AnnList Name dom stage
            , _funDepRhs :: AnnList Name dom stage
            }
  
-- | A constructor declaration for a datatype
data ConDecl dom stage
  = UConDecl      { _conDeclName :: Ann Name dom stage
                  , _conDeclArgs :: AnnList Type dom stage
                  } -- ^ ordinary data constructor (@ C t1 t2 @)
  | URecordDecl   { _conDeclName :: Ann Name dom stage
                  , _conDeclFields :: AnnList FieldDecl dom stage
                  } -- ^ record data constructor (@ C { _n1 :: t1, _n2 :: t2 } @)
  | UInfixConDecl { _conDeclLhs :: Ann Type dom stage
                  , _conDeclOp :: Ann Operator dom stage
                  , _conDeclRhs :: Ann Type dom stage
                  } -- ^ infix data constructor (@ t1 :+: t2 @)
  
-- | Field declaration (@ _fld :: Int @)
data FieldDecl dom stage
  = UFieldDecl { _fieldNames :: AnnList Name dom stage
               , _fieldType :: Ann Type dom stage
               }
  
-- | A deriving clause following a data type declaration. (@ deriving Show @ or @ deriving (Show, Eq) @)
data Deriving dom stage
  = UDerivingOne { _oneDerived :: Ann InstanceHead dom stage }
  | UDerivings { _allDerived :: AnnList InstanceHead dom stage }
  
-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the where keyword.
data InstanceRule dom stage
  = UInstanceRule  { _irVars :: AnnMaybe (AnnList TyVar) dom stage
                   , _irCtx :: AnnMaybe Context dom stage
                   , _irHead :: Ann InstanceHead dom stage
                   }
  | UInstanceParen { _irRule :: Ann InstanceRule dom stage
                   }

-- | The specification of the class instance declaration
data InstanceHead dom stage
  = UInstanceHeadCon   { _ihConName :: Ann Name dom stage
                       } -- ^ Type or class name
  | UInstanceHeadInfix { _ihLeftOp :: Ann Type dom stage
                       , _ihOperator :: Ann Name dom stage
                       } -- ^ Infix application of the type/class name to the left operand
  | UInstanceHeadParen { _ihHead :: Ann InstanceHead dom stage
                       } -- ^ Parenthesized instance head
  | UInstanceHeadApp   { _ihFun :: Ann InstanceHead dom stage
                       , _ihType :: Ann Type dom stage
                       } -- ^ Application to one more type
        
-- | Type equations as found in closed type families (@ T A = S @)
data TypeEqn dom stage
  = UTypeEqn { _teLhs :: Ann Type dom stage
             , _teRhs :: Ann Type dom stage
             }

-- | A pattern type signature (@ pattern p :: Int -> T @)
data PatternTypeSignature dom stage
  = UPatternTypeSignature { _patSigName :: Ann Name dom stage
                          , _patSigType :: Ann Type dom stage
                          }   

-- | Pattern synonyms: @ pattern Arrow t1 t2 = App "->" [t1, t2] @
data PatternSynonym dom stage
  = UPatternSynonym { _patLhs :: Ann PatSynLhs dom stage
                    , _patRhs :: Ann PatSynRhs dom stage
                    }

-- | Left hand side of a pattern synonym
data PatSynLhs dom stage
  = UNormalPatSyn { _patName :: Ann Name dom stage
                  , _patArgs :: AnnList Name dom stage
                  }
  | UInfixPatSyn { _patSynLhs :: Ann Name dom stage
                 , _patSynOp :: Ann Operator dom stage
                 , _patSynRhs :: Ann Name dom stage
                 }
  | URecordPatSyn { _patName :: Ann Name dom stage
                  , _patArgs :: AnnList Name dom stage
                  }

-- | Right-hand side of pattern synonym
data PatSynRhs dom stage
  -- TODO: this feels bad, changing _patRhsOpposite may switch between <- and =
  = UBidirectionalPatSyn { _patRhsPat :: Ann Pattern dom stage
                         , _patRhsOpposite :: AnnMaybe PatSynWhere dom stage
                         } -- ^ @ pattern Int = App "Int" [] @ or @ pattern Int <- App "Int" [] where Int = App "Int" [] @
  | UOneDirectionalPatSyn { _patRhsPat :: Ann Pattern dom stage
                          } -- ^ @ pattern Int <- App "Int" [] @

-- | Where clause of pattern synonym (explicit expression direction)
data PatSynWhere dom stage
  = UPatSynWhere { _patOpposite :: AnnList Match dom stage }
