module Language.Haskell.Tools.AST.Decl where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Literals

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
                         } -- ^ A data or newtype declaration.
  | GDataDecl            { _declNewtype :: Ann DataOrNewtypeKeyword a
                         , _declCtx  :: AnnMaybe Context a
                         , _declHead :: Ann DeclHead a
                         , _declKind :: AnnMaybe KindConstraint a
                         , _declGadt :: Ann GadtDeclList a
                         , _declDeriving :: AnnMaybe Deriving a
                         } -- ^ A data or newtype declaration.
  | TypeInstDecl         { _declInstance :: Ann Type a
                         , _declAssignedType :: Ann Type a
                         } -- ^ Type instance declaration (@ type instance Fam T = AssignedT @)
  | DataInstDecl         { _declNewtype :: Ann DataOrNewtypeKeyword a
                         , _declInstance :: Ann Type a
                         , _declCons :: AnnList ConDecl a
                         } -- ^ Data instance declaration (@ data instance Fam T = Con1 | Con2 @)
  | GDataInstDecl        { _declNewtype :: Ann DataOrNewtypeKeyword a
                         , _declInstance :: Ann Type a
                         , _declKind :: AnnMaybe KindConstraint a
                         , _declGadt :: Ann GadtDeclList a
                         } -- ^ Data instance declaration (@ data instance T = Con1 | Con2 @)
  | ClassDecl            { _declCtx :: AnnMaybe Context a
                         , _declHead :: Ann DeclHead a
                         , _declFunDeps :: AnnMaybe FunDeps a
                         , _declBody :: AnnMaybe ClassBody a
                         } -- ^ Type class declaration (@ class X a [where f = ...] @)
  | InstDecl             { _declOverlap :: AnnMaybe OverlapPragma a
                         , _declInstRule :: Ann InstanceRule a
                         , _declInstDecl :: AnnMaybe InstBody a
                         } -- ^ Instance declaration (@ instance X T [where f = ...] @)
  | DerivDecl            { _declOverlap :: AnnMaybe OverlapPragma a
                         , _declInstRule :: Ann InstanceRule a
                         } -- ^ Standalone deriving declaration (@ deriving instance X T @)
  | FixityDecl           { _declFixity :: Ann FixitySignature a 
                         } -- ^ Fixity declaration (@ infixl 5 +, - @)
  | DefaultDecl          { _declTypes :: AnnList Type a
                         } -- ^ Default types (@ default (T1, T2) @)
  | TypeSigDecl          { _declTypeSig :: Ann TypeSignature a 
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
  | Pragma               { _declPragma :: Ann TopLevelPragma a 
                         } -- ^ top level pragmas
  | SpliceDecl           { _declSplice :: Ann Splice a 
                         } -- ^ A Template Haskell splice declaration (@ $(generateDecls) @)
       
-- | A type signature (@ _f :: Int -> Int @)
data TypeSignature a 
  = TypeSignature { _tsName :: Ann Name a
                  , _tsType :: Ann Type a
                  }     
    
-- | Open type and data families
data TypeFamily a
  = TypeFamily { _tfHead :: Ann DeclHead a
               , _tfKind :: AnnMaybe KindConstraint a
               } -- ^ A type family declaration (@ type family A _a :: * -> * @)    
 | DataFamily { _tfHead :: Ann DeclHead a
              , _tfKind :: AnnMaybe KindConstraint a
              } -- ^ Data family declaration
                  
-- | A fixity signature (@ infixl 5 +, - @).
data FixitySignature a 
  = FixitySignature { _fixityAssoc :: Ann Assoc a
                    , _fixityPrecedence :: Ann Precedence a
                    , _fixityOperators :: AnnList Name a
                    }
       
-- | The list of declarations that can appear in a typeclass
data ClassBody a
  = ClassBody { _cbElements :: AnnList ClassElement a 
              }
              
-- | A list of GADT declarations with the @where@ keyword
data GadtDeclList a 
  = GadtDeclList { _gadtList :: AnnList GadtDecl a 
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
       
-- The declared (possibly parameterized) type (@ A x :+: B y @).
data DeclHead a
  = DeclHead { _dhName :: Ann Name a 
             } -- ^ Type or class name
  | DHParen  { _dhBody :: Ann DeclHead a 
             } -- ^ Parenthesized type
  | DHApp    { _dhAppFun :: Ann DeclHead a
             , _dhAppOperand :: Ann TyVar a
             } -- ^ Type application
  | DHInfix  { _dhInfixName :: Ann Name a 
             , _dhInfixLeft :: Ann TyVar a
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
                         , _instBodyGadtCons :: AnnList GadtDecl a
                         , _instBodyDerivings :: AnnMaybe Deriving a
                         } -- ^ An associated data type implemented using GADT style

-- | GADT constructor declaration (@ _D1 :: { _val :: Int } -> T String @)
data GadtDecl a
  = GadtDecl { _gdName :: Ann Name a
             , _gdFields :: AnnList FieldDecl a
             , _gdResType :: Ann Type a
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
  | InfixConDecl { _icdName :: Ann Name a
                 , _icdLhs :: Ann Type a
                 , _icdRhs :: Ann Type a
                 } -- ^ infix data constructor (@ t1 :+: t2 @)
  
-- | Field declaration (@ _fld :: Int @)
data FieldDecl a
  = FieldDecl { _fieldNames :: AnnList Name a
              , _fieldType :: Ann Type a
              }
  
-- | A deriving clause following a data type declaration. (@ deriving Show @ or @ deriving (Show, Eq) @)
data Deriving a
  = DerivingOne { _oneDerived :: Ann InstanceRule a }
  | Derivings { _allDerived :: AnnList InstanceRule a }
  
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
  
-- | Kind constraint (@ :: * -> * @)
data KindConstraint a 
  = KindConstraint { _kindConstr :: Ann Kind a 
                   }

----------------------------------------------------
-- Types -------------------------------------------
----------------------------------------------------
      
-- | Type variable declaration
data TyVar a 
  = TyVarDecl { _tyVarName :: Ann Name a
              , _tyVarKind :: AnnMaybe KindConstraint a
              }

-- | Haskell types
data Type a
  = TyForall     { _typeBounded :: AnnList TyVar a
                 , _typeCtx :: AnnMaybe Context a
                 , _typeType :: Ann Type a
                 } -- ^ Forall types (@ forall x y . type @)
  | TyFun        { _typeParam :: Ann Type a
                 , _typeResult :: Ann Type a
                 } -- ^ Function types (@ a -> b @)
  | TyTuple      { _typeElements :: AnnList Type a
                 } -- ^ Tuple types (@ (a,b) @)
  | TyUnbTuple   { _typeElements :: AnnList Type a 
                 } -- ^ Unboxed tuple types (@ (#a,b#) @)
  | TyList       { _typeElement :: Ann Type a 
                 } -- ^ List type with special syntax (@ [a] @)
  | TyParArray   { _typeElement :: Ann Type a
                 } -- ^ Parallel array type (@ [:a:] @)
  | TyApp        { _typeCon :: Ann Type a
                 , _typeArg :: Ann Type a
                 } -- ^ Type application (@ F a @)
  | TyVar        { _typeName :: Ann Name a
                 } -- ^ type variable or constructor (@ a @)
  | TyParen      { _typeInner :: Ann Type a
                 } -- ^ type surrounded by parentheses (@ (T a) @)
  | TyInfix      { _typeLeft :: Ann Type a 
                 , _typeOperator :: Ann Name a
                 , _typeRight :: Ann Type a
                 } -- ^ Infix type constructor (@ (a <: b) @)
  | TyKinded     { _typeInner :: Ann Type a
                 , _typeKind :: Ann Kind a
                 } -- ^ Type with explicit kind signature (@ _a :: * @)
  | TyPromoted   { _tpPromoted :: Promoted a
                 } -- A promoted data type with @-XDataKinds@ (@ '3 @).
  | TySplice     { _tsSplice :: Splice a
                 } -- ^ a Template Haskell splice type (@ $(genType) @).
  | TyQuasiQuote { _typeQQ :: QuasiQuote a
                 } -- ^ a Template Haskell quasi-quote type (@ [quoter| ... ] @).
  | TyBang       { _typeInner :: Ann Type a
                 } -- ^ Strict type marked with "!".
  | TyUnpack     { _typeInner :: Ann Type a
                 } -- ^ Type marked with UNPACK pragma.
  | TyNumLit     { _typeNumLit :: Integer
                 } -- ^ A numeric type literal (as @4096@ in @ ArrPtr 4096 Word8 @) with @-XDataKinds@
  | TyStrLit     { _typeStrLit :: String
                 } -- ^ A textual type literal (as @"x"@ in @ _Get :: Label "x" @) with @-XDataKinds@
  | TyWildcard   -- ^ A wildcard type (@ _ @) with @-XPartialTypeSignatures@
  | TyNamedWildc { _typeWildcardName :: Ann Name a
                 } -- ^ A named wildcard type (@ _t @) with @-XPartialTypeSignatures@

-- | Haskell kinds
data Kind a
  = KindStar  -- ^ @*@, the kind of types
  | KindUnbox -- ^ @#@, the kind of unboxed types
  | KindFn    { _kindLeft :: Ann Kind a
              , _kindRight :: Ann Kind a
              } -- ^ @->@, the kind of type constructor
  | KindParen { _kindParen :: Ann Kind a
              } -- ^ A parenthesised kind
  | KindVar   { _kindVar :: Ann Name a
              } -- ^ kind variable (using @PolyKinds@ extension)
  | KindApp   { _kindAppFun :: Ann Kind a
              , _kindAppArg :: Ann Kind a 
              } -- ^ Kind application (@ k1 k2 @)
  | KindTuple { _kindTuple :: AnnList Kind a
              } -- ^ A promoted tuple (@ '(k1,k2,k3) @)
  | KindList  { _kindList :: AnnList Kind a
              } -- ^ A promoted list literal (@ '[k1,k2,k3] @)
  
-- One or more assertions
data Context a
  = ContextOne   { _contextAssertion :: Ann Assertion a
                 } -- ^ One assertion (@ C a => ... @)
  | ContextMulti { _contextAssertions :: AnnList Assertion a 
                 } -- ^ A set of assertions (@ (C1 a, C2 b) => ... @, but can be one: @ (C a) => ... @)

-- | A single assertion in the context
data Assertion a
  = ClassAssert { _assertClsName :: Ann Name a
                , _assertTypes :: AnnList Type a
                } -- ^ Class assertion (@Cls x@)
  | InfixAssert { _assertLhs :: Ann Type a
                , _assertOp :: Ann Name a
                , _assertRhs :: Ann Type a
                } -- ^ Infix class assertion, also contains type equations (@ a ~ X y @)
                 
-- | Haskell expressions
data Expr a
  = Var            { _exprName :: Ann Name a 
                   } -- ^ A variable or a data constructor (@ a @)
  | Lit            { _exprLit :: Ann Literal a
                   } -- ^ Primitive literal
  | InfixApp       { _exprLhs :: Ann Expr a
                   , _exprOperator :: Ann Name a
                   , _exprRhs :: Ann Expr a
                   } -- ^ Infix operator application (@ a + b @)
  | PrefixApp      { _exprOperator :: Ann Name a
                   , _exprRhs :: Ann Expr a
                   } -- ^ Prefix operator application (@ -x @)
  | App            { _exprFun :: Ann Expr a
                   , _exprArg :: Ann Expr a
                   } -- ^ Function application (@ f 4 @)
                   -- unary minus omitted
  | Lambda         { _exprBindings :: AnnList Pattern a -- ^ at least one
                   , _exprInner :: Ann Expr a
                   } -- ^ Lambda expression (@ \a b -> a + b @)
  | Let            { _exprFunBind :: AnnList LocalBind a -- ^ nonempty
                   , _exprInner :: Ann Expr a
                   } -- ^ Local binding (@ let x = 2; y = 3 in e x y @)
  | If             { _exprCond :: Ann Expr a
                   , _exprThen :: Ann Expr a
                   , _exprElse :: Ann Expr a
                   } -- ^ If expression (@ if a then b else c @)
  | MultiIf        { _exprIfAlts :: AnnList GuardedRhs a 
                   } -- ^ Multi way if expressions with @MultiWayIf@ extension (@ if | guard1 -> expr1; guard2 -> expr2 @)
  | Case           { _exprCase :: Ann Expr a
                   , _exprAlts :: AnnList Alt a
                   } -- ^ Pattern matching expression (@ case expr of pat1 -> expr1; pat2 -> expr2 @)
  | Do             { _doKind :: Ann DoKind a
                   , _exprStmts :: AnnList Stmt a
                   } -- ^ Do-notation expressions (@ do x <- act1; act2 @)
  | Tuple          { _tupleElems :: AnnList Expr a
                   } -- ^ Tuple expression (@ (e1, e2, e3) @)
  | UnboxedTuple   { _tupleElems :: AnnList Expr a 
                   } -- ^ Unboxed tuple expression (@ (# e1, e2, e3 #) @)
  | TupleSection   { _tupleSectionElems :: AnnList TupSecElem a
                   } -- ^ Tuple section, enabled with @TupleSections@ (@ (a,,b) @). One of the elements must be missing.
  | UnboxedTupSec  { _tupleSectionElems :: AnnList TupSecElem a 
                   }
  | List           { _listElems :: AnnList Expr a 
                   } -- ^ List expression: @[1,2,3]@
  | ParArray       { _listElems :: AnnList Expr a 
                   } -- ^ Parallel array expression: @[: 1,2,3 :]@
  | Paren          { _exprInner :: Ann Expr a 
                   }
  | LeftSection    { _exprLhs :: Ann Expr a
                   , _exprOperator :: Ann Name a
                   } -- ^ Left operator section: @(1+)@
  | RightSection   { _exprOperator :: Ann Name a
                   , _exprRhs :: Ann Expr a
                   } -- ^ Right operator section: @(+1)@
  | RecCon         { _exprRecName :: Ann Name a
                   , _exprRecFields :: AnnList FieldUpdate a
                   } -- ^ Record value construction: @Point { x = 3, y = -2 }@
  | RecUpdate      { _exprRecBase :: Ann Expr a
                   , _exprRecFields :: AnnList FieldUpdate a
                   } -- ^ Record value  update: @p1 { x = 3, y = -2 }@
  | Enum           { _enumFrom :: Ann Expr a
                   , _enumThen :: AnnMaybe Expr a
                   , _enumTo :: AnnMaybe Expr a
                   } -- ^ Enumeration expression (@ [1,3..10] @)
  | ParArrayEnum   { _parEnumFrom :: Ann Expr a
                   , _parEnumThen :: AnnMaybe Expr a
                   , _parEnumTo :: Ann Expr a
                   } -- ^ Parallel array enumeration (@ [: 1,3 .. 10 :] @)
  | ListComp       { _compExpr :: Ann Expr a
                   , _compBody :: AnnList ListCompBody a -- ^ Can only have 1 element without @ParallelListComp@
                   } -- ^ List comprehension (@ [ (x, y) | x <- xs | y <- ys ] @)
  | ParArrayComp   { _compExpr :: Ann Expr a
                   , _parCompBody :: AnnList ListCompBody a
                   } -- ^ Parallel array comprehensions @ [: (x, y) | x <- xs , y <- ys :] @ enabled by @ParallelArrays@
  | TypeSig        { _exprInner :: Ann Expr a
                   , _exprSig :: Ann Type a
                   } -- ^ Explicit type signature (@ _x :: Int @)
  | VarQuote       { _quotedName :: Ann Name a 
                   } -- ^ @'x@ for template haskell reifying of expressions
  | TypeQuote      { _quotedName :: Ann Name a 
                   } -- ^ @''T@ for template haskell reifying of types
  | BracketExpr    { _bracket :: Ann Bracket a 
                   } -- ^ Template haskell bracket expression
  | Splice         { _innerExpr :: Ann Splice a 
                   } -- ^ Template haskell splice expression, for example: @$(gen a)@ or @$x@
  | QuasiQuoteExpr { _exprQQ :: Ann QuasiQuote a 
                   } -- ^ template haskell quasi-quotation: @[$quoter|str]@
  | ExprPragma     { _exprPragma :: Ann ExprPragma a
                   }
  -- Arrows
  | Proc           { _procPattern :: Ann Pattern a
                   , _procExpr :: Ann Expr a
                   }
  | ArrowApp       { _exprLhs :: Ann Expr a
                   , _arrowAppl :: Ann ArrowAppl a
                   , _exprRhs :: Ann Expr a
                   }
  | LamCase        { _exprAlts :: AnnList Alt a
                   } -- ^ Lambda case ( @\case 0 -> 1; 1 -> 2@ )
  -- XML expressions omitted
        
data TupSecElem a
  = Present { _tupSecExpr :: Ann Expr a 
            } -- ^ An existing element in a tuple section
  | Missing -- ^ A missing element in a tuple section
        
-- | Normal monadic statements
data Stmt a
  = BindStmt { _stmtPattern :: Ann Pattern a
             , _stmtBounded :: Ann Expr a
             } -- ^ Binding statement (@ x <- action @)
  | ExprStmt { _stmtExpr :: Ann Expr a 
             } -- ^ Non-binding statement (@ action @)
  | LetStmt  { _stmtBinds :: AnnList LocalBind a 
             } -- ^ Let statement (@ let x = 3; y = 4 @)
  | RecStmt  { _stmtRecBinds :: AnnList Stmt a 
             } -- ^ A recursive binding group for arrows (@ rec b <- f a c; c <- f b a @)
        
-- | Body of a list comprehension: (@ | x <- [1..10] @)
data ListCompBody a
  = ListCompBody { _compStmts :: AnnList CompStmt a 
                 } 
         
-- | List comprehension statement
data CompStmt a
  = CompStmt   { _compStmt :: Ann Stmt a 
               } -- ^ Normal monadic statement of a list comprehension
  | ThenStmt   { _thenExpr :: Ann Expr a 
               , _byExpr :: AnnMaybe Expr a
               } -- ^ Then statements by @TransformListComp@ (@ then sortWith by (x + y) @)
  | GroupStmt  { _byExpr :: AnnMaybe Expr a
               , _usingExpr :: AnnMaybe Expr a
               } -- ^ Grouping statements by @TransformListComp@ (@ then group by (x + y) using groupWith @) 
                 -- Note: either byExpr or usingExpr must have a value
          
-- | Value binding for top-level and local bindings
data ValueBind a
  = SimpleBind { _valBindPat :: Ann Pattern a
               , _valBindRhs :: Ann Rhs a  
               , _valBindLocals :: AnnMaybe LocalBinds a
               } -- ^ Non-function binding (@ v = "12" @)  
  -- TODO: use one name for a function instead of names in each match
  | FunBind    { _funBindMatches :: AnnList Match a 
               } -- ^ Function binding (@ f 0 = 1; f x = x @). All matches must have the same name.

-- | Representation of patterns for pattern bindings
data Pattern a
  = VarPat        { _patternVar :: Ann Name a } -- ^ Pattern name binding
  | LitPat        { _patternLiteral :: Ann Literal a } -- ^ Literal pattern
  | InfixPat      { _patternLhs :: Ann Pattern a
                  , _patternOp :: Ann Name a
                  , _patternRhs :: Ann Pattern a
                  } -- ^ Infix constructor application pattern (@ a :+: b @)
  | AppPat        { _patternCon :: Ann Name a
                  , _patternArg :: Ann Pattern a
                  } -- ^ Constructor application pattern (@ Point x y @)
  | TuplePat      { _patternElems :: AnnList Pattern a
                  } -- ^ Tuple pattern (@ (x,y) @)
  | UnboxTuplePat { _patternElems :: AnnList Pattern a
                  } -- ^ Unboxed tuple pattern (@ (# x, y #) @)
  | ListPat       { _patternElems :: AnnList Pattern a 
                  } -- ^ List pattern (@ [1,2,a,x] @)
  | ParArrPat     { _patternElems :: AnnList Pattern a 
                  } -- ^ Parallel array pattern (@ [:1,2,a,x:] @)
  | ParenPat      { _patternInner :: Ann Pattern a 
                  } -- ^ Parenthesised patterns
  | RecPat        { _patternName :: Ann Name a
                  , _patternFields :: AnnList PatternField a
                  } -- ^ Record pattern (@ Point { x = 3, y } @)
  | AsPat         { _patternName :: Ann Name a
                  , _patternInner :: Ann Pattern a
                  } -- ^ As-pattern (explicit name binding) (@ ls\@(hd:_) @)
  | WildPat       -- ^ Wildcard pattern: (@ _ @)
  | IrrPat        { _patternInner :: Ann Pattern a 
                  } -- ^ Irrefutable pattern (@ ~(x:_) @)
  | BangPat       { _patternInner :: Ann Pattern a 
                  } -- ^ Bang pattern (@ !x @)
  | TypeSigPat    { _patternInner :: Ann Pattern a
                  , _patternType :: Ann Type a
                  } -- ^ Pattern with explicit type signature (@ __ :: Int @)
  | ViewPat       { _patternExpr :: Ann Expr a
                  , _patternInner :: Ann Pattern a
                  } -- ^ View pattern (@ f -> Just 1 @)
  -- regular list pattern omitted
  -- xml patterns omitted
  | SplicePat     { _patternSplice :: Ann Splice a 
                  }
  | QuasiQuotePat { _patQQ :: Ann QuasiQuote a 
                  }
                  
-- Field specification of a record pattern
data PatternField a 
  = NormalFieldPattern   { _fieldPatternName :: Ann Name a
                         , _fieldPattern :: Ann Pattern a
                         } -- ^ Named field pattern (@ p = Point 3 2 @)
  | FieldPunPattern      { _fieldPunName :: Ann Name a 
                         } -- ^ Named field pun (@ p @)
  | FieldWildcardPattern -- ^ Wildcard field pattern (@ .. @)
          
-- | A template haskell splice          
data Splice a
  = IdSplice    { _spliceId :: Ann Name a 
                } -- ^ A simple name splice
  | ParenSplice { _spliceExpr :: Ann Expr a
                }
  
-- | Template haskell quasi-quotation: @[quoter|str]@  
data QuasiQuote a 
  = QuasiQuote { _qqExprName :: Ann Name a
               , _qqExprBody :: Ann QQString a
               } 
        
-- | Template Haskell Quasi-quotation content
data QQString a
  = QQString { _qqString :: String 
             } 

-- | Clause of function (or value) binding   
data Match a
  = Match { _matchName :: Ann Name a
          , _matchArgs :: AnnList Pattern a
          , _matchType :: AnnMaybe Type a
          , _matchRhs :: Ann Rhs a
          , _matchBinds :: AnnMaybe LocalBinds a
          } 
    
-- | Clause of case expression          
data Alt a
  = Alt { _altPattern :: Ann Pattern a
        , _altRhs :: Ann CaseRhs a
        , _altBinds :: AnnMaybe LocalBinds a
        }

-- | Local bindings attached to a declaration (@ where x = 42 @)             
data LocalBinds a
  = LocalBinds { _localBinds :: AnnList LocalBind a 
               }
  
-- | Bindings that are enabled in local blocks (where or let).
data LocalBind a 
  = LocalValBind   { _localVal :: Ann ValueBind a 
                   }
  -- TODO: check that no other signature can be inside a local binding
  | LocalSignature { _localSig :: Ann TypeSignature a 
                   }
  | LocalFixity    { _localFixity :: Ann FixitySignature a
                   }
   
-- | Right hand side of a value binding (possible with guards): (@ = 3 @ or @ | x == 1 = 3; | otherwise = 4 @)
data Rhs a
  = UnguardedRhs { _rhsExpr :: Ann Expr a
                 }
  | GuardedRhss  { _rhsGuards :: AnnList GuardedRhs a
                 }
  
-- | Right hand side of a match (possible with guards): (@ = 3 @ or @ | x == 1 = 3; | otherwise = 4 @)
data CaseRhs a
  = UnguardedCaseRhs { _rhsCaseExpr :: Ann Expr a 
                     }
  | GuardedCaseRhss  { _rhsCaseGuards :: AnnList GuardedCaseRhs a 
                     }
      
-- | A guarded right-hand side of a value binding (@ | x > 3 = 2 @)      
data GuardedRhs a
  = GuardedRhs { _guardStmts :: AnnList RhsGuard a -- ^ Cannot be empty.
               , _guardExpr :: Ann Expr a
               } 

-- | A guarded right-hand side of pattern matches binding (@ | x > 3 -> 2 @)      
data GuardedCaseRhs a
  = GuardedCaseRhs { _caseGuardStmts :: AnnList RhsGuard a -- ^ Cannot be empty.
                   , _caseGuardExpr :: Ann Expr a
                   } 
               
-- | Guards for value bindings and pattern matches (@ Just v <- x, v > 1 @)
data RhsGuard a
  = GuardBind  { _guardPat :: Ann Pattern a
               , _guardRhs :: Ann Expr a
               }
  | GuardLet   { _guardBinds :: AnnList LocalBind a 
               }
  | GuardCheck { _guardCheck :: Ann Expr a 
               }
               
-- Field update expressions
data FieldUpdate a 
  = NormalFieldUpdate { _fieldName :: Ann Name a
                      , _fieldValue :: Ann Expr a
                      } -- ^ Update of a field (@ x = 1 @)
  | FieldPun          { _fieldUpdateName :: Ann Name a 
                      } -- ^ Update the field to the value of the same name (@ x @)
  | FieldWildcard     -- ^ Update the fields of the bounded names to their values (@ .. @). Must be the last update. Cannot be used in a record update expression.
               
-- | Template Haskell bracket expressions
data Bracket a
  = ExprBracket    { _bracketExpr :: Ann Expr a 
                   } -- ^ Expression bracket (@ [| x + y |] @)
  | PatternBracket { _bracketPattern :: Ann Pattern a 
                   } -- ^ Pattern bracket (@ [| Point x y |] @)
  | TypeBracket    { _bracketType :: Ann Type a 
                   } -- ^ Pattern bracket (@ [| (Int,Int) |] @)
  | DeclBracket    { _bracketDecl :: Ann Decl a 
                   } -- ^ Declaration bracket (@ [| _f :: Int -> Int; f x = x*x |] @)
                  
-- * Pragmas

-- | Top level pragmas
data TopLevelPragma a
  = RulePragma    { _pragmaRule :: AnnList Rule a 
                  }
  | DeprPragma    { _pragmaObjects :: AnnList Name a
                  , _pragmaMessage :: Ann StringNode a
                  }
  | WarningPragma { _pragmaObjects :: AnnList Name a
                  , _pragmaMessage :: Ann StringNode a
                  }
  | AnnPragma     { _pragmaAnnotation :: Ann Annotation a 
                  }
  | MinimalPragma { _pragmaFormula :: AnnMaybe MinimalFormula a 
                  }
 
-- | A rewrite rule (@ "map/map" forall f g xs. map f (map g xs) = map (f.g) xs @)
data Rule a
  = Rule { _ruleName :: Ann StringNode a -- ^ User name of the rule
         , _rulePhase :: AnnMaybe PhaseControl a
         , _ruleBounded :: AnnList Name a
         , _ruleTopLevel :: Ann Name a
         , _ruleApplied :: AnnList Expr a
         , _ruleRhs :: Ann Expr a
         }
 
-- | Annotation allows you to connect an expression to any declaration. 
data Annotation a
  = NameAnnotation   { _annotateType :: AnnMaybe TypeKeyword a
                     , _annotateName :: Ann Name a
                     , _annotateExpr :: Ann Expr a
                     }
  | ModuleAnnotation { _annotateExpr :: Ann Expr a 
                     }

-- | Formulas of minimal annotations declaring which functions should be defined.
data MinimalFormula a
  = MinimalName  { _minimalName :: Ann Name a 
                 }
  | MinimalParen { _minimalInner :: Ann MinimalFormula a 
                 }
  | MinimalOr    { _minimalLhs :: Ann MinimalFormula a
                 , _minimalRhs :: Ann MinimalFormula a
                 } -- ^ One of the minimal formulas are needed (@ min1 | min2 @)
  | MinimalAnd   { _minimalLhs :: Ann MinimalFormula a
                 , _minimalRhs :: Ann MinimalFormula a
                 } -- ^ Both of the minimal formulas are needed (@ min1 , min2 @)
         
-- | Pragmas that can be applied to expressions
data ExprPragma a
  = CorePragma      { _pragmaStr :: Ann StringNode a 
                    }
  | SccPragma       { _pragmaStr :: Ann StringNode a 
                    }
  | GeneratedPragma { _pragmaSrcRange :: Ann SourceRange a 
                    }
                  
data SourceRange a
  = SourceRange { _srFileName :: Ann StringNode a
                , _srFromLine :: Ann Number a
                , _srFromCol :: Ann Number a
                , _srToLine :: Ann Number a
                , _srToCol :: Ann Number a
                }
  
data Number a 
  = Number { _numberInteger :: Integer 
           }
                  