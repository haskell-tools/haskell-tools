
module Language.Haskell.Tools.AST.Decl2 where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Literals


data Decl a wt
  = TypeDecl { declHead :: IdType wt (DeclHead wt) a
             , declType :: IdType wt (Type wt) a
             } -- ^ A type synonym ( @type String = [Char]@ )
{-  | TypeFamilyDecl { declHead :: Ann DeclHead a
                   , declKind :: AnnMaybe KindConstraint a
                   } -- ^ A type family declaration
  | ClosedTypeFamilyDecl { declHead :: Ann DeclHead a
                         , declKind :: AnnMaybe KindConstraint a
                         , declDecl :: AnnList TypeEqn a -- ^ cannot be empty
                         } -- ^ A closed type family declaration
  | DataDecl { declNewtype :: Ann DataOrNewtypeKeyword a
             , declCtx  :: AnnMaybe Context a
             , declHead :: Ann DeclHead a
             , declCons :: AnnList ConDecl a
             , declDeriving :: AnnMaybe Deriving a
             } -- ^ A data or newtype declaration.
  | GDataDecl { declNewtype :: Ann DataOrNewtypeKeyword a
              , declCtx  :: AnnMaybe Context a
              , declHead :: Ann DeclHead a
              , declKind :: AnnMaybe KindConstraint a
              , declGadt :: Ann GadtDeclList a
              , declDeriving :: AnnMaybe Deriving a
              } -- ^ A data or newtype declaration.
  | DataFamilyDecl { declCtx  :: AnnMaybe Context a
                   , declHead :: Ann DeclHead a
                   , declKind :: AnnMaybe KindConstraint a
                   } -- ^ Data family declaration
  | TypeInstDecl { declInstance :: Ann Type a
                 , declAssignedType :: Ann Type a
                 } -- ^ Type instance declaration (@ type instance Fam T = AssignedT @)
  | DataInstDecl { declNewtype :: Ann DataOrNewtypeKeyword a
                 , declInstance :: Ann Type a
                 , declCons :: AnnList ConDecl a
                 } -- ^ Data instance declaration (@ data instance Fam T = Con1 | Con2 @)
  | GDataInstDecl { declNewtype :: Ann DataOrNewtypeKeyword a
                  , declInstance :: Ann Type a
                  , declKind :: AnnMaybe KindConstraint a
                  , declGadt :: Ann GadtDeclList a
                  } -- ^ Data instance declaration (@ data instance T = Con1 | Con2 @)
  | ClassDecl { declCtx :: AnnMaybe Context a
              , declHead :: Ann DeclHead a
              , declFunDeps :: AnnMaybe FunDeps a
              , declBody :: AnnMaybe ClassBody a
              } -- ^ Type class declaration (@ class X a [where f = ...] @)
  | InstDecl { declOverlap :: AnnMaybe OverlapPragma a
             , declInstRule :: Ann InstanceRule a
             , declInstDecl :: AnnMaybe InstBody a
             } -- ^ Instance declaration (@ instance X T [where f = ...] @)
  | DerivDecl { declOverlap :: AnnMaybe OverlapPragma a
              , declInstRule :: Ann InstanceRule a
              } -- ^ Standalone deriving declaration (@ deriving instance X T @)
  | FixityDecl { declAssoc :: Ann Assoc a
               , declPrecedence :: Ann Precedence a
               , declOperators :: AnnList Name a
               } -- ^ Fixity declaration (@ infixl 5 +, - @)
  | DefaultDecl { declTypes :: AnnList Type a
                , declInfo :: a
                } -- ^ Default types (@ default (T1, T2) @)
  | TypeSignature { declName :: Ann Name a
                  , declType :: Ann Type a
                  } -- ^ Type signature (@ f :: Int -> Int @)
  | FunBinding { declFunBind :: Ann FunBind a } -- ^ Function binding (@ f x = 12 @)
  | ForeignImport { declCallConv :: Ann CallConv a
                  , declSafety :: AnnMaybe Safety a
                  , declName :: Ann Name a
                  , declType :: Ann Type a
                  } -- ^ Foreign import (@ foreign import foo :: Int -> IO Int @)
  | ForeignExport { declCallConv :: Ann CallConv a
                  , declName :: Ann Name a
                  , declType :: Ann Type a
                  } -- ^ foreign export (@ foreign export ccall foo :: Int -> IO Int @)
  | Pragma { declPragma :: TopLevelPragma a } -- ^ top level pragmas
  | SpliceDecl { declExpr :: Ann Expr a } -- ^ A Template Haskell splice declaration (@ $(generateDecls) @)
       
-- | The list of declarations that can appear in a typeclass
data ClassBody a
  = ClassBody { cbElements :: AnnList ClassElement a }
              
-- | A list of GADT declarations with the @where@ keyword
data GadtDeclList a 
  = GadtDeclList { gadtList :: AnnList GadtDecl a } 
                 
-- | Members of a class declaration       
data ClassElement a
  = ClsDecl { ceDecl :: Ann Decl a } -- ^ Ordinary declaration: @ f :: A -> B @
  | ClsDataFam { ceCtx :: AnnMaybe Context a
               , ceHead :: Ann DeclHead a
               , ceKind :: AnnMaybe Kind a
               } -- ^ Declaration of an associated data type: @ data T x :: * @ 
  | ClsTypeFam { ceHead :: Ann DeclHead a
               , ceKind :: AnnMaybe Kind a
               } -- ^ Declaration of an associated type synonym: @ type T x :: * @ 
  | ClsTypeDef { ceHead :: Ann DeclHead a
               , ceKind :: AnnMaybe Kind a
               } -- ^ Default choice for type synonym: @ type T x = TE @ or @ type instance T x = TE @ 
  | ClsDefSig { ceName :: Ann Name a
              , ceType :: Ann Type a
              } -- ^ Default signature (by using @DefaultSignatures@): @ default enum :: (Generic a, GEnum (Rep a)) => [a] @
  -}     
-- The declared (possibly parameterized) type (@ A x :+: B y @).
data DeclHead wt a
  = DeclHead { dhName :: Name a } -- ^ Type or class name
  | DHParen  { dhBody :: DeclHead wt a } -- ^ Parenthesized type
  | DHApp    { dhAppFun :: IdType wt (DeclHead wt) a
             , dhAppOperand :: IdType wt (TyVar wt) a
             } -- ^ Type application
  | DHInfix  { dhInfixName :: IdType wt Name a 
             , dhInfixLeft :: IdType wt (TyVar wt) a
             } -- ^ Infix application of the type/class name to the left operand
       
-- | Instance body is the implementation of the class functions (@ where a x = 1; b x = 2 @)
{-data InstBody a
  = InstBody { instBodyDecls :: AnnList InstBodyDecl a }
-}
-- | Declarations inside an instance declaration.
{-data InstBodyDecl a
  = InstBodyNormalDecl { instBodyDeclFunbind :: FunBind a } -- ^ A normal declaration (@ f x = 12 @)
  | InstBodyTypeDecl { instBodyLhsType :: Ann Type a
                     , instBodyRhsType :: Ann Type a
                     } -- ^ An associated type definition (@ type A X = B @)
  | InstBodyDataDecl { instBodyDataNew :: Ann DataOrNewtypeKeyword a
                     , instBodyLhsType :: Ann Type a
                     , instBodyDataCons :: AnnList ConDecl a
                     , instBodyDerivings :: AnnMaybe Deriving a
                     } -- ^ An associated data type implementation (@ data A X = C1 | C2 @)
  | InstBodyGadtDataDecl { instBodyDataNew :: Ann DataOrNewtypeKeyword a
                         , instBodyLhsType :: Ann Type a
                         , instBodyDataKind :: AnnMaybe Kind a
                         , instBodyGadtCons :: AnnList GadtDecl a
                         , instBodyDerivings :: AnnMaybe Deriving a
                         } -- ^ An associated data type implemented using GADT style
-}
-- | GADT constructor declaration (@ D1 :: { val :: Int } -> T String @)
{-data GadtDecl a
  = GadtDecl { gdName :: Ann Name a
             , gdFields :: AnnList FieldDecl a
             , gdResType :: Ann Type a
             }
             
data GadtField a
  = GadtNormalField { gadtFieldType :: Ann Type a }
  | GadtNamedField { gadtFieldName :: Ann Name a
                   , gadtFieldType :: Ann Type a
                   } -- ^ Named GADT field (@ { val :: Int } @)
         
-- | A list of functional dependencies: @ | a -> b, c -> d @ separated by commas  
data FunDeps a
  = FunDeps { funDeps :: AnnList FunDep a } 
         
-- | A functional dependency, given on the form @l1 ... ln -> r1 ... rn@         
data FunDep a
  = FunDep { funDepLhs :: AnnList Name a
           , funDepRhs :: AnnList Name a
           }
  
data ConDecl a
  = ConDecl { conDeclName :: Ann Name a
            , conDeclArgs :: AnnList Type a
            } -- ^ ordinary data constructor (@ C t1 t2 @)
  | RecordDecl { conDeclName :: Ann Name a
               , conDeclFields :: AnnList FieldDecl a
               } -- ^ record data constructor (@ C { n1 :: t1, n2 :: t2 } @)
  | InfixConDecl { icdName :: Ann Name a
                 , icdLhs :: Ann Type a
                 , icdRhs :: Ann Type a
                 } -- ^ infix data constructor (@ t1 :+: t2 @)
  
data FieldDecl a
  = FieldDecl { fieldNames :: AnnList Name a
              , fieldType :: Ann Type a
              }
  
-- | A deriving clause following a data type declaration. (@ deriving Show @ or @ deriving (Show, Eq) @)
data Deriving a
  = DerivingOne { oneDerived :: Ann InstanceRule a }
  | Derivings { allDerived :: AnnList InstanceRule a }
  
-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the where keyword.
data InstanceRule a
  = InstanceRule { irVars :: AnnMaybe (AnnList TyVar) a
                 , irCtx :: AnnMaybe Context a
                 , irHead :: Ann InstanceHead a
                 }
  | InstanceParen { irRule :: Ann InstanceRule a }

data InstanceHead a
  = InstanceHeadCon { ihConName :: Ann Name a } -- ^ Type or class name
  | InstanceHeadInfix { ihLeftOp :: Ann Type a
                      , ihOperator :: Ann Name a
                      } -- ^ Infix application of the type/class name to the left operand
  | InstanceHeadParen { ihHead :: Ann InstanceHead a } -- ^ Parenthesized instance head
  | InstanceHeadApp { ihFun :: Ann InstanceHead a
                    , ihType :: Ann Type a
                    } -- ^ Application to one more type
        
data TypeEqn a
  = TypeEqn { teLhs :: Ann Type a
            , teRhs :: Ann Type a
            } -- ^ Type equations as found in closed type families (@ T A = S @)
-}  
data KindConstraint wt a 
  = KindConstraint { kindConstr :: IdType wt (Kind wt) a } -- ^ Kind constraint (@ :: * -> * @)

----------------------------------------------------
-- Types -------------------------------------------
----------------------------------------------------
   
-- | Type variable declaration
data TyVar wt a
  = TyVarDecl { tyVarName :: IdType wt Name a
              , tyVarKind :: MaybeType wt (KindConstraint wt) a
              }
         
data Type wt a
  = TyForall { typeBounded :: MaybeType wt (TyVar wt) a
--             , typeCtx :: AnnMaybe Context a
             , typeType :: IdType wt (Type wt) a
             } -- ^ Forall types (@ forall x y . type @)
  | TyFun { typeParam :: IdType wt (Type wt) a
          , typeResult :: IdType wt (Type wt) a
          } -- ^ Function types (@ a -> b @)
--  | TyTuple { typeElements :: AnnList Type a } -- ^ Tuple types (@ (a,b) @)
--  | TyUnbTuple { typeElements :: AnnList Type a } -- ^ Unboxed tuple types (@ (#a,b#) @)
--  | TyList { typeElement :: Ann Type a } -- ^ List type with special syntax (@ [a] @)
--  | TyParArray { typeElement :: Ann Type a } -- ^ Parallel array type (@ [:a:] @)
--  | TyApp { typeCon :: Ann Type a
--          , typeArg :: Ann Type a
--          } -- ^ Type application (@ F a @)
  | TyVar { typeName :: IdType wt Name a } -- ^ type variable (@ a @)
--  | TyCon { typeName :: Ann Name a } -- ^ type constructor (@ T @)
--  | TyParen { typeInner :: Ann Type a } -- ^ type surrounded by parentheses (@ (T a) @)
--  | TyInfix { typeLeft :: Ann Type a 
--            , typeOperator :: Ann Name a
--            , typeRight :: Ann Type a
--            } -- ^ Infix type constructor (@ (a <: b) @)
--  | TyKinded { typeInner :: Ann Type a
--             , typeKind :: Ann Kind a
--             } -- ^ Type with explicit kind signature (@ a :: * @)
--  | TyPromoted { tpPromoted :: Promoted a } -- A promoted data type with @-XDataKinds@ (@ '3 @).
--  | TySplice { tsSplice :: Splice a } -- ^ a Template Haskell splice type (@ $(genType) @).
--  | TyBang { typeInner :: Ann Type a } -- ^ Strict type marked with "!".
--  | TyUnpack { typeInner :: Ann Type a } -- ^ Type marked with UNPACK pragma.

data Kind wt a
  = KindStar -- ^ @*@, the kind of types
  | KindUnbox -- ^ @#@, the kind of unboxed types
  | KindFn { kindLeft :: IdType wt (Kind wt) a
           , kindRight :: IdType wt (Kind wt) a
           } -- ^ @->@, the kind of type constructor
  | KindParen { kindParen :: IdType wt (Kind wt) a } -- ^ A parenthesised kind
  | KindVar { kindVar :: IdType wt Name a } -- ^ kind variable (using @PolyKinds@ extension)
  | KindApp { kindAppFun :: IdType wt (Kind wt) a
            , kindAppArg :: IdType wt (Kind wt) a 
            } -- ^ Kind application (@ k1 k2 @)
  | KindTuple { kindTuple :: ListType wt (Kind wt) a } -- ^ A promoted tuple (@ '(k1,k2,k3) @)
  | KindList { kindList :: ListType wt (Kind wt) a } -- ^ A promoted list literal (@ '[k1,k2,k3] @)
{-  
-- One or more assertions
data Context a
  = ContextOne { contextAssertion :: Assertion a } -- ^ One assertion (@ C a => ... @)
  | ContextMulti { contextAssertions :: AnnList Assertion a } 
      -- ^ A set of assertions (@ (C1 a, C2 b) => ... @, but can be one: @ (C a) => ... @)

-- | A single assertion in the context
data Assertion a
  = ClassAssert { assertClsName :: Ann Name a
                , assertTypes :: AnnList Type a
                } -- ^ Class assertion (@Cls x@)
  | AppAssert { assertConstrName :: Ann Name a
              , assertTypes :: AnnList Type a
              } -- ^ Class assertion application
  | InfixAssert { assertLhs :: Ann Type a
                , assertOp :: Ann Name a
                , assertRhs :: Ann Type a
                } -- ^ Infix class assertion, also contains type equations (@ a ~ X y @)
  | ParenAssert { assertInner :: Ann Assertion a } -- ^ Parenthesised class assertion
                 
-- | Haskell expressions
data Expr a
  = Var { exprName :: Ann Name a } -- ^ A variable (@ a @)
  | Con { exprName :: Ann Name a } -- ^ Data constructor (@Point@ in @Point 1 2@)
  | Lit { exprLit :: Ann Literal a } -- ^ Primitive literal
  | InfixApp { exprLhs :: Ann Expr a
             , exprOperator :: Ann Name a
             , exprRhs :: Ann Expr a
             } -- ^ Infix operator application (@ a + b @)
  | App { exprFun :: Ann Expr a
        , exprArg :: Ann Expr a
        } -- ^ Function application (@ f 4 @)
  -- unary minus omitted
  | Lambda { exprBindings :: AnnList Pattern a -- ^ at least one
           , exprInner :: Ann Expr a
           } -- ^ Lambda expression (@ \a b -> a + b @)
  | Let { exprFunBind :: AnnList FunBind a -- ^ nonempty
        , exprInner :: Ann Expr a
        } -- ^ Local binding (@ let x = 2; y = 3 in e x y @)
  | If { exprCond :: Ann Expr a
       , exprThen :: Ann Expr a
       , exprElse :: Ann Expr a
       } -- ^ If expression (@ if a then b else c @)
  | MultiIf { exprIfAlts :: AnnList GuardedRhs a }
    -- ^ Multi way if expressions with @MultiWayIf@ extension (@ if | guard1 -> expr1; guard2 -> expr2 @)
  | Case { exprCase :: Ann Expr a
         , exprAlts :: AnnList Alt a
         } -- ^ Pattern matching expression (@ case expr of pat1 -> expr1; pat2 -> expr2 @)
  | Do { doKind :: Ann DoKind a
       , exprStmts :: AnnList Stmt a
       } -- ^ Do-notation expressions (@ do x <- act1; act2 @)
  | Tuple { tupleElems :: AnnList Expr a } -- ^ Tuple expression (@ (e1, e2, e3) @)
  | UnboxedTuple { tupleElems :: AnnList Expr a } -- ^ Unboxed tuple expression (@ (# e1, e2, e3 #) @)
  | TupleSection { tupleSectionElems :: AnnList (AnnMaybe Expr) a }
    -- ^ Tuple section, enabled with @TupleSections@ (@ (a,,b) @)
  | BoxedTupleSection { tupleSectionElems :: AnnList (AnnMaybe Expr) a }
  | List { listElems :: AnnList Expr a } -- ^ List expression: @[1,2,3]@
  | ParArray { listElems :: AnnList Expr a } -- ^ Parallel array expression: @[: 1,2,3 :]@
  | Paren { exprInner :: Ann Expr a }
  | LeftSection { exprLhs :: Ann Expr a
                , exprOperator :: Ann Name a
                } -- ^ Left operator section: @(1+)@
  | RightSection { exprOperator :: Ann Name a
                 , exprRhs :: Ann Expr a
                 } -- ^ Right operator section: @(+1)@
  | RecExpr { exprRecName :: Ann Expr a
            , exprRecFields :: AnnList FieldUpdate a
            } -- ^ Record value construction or update: @p1 { x = 3, y = -2 }@
  | Enum { enumFrom :: Ann Expr a
         , enumThen :: AnnMaybe Expr a
         , enumTo :: AnnMaybe Expr a
         , exprInfo :: a
         } -- ^ Enumeration expression (@ [1,3..10] @)
  | ParArrayEnum { parEnumFrom :: Ann Expr a
                 , parEnumThen :: AnnMaybe Expr a
                 , parEnumTo :: Ann Expr a
                 } -- ^ Parallel array enumeration (@ [: 1,3 .. 10 :] @)
  | ListComp { compExpr :: Ann Expr a
             , compBody :: AnnList CompStmt a
             } -- ^ List comprehension (@  @)
  | ParListComp { compExpr :: Ann Expr a
                , parCompBody :: AnnList (AnnList CompStmt) a
                } -- ^ Parallel list comprehension: @ [ (x, y) | x <- xs | y <- ys ] @
  | ParArrayComp { compExpr :: Ann Expr a
                 , parCompBody :: AnnList (AnnList CompStmt) a
                 } -- ^ List comprehension  
  | TypeSig { exprInner :: Ann Expr a
            , exprSig :: Ann Type a
            } -- ^ Explicit type signature (@ x :: Int @)
  -- Template Haskell
  | VarQuote { quotedName :: Name a } -- ^ @'x@ for template haskell reifying of expressions
  | TypeQuote { quotedName :: Name a } -- ^ @''T@ for template haskell reifying of types
  | BracketExpr { bracket :: Bracket a } -- ^ Template haskell bracket expression
  | Splice { innerExpr :: Ann Expr a } -- ^ Template haskell splice expression, for example: @$(gen a)@ or @$x@
  | QuasiQuote { qqExprName :: Ann Name a
               , qqExprBody :: Ann QQString a
               } -- ^ template haskell quasi-quotation: @[$quoter|str]@
  | ExprPragma { exprPragma :: ExprPragma a }
  -- Arrows
  | Proc { procPattern :: Ann Pattern a
         , procExpr :: Ann Expr a
         }
  | ArrowApp { exprLhs :: Ann Expr a
             , arrowAppl :: Ann ArrowAppl a
             , exprRhs :: Ann Expr a
             }
  | LamCase { exprAlts :: AnnList Alt a } -- ^ Lambda case ( @\case 0 -> 1; 1 -> 2@ )
  -- XML expressions omitted
          
data Stmt a
  = BindStmt { stmtPattern :: Ann Pattern a
             , stmtBounded :: Ann Expr a
             } -- ^ Binding statement (@ x <- action @)
  | ExprStmt { stmtExpr :: Expr a } -- ^ Non-binding statement (@ action @)
  | LetStmt  { stmtBinds :: Ann Binds a } -- ^ Let statement (@ let x = 3; y = 4 @)
  | RecStmt  { stmtRecBinds :: AnnList Stmt a } -- ^ A recursive binding group for arrows (@ rec b <- f a c; c <- f b a @)
         
-- | List comprehension statement
data CompStmt a
  = CompStmt   { compStmt :: Ann Stmt a } -- ^ Normal monadic statement of a list comprehension
  | ThenStmt   { thenExpr :: Ann Expr a 
               , byExpr :: AnnMaybe Expr a
               } -- ^ Then statements by @TransformListComp@ (@ then sortWith by (x + y) @)
  | GroupStmt  { byExpr :: AnnMaybe Expr a
               , usingExpr :: AnnMaybe Expr a
               } -- ^ Grouping statements by @TransformListComp@ (@ then group by (x + y) using groupWith @) 
                 -- Note: byExpr or usingExpr must have a value
          
-- | Function binding for top-level and local bindings
data FunBind a
  = FunBind { funBindMatches :: AnnList Match a }                    

-- | Representation of patterns for pattern bindings
data Pattern a
  = VarPat { patternVar :: Name a } -- ^ Pattern name binding
  | LitPat { patternLiteral :: Literal a } -- ^ Literal pattern
  | InfixPat { patternLhs :: Ann Pattern a
             , patternOp :: Ann Name a
             , patternRhs :: Ann Pattern a
             } -- ^ Infix constructor application pattern (@ a :+: b @)
  | AppPat { patternCon :: Ann Name a
           , patternArg :: Ann Pattern a
           } -- ^ Constructor application pattern (@ Point x y @)
  | TuplePat { patternElems :: AnnList Pattern a } -- ^ Tuple pattern (@ (x,y) @)
  | UnboxTuplePat { patternElems :: AnnList Pattern a } -- ^ Unboxed tuple pattern (@ (# x, y #) @)
  | ListPat { patternElems :: AnnList Pattern a } -- ^ List pattern (@ [1,2,a,x] @)
  | ParenPat { patternInner :: Ann Pattern a } -- ^ Parenthesised patterns
  | RecPat { patternName :: Ann Name a
           , patternFields :: AnnList PatternField a
           } -- ^ Record pattern (@ Point { x = 3, y } @)
  | AsPat { patternName :: Ann Name a
          , patternInner :: Ann Pattern a
          } -- ^ As-pattern (explicit name binding) (@ ls\@(hd:_) @)
  | WildPat { patternInfo :: a } -- ^ Wildcard pattern: (@ _ @)
  | IrrPat { patternInner :: Ann Pattern a } -- ^ Irrefutable pattern (@ ~(x:_) @)
  | BangPat { patternInner :: Ann Pattern a } -- ^ Bang pattern (@ !x @)
  | TypeSigPat { patternInner :: Ann Pattern a
               , patternType :: Ann Type a
               } -- ^ Pattern with explicit type signature (@ _ :: Int @)
  | ViewPat { patternExpr :: Ann Expr a
            , patternInner :: Ann Pattern a
            } -- ^ View pattern (@ f -> Just 1 @)
  -- regular list pattern omitted
  -- xml patterns omitted
  | QuasiQuotePat { qqPatternName :: Ann Name a
                  , qqPatternBody :: Ann QQString a
                  }
                  
-- Field specification of a record pattern
data PatternField a 
  = NormalFieldPattern { fieldPatternName :: Ann Name a
                       , fieldPattern :: Ann Pattern a
                       } -- ^ Named field pattern (@ p = Point 3 2 @)
  | FieldPunPattern { fieldPunName :: Name a } -- ^ Named field pun (@ p @)
  | FieldWildcardPattern -- ^ Wildcard field pattern (@ .. @)
          
-- | A template haskell splice          
data Splice a
  = IdSplice { spliceId :: Ann Name a } -- ^ A simple name splice
  | ParenSplice { spliceExpr :: Ann Expr a }
        
-- | Template Haskell Quasi-Quotation        
data QQString a
  = QQString { qqString :: String } 

-- | Clause of function binding   
data Match a
  = Match { matchName :: Ann Name a
          , matchArgs :: AnnList Pattern a
          , matchType :: AnnMaybe Type a
          , matchRhs :: Ann Rhs a
          , matchBinds :: AnnMaybe Binds a
          } 
    
-- | Clause of case expression          
data Alt a
  = Alt { altPattern :: Ann Pattern a
        , altRhs :: Ann Rhs a
        , altBinds :: AnnMaybe Binds a
        }

-- | Local bindings attached to a declaration (@ where x = 42 @)             
data Binds a
  = DeclBindings { bindingDecls :: AnnList Decl a }
   
data Rhs a
  = UnguardedRhs { rhsExpr :: Expr a }
  | GuardedRhss { rhsGuards :: AnnList GuardedRhs a }
               
data GuardedRhs a
  = GuardedRhs { guardStmts :: AnnList Stmt a
               , guardExpr :: Ann Expr a
               } 
               
data FieldUpdate a 
  = NormalFieldUpdate { fieldName :: Ann Name a
                      , fieldValue :: Ann Expr a
                      } -- ^ Update of a field (@ x = 1 @)
  | FieldPun { fieldUpdateName :: Name a } -- ^ Update the field to the value of the same name (@ x @)
  | FieldWildcard -- ^ Update the fields of the bounded names to their values (@ .. @)
               
-- | Template Haskell bracket expressions
data Bracket a
  = ExprBracket { bracketExpr :: Ann Expr a } -- ^ Expression bracket (@ [| x + y |] @)
  | PatternBracket { bracketPattern :: Ann Pattern a } -- ^ Pattern bracket (@ [| Point x y |] @)
  | TypeBracket { bracketType :: Ann Type a } -- ^ Pattern bracket (@ [| (Int,Int) |] @)
  | DeclBracket { bracketDecl :: Ann Decl a } -- ^ Declaration bracket (@ [| f :: Int -> Int; f x = x*x |] @)
                  
-- * Pragmas

-- | Top level pragmas
data TopLevelPragma a
  = RulePragma { pragmaRule :: AnnList Rule a }
  | DeprPragma { pragmaObjects :: AnnList Name a
               , pragmaMessage :: Ann StringNode a
               }
  | WarningPragma { pragmaObjects :: AnnList Name a
                  , pragmaMessage :: Ann StringNode a
                  }
  | AnnPragma { pragmaAnnotation :: Annotation a }
  | MinimalPragma { pragmaFormula :: AnnMaybe MinimalFormula a }
 
-- | A rewrite rule (@ "map/map" forall f g xs. map f (map g xs) = map (f.g) xs @)
data Rule a
  = Rule { ruleName :: Ann StringNode a -- ^ User name of the rule
         , rulePhase :: AnnMaybe PhaseControl a
         , ruleBounded :: AnnList Name a
         , ruleTopLevel :: Ann Name a
         , ruleApplied :: AnnList Expr a
         , ruleRhs :: Ann Expr a
         }
 
-- | Annotation allows you to connect an expression to any declaration. 
data Annotation a
  = NameAnnotation { annotateType :: AnnMaybe TypeKeyword a
                   , annotateName :: Ann Name a
                   , annotateExpr :: Ann Expr a
                   }
  | ModuleAnnotation { annotateExpr :: Ann Expr a }
         
data MinimalFormula a
  = MinimalName { minimalName :: Name a }
  | MinimalParen { minimalInner :: Ann MinimalFormula a }
  | MinimalOr { minimalLhs :: Ann MinimalFormula a
              , minimalRhs :: Ann MinimalFormula a
              } -- ^ One of the minimal formulas are needed (@ min1 | min2 @)
  | MinimalAnd { minimalLhs :: Ann MinimalFormula a
               , minimalRhs :: Ann MinimalFormula a
               } -- ^ Both of the minimal formulas are needed (@ min1 , min2 @)
         
-- | Pragmas that can be applied to expressions
data ExprPragma a
  = CorePragma { pragmaStr :: Ann StringNode a }
  | SccPragma { pragmaStr :: Ann StringNode a }
  | GeneratedPragma { pragmaSrcRange :: Ann SourceRange a }
                  
data SourceRange a
  = SourceRange { srFileName :: Ann StringNode a
                , srFromLine :: Ann Number a
                , srFromCol :: Ann Number a
                , srToLine :: Ann Number a
                , srToCol :: Ann Number a
                }
  
data Number a = Number { numberInteger :: Integer }
-}  
