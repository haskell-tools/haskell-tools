module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.AST.Rewrite as G

main :: IO ()
main = defaultMain genTests

genTests :: TestTree
genTests = testGroup "ast generation tests" 
             [ testGroup "name tests" testBase
             , testGroup "expression tests" (map makeGenTest testExprs)
             , testGroup "pattern tests" (map makeGenTest testPatterns)
             , testGroup "type tests" (map makeGenTest testType) 
             , testGroup "binding tests" (map makeGenTest testBinds)
             , testGroup "declaration tests" (map makeGenTest testDecls)
             , testGroup "module tests" (map makeGenTest testModules)
             ]

makeGenTest :: SourceInfoTraversal elem => (String, String, Ann elem dom SrcTemplateStage) -> TestTree
makeGenTest (name, expected, ast) = testCase name $ assertEqual "The generated AST is not what is expected" expected (prettyPrint ast)

testBase
  = [ makeGenTest ("qualified name", "A.b", mkNormalName $ mkQualifiedName ["A"] "b")
    , makeGenTest ("qualified operator", "A.+", mkQualOp ["A"] "+")
    , makeGenTest ("backtick operator", "`mod`", mkBacktickOp [] "mod")
    , makeGenTest ("operator name", "(+)", mkParenName $ mkSimpleName "+")
    ]

testExprs 
  = [ ("infix", "a + 3", mkInfixApp (mkVar (mkName "a")) (mkUnqualOp "+") (mkLit $ mkIntLit 3)) 
    , ("section", "(\"xx\" ++)", mkLeftSection (mkLit (mkStringLit "xx")) (mkUnqualOp "++"))
    , ("tuple", "(1, [2, 3])", mkTuple [ mkLit (mkIntLit 1), mkList [ mkLit (mkIntLit 2), mkLit (mkIntLit 3) ] ])
    , ("record constructor", "P { x = 1 }", mkRecCon (mkName "P") [ mkFieldUpdate (mkName "x") (mkLit $ mkIntLit 1) ])
    , ("if", "if f a then x else y"
      , mkIf (mkApp (mkVar $ mkName "f") (mkVar $ mkName "a")) (mkVar $ mkName "x") (mkVar $ mkName "y"))
    , ("let", "let nat = [0..] in !z"
      , mkLet [mkLocalValBind $ mkSimpleBind' (mkName "nat") (mkEnum (mkLit (mkIntLit 0)) Nothing Nothing)] 
                                              (mkPrefixApp (mkUnqualOp "!") (mkVar $ mkName "z")) )
    , ("case",   "case x of Just y -> y\n"
              ++ "          Nothing -> 0"
      , mkCase (mkVar (mkName "x")) 
          [ mkAlt (mkAppPat (mkName "Just") [mkVarPat (mkName "y")]) (mkCaseRhs $ mkVar (mkName "y")) Nothing
          , mkAlt (mkVarPat $ mkName "Nothing") (mkCaseRhs $ mkLit $ mkIntLit 0) Nothing
          ])
    , ("multiway if",   "if | x > y -> x\n"
                     ++ "   | otherwise -> y"
      , mkMultiIf [ mkGuardedCaseRhs 
                     [ mkGuardCheck $ mkInfixApp (mkVar (mkName "x")) (mkUnqualOp ">") (mkVar (mkName "y"))] 
                     (mkVar (mkName "x"))
                  , mkGuardedCaseRhs [mkGuardCheck $ mkVar (mkName "otherwise")] (mkVar (mkName "y"))
                  ])
    , ("do notation",   "do x <- a\n"
                     ++ "   return x"
      , mkDoBlock [ G.mkBindStmt (mkVarPat (mkName "x")) (mkVar (mkName "a"))
                  , mkExprStmt (mkApp (mkVar $ mkName "return") (mkVar $ mkName "x"))
                  ])
    ]

testPatterns
  = [ ("irrefutable pattern", "~[0, a]", mkIrrefutablePat $ mkListPat [ mkLitPat (mkIntLit 0), mkVarPat (mkName "a") ])
    , ("named pattern", "p@Point{ x = 1 }"
      , mkAsPat (mkName "p") $ mkRecPat (mkName "Point") 
                                 [ mkPatternField (mkName "x") (mkLitPat (mkIntLit 1)) ])
    , ("bang pattern", "!(_, f -> 3)"
      , mkBangPat $ mkTuplePat [mkWildPat, mkViewPat (mkVar $ mkName "f") (mkLitPat (mkIntLit 3))])
    ]

testType
  = [ ("forall type", "forall x . Eq x => x -> ()"
      , mkForallType [mkTypeVar (mkName "x")] 
          $ mkCtxType (mkContextOne (mkClassAssert (mkName "Eq") [mkVarType (mkName "x")])) 
          $ mkFunctionType (mkVarType (mkName "x")) (mkVarType (mkName "()")))
    , ("type operators", "(A :+: B) (x, x)"
      , mkTypeApp (mkParenType $ mkInfixTypeApp (mkVarType (mkName "A")) (mkUnqualOp ":+:") (mkVarType (mkName "B")))
                  (mkTupleType [ mkVarType (mkName "x"), mkVarType (mkName "x") ]))
    ]

testBinds
  = [ ("locals",   "x = (a, b) where a = 3\n"
                ++ "                 b = 4"
      , mkSimpleBind (mkVarPat (mkName "x")) (mkUnguardedRhs (mkTuple [(mkVar (mkName "a")), (mkVar (mkName "b"))]))
                     (Just $ mkLocalBinds' [ mkLocalValBind $ mkSimpleBind' (mkName "a") (mkLit $ mkIntLit 3)
                                           , mkLocalValBind $ mkSimpleBind' (mkName "b") (mkLit $ mkIntLit 4)
                                           ]) )
    , ("function bind",   "f i 0 = i\n"
                       ++ "f i x = x"
      , mkFunctionBind' (mkName "f") [ ([mkVarPat $ mkName "i", mkLitPat $ mkIntLit 0], mkVar $ mkName "i")
                                     , ([mkVarPat $ mkName "i", mkVarPat $ mkName "x"], mkVar $ mkName "x")
                                     ])
    ]

testDecls
  = [ ("signature", "id :: a -> a"
      , mkTypeSigDecl $ mkTypeSignature (mkName "id") (mkFunctionType (mkVarType (mkName "a")) (mkVarType (mkName "a"))))
    , ("binding", "id x = x"
      , mkValueBinding $ mkFunctionBind' (mkName "id") [([mkVarPat $ mkName "x"], mkVar $ mkName "x")])
    , ("datatype definition", "data A a = A a deriving Show"
      , mkDataDecl mkDataKeyword Nothing (mkDeclHeadApp (mkNameDeclHead (mkName "A")) (mkTypeVar (mkName "a"))) 
          [mkConDecl (mkName "A") [mkVarType (mkName "a")]] (Just $ mkDeriving [mkInstanceHead (mkName "Show")]))
    , ("record definition", "data A = A { x :: Int }"
      , mkDataDecl mkDataKeyword Nothing (mkNameDeclHead (mkName "A")) 
          [mkRecordConDecl (mkName "A") [mkFieldDecl [mkName "x"] (mkVarType (mkName "Int"))]] Nothing)
    , ("typeclass definition",    "class A t => C t where f :: t\n"
                               ++ "                       type T t :: *"
      , mkClassDecl (Just $ mkContextOne (mkClassAssert (mkName "A") [mkVarType (mkName "t")])) 
                    (mkDeclHeadApp (mkNameDeclHead (mkName "C")) (mkTypeVar (mkName "t"))) []
                    (Just $ mkClassBody [ mkClassElemSig $ mkTypeSignature (mkName "f") (mkVarType (mkName "t"))
                                        , mkClassElemTypeFam (mkDeclHeadApp (mkNameDeclHead (mkName "T")) 
                                                                            (mkTypeVar (mkName "t"))) 
                                                             (Just $ mkTypeFamilyKindSpec $ mkKindConstraint $ mkKindStar)
                                        ])
      )
    , ( "instance definition", "instance C Int where f = 0"
      , mkInstanceDecl Nothing (mkInstanceRule Nothing $ mkAppInstanceHead (mkInstanceHead $ mkName "C") (mkVarType (mkName "Int"))) 
          (Just $ mkInstanceBody [mkInstanceBind $ mkSimpleBind' (mkName "f") (mkLit $ mkIntLit 0)]))
    , ("fixity definition", "infixl 6 +", mkFixityDecl $ mkInfixL 6 (mkUnqualOp "+"))
    ]

testModules
  = [ ("empty module", "", G.mkModule [] Nothing [] [])
    , ("exports", "module Test(x, A(a), B(..)) where"
      , G.mkModule [] (Just $ mkModuleHead (G.mkModuleName "Test") 
          (Just $ mkExportSpecs 
                    [ mkExportSpec $ mkIESpec (mkName "x") Nothing
                    , mkExportSpec $ mkIESpec (mkName "A") (Just $ mkSubList [mkName "a"])
                    , mkExportSpec $ mkIESpec (mkName "B") (Just mkSubAll)
                    ]) Nothing) [] [])
    , ("imports",  "\nimport qualified A\n"
                  ++ "import B as BB(x)\n"
                  ++ "import B hiding (x)"
    , G.mkModule [] Nothing 
        [ mkImportDecl False True False Nothing (G.mkModuleName "A") Nothing Nothing
        , mkImportDecl False False False Nothing (G.mkModuleName "B") (Just $ G.mkModuleName "BB") 
            (Just $ mkImportSpecList [mkIESpec (mkName "x") Nothing])
        , mkImportDecl False False False Nothing (G.mkModuleName "B") Nothing 
            (Just $ mkImportHidingList [mkIESpec (mkName "x") Nothing])
        ] [])
    ]
