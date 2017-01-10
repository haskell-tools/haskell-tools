{-# LANGUAGE RankNTypes, FlexibleContexts, ViewPatterns #-}
module Language.Haskell.Tools.Refactor.Predefined.IfToGuards (ifToGuards, tryItOut) where

import Language.Haskell.Tools.Refactor
import Control.Reference
import SrcLoc
import Data.Generics.Uniplate.Data ()

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . ifToGuards)

ifToGuards :: Domain dom => RealSrcSpan -> LocalRefactoring dom
ifToGuards sp = return . (nodesContaining sp .- changeBindings)

changeBindings :: ValueBind dom -> ValueBind dom
changeBindings (SimpleBind (VarPat name) (UnguardedRhs (If pred thenE elseE)) locals) 
  = mkFunctionBind [mkMatch (mkMatchLhs name []) (createSimpleIfRhss pred thenE elseE) (locals ^. annMaybe) ]
changeBindings fbs@(FunctionBind {}) 
  = funBindMatches&annList&matchRhs .- trfRhs $ fbs
  where trfRhs :: Rhs dom -> Rhs dom
        trfRhs (UnguardedRhs (If pred thenE elseE)) = createSimpleIfRhss pred thenE elseE
        trfRhs e = e -- don't transform already guarded right-hand sides to avoid multiple evaluation of the same condition
changeBindings b = b

createSimpleIfRhss :: Expr dom -> Expr dom -> Expr dom -> Rhs dom
createSimpleIfRhss pred thenE elseE = mkGuardedRhss [ mkGuardedRhs [mkGuardCheck pred] thenE
                                                    , mkGuardedRhs [mkGuardCheck (mkVar (mkName "otherwise"))] elseE
                                                    ]

