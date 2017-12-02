{-# LANGUAGE RankNTypes, FlexibleContexts, TypeFamilies #-}
module Language.Haskell.Tools.Refactor.Builtin.IfToGuards (ifToGuards, tryItOut) where

import Control.Reference ((^.), (.-), (&))
import Data.Generics.Uniplate.Data ()
import Language.Haskell.Tools.Refactor
import SrcLoc (RealSrcSpan)

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . ifToGuards)

ifToGuards :: RealSrcSpan -> LocalRefactoring
ifToGuards sp = return . (nodesContaining sp .- changeBindings)

changeBindings :: ValueBind -> ValueBind
changeBindings (SimpleBind (VarPat name) (UnguardedRhs (If pred thenE elseE)) locals)
  = mkFunctionBind [mkMatch (mkMatchLhs name []) (createSimpleIfRhss pred thenE elseE) (locals ^. annMaybe) ]
changeBindings fbs@(FunctionBind {})
  = funBindMatches&annList&matchRhs .- trfRhs $ fbs
  where trfRhs :: Rhs -> Rhs
        trfRhs (UnguardedRhs (If pred thenE elseE)) = createSimpleIfRhss pred thenE elseE
        trfRhs e = e -- don't transform already guarded right-hand sides to avoid multiple evaluation of the same condition
changeBindings b = b

createSimpleIfRhss :: Expr -> Expr -> Expr -> Rhs
createSimpleIfRhss pred thenE elseE = mkGuardedRhss [ mkGuardedRhs [mkGuardCheck pred] thenE
                                                    , mkGuardedRhs [mkGuardCheck (mkVar (mkName "otherwise"))] elseE
                                                    ]
