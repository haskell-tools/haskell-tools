{-# LANGUAGE RankNTypes, FlexibleContexts, ViewPatterns #-}
module Language.Haskell.Tools.Refactor.Predefined.IfToGuards (ifToGuards) where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Rewrite
import Language.Haskell.Tools.Refactor.RefactorBase

import Control.Reference hiding (element)
import SrcLoc
import Data.Generics.Uniplate.Data

import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.Refactor

tryItOut moduleName sp = tryRefactor (localRefactoring $ ifToGuards (readSrcSpan (toFileName "." moduleName) sp)) moduleName

ifToGuards :: Domain dom => RealSrcSpan -> LocalRefactoring dom
ifToGuards sp = return . (nodesContaining sp .- ifToGuards')

ifToGuards' :: ValueBind dom -> ValueBind dom
ifToGuards' (SimpleBind (VarPat name) (UnguardedRhs (If pred thenE elseE)) locals) 
  = mkFunctionBind [mkMatch (mkMatchLhs name []) (createSimpleIfRhss pred thenE elseE) (locals ^. annMaybe) ]
ifToGuards' fbs@(FunctionBind {}) 
  = funBindMatches&annList&matchRhs .- trfRhs $ fbs
  where trfRhs :: Rhs dom -> Rhs dom
        trfRhs (UnguardedRhs (If pred thenE elseE)) = createSimpleIfRhss pred thenE elseE
        trfRhs e = e -- don't transform already guarded right-hand sides to avoid multiple evaluation of the same condition

createSimpleIfRhss :: Expr dom -> Expr dom -> Expr dom -> Rhs dom
createSimpleIfRhss pred thenE elseE = mkGuardedRhss [ mkGuardedRhs [mkGuardCheck pred] thenE
                                                    , mkGuardedRhs [mkGuardCheck (mkVar (mkName "otherwise"))] elseE
                                                    ]

