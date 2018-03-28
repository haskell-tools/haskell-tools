{-# LANGUAGE FlexibleContexts, MonoLocalBinds, ScopedTypeVariables, TupleSections, ViewPatterns #-}

module Language.Haskell.Tools.Refactor.Builtin.AutoCorrect (autoCorrect, tryItOut, autoCorrectRefactoring) where

import SrcLoc
import GHC
import Type
-- import Outputable
--
import Control.Monad.State
import Control.Reference
import Data.List
import Data.Maybe
--
import Language.Haskell.Tools.Refactor as HT
--
import Language.Haskell.Tools.PrettyPrint

autoCorrectRefactoring :: RefactoringChoice
autoCorrectRefactoring = SelectionRefactoring "AutoCorrect" (localRefactoring . autoCorrect)

tryItOut :: String -> String -> IO ()
tryItOut mod sp = tryRefactor (localRefactoring . autoCorrect) mod sp

autoCorrect :: RealSrcSpan -> LocalRefactoring
autoCorrect sp mod
  = do res <- mapM (\f -> f sp mod) [reParen, reOrder]
       case catMaybes res of mod':_ -> return mod'
                             []     -> refactError "Cannot auto-correct the selection."

--------------------------------------------------------------------------------------------

reOrder :: RealSrcSpan -> HT.Module -> LocalRefactor (Maybe HT.Module)
reOrder sp mod = do let accessibleMods = semanticsModule mod : semanticsPrelTransMods mod ++ concatMap semanticsTransMods (mod ^? modImports & annList :: [HT.ImportDecl])
                        rng:_ = map getRange (mod ^? nodesContained sp :: [Expr])
                    insts <- liftGhc $ fst <$> getInstances accessibleMods
                    (res,done) <- liftGhc $ flip runStateT False ((nodesContained sp & filtered ((==rng) . getRange) !~ reOrderExpr insts) mod)
                    return (if done then Just res else Nothing)

reOrderExpr :: [ClsInst] -> Expr -> StateT Bool Ghc Expr
reOrderExpr insts e@(App (App f a1) a2)
  = do funTy <- lift $ typeExpr f
       arg1Ty <- lift $ typeExpr a1
       arg2Ty <- lift $ typeExpr a2
       -- liftIO $ putStrLn $ (show $ isJust $ appTypeMatches insts funTy [arg1Ty, arg2Ty]) ++ " " ++ (show $ isJust $ appTypeMatches insts funTy [arg2Ty, arg1Ty]) ++ " " ++ (showSDocUnsafe $ ppr $ funTy) ++ " " ++ (showSDocUnsafe $ ppr $ arg1Ty) ++ " " ++ (showSDocUnsafe $ ppr $ arg2Ty)
       if not (isJust (appTypeMatches insts funTy [arg1Ty, arg2Ty])) && isJust (appTypeMatches insts funTy [arg2Ty, arg1Ty])
          then put True >> return (exprArg .= a1 $ exprFun&exprArg .= a2 $ e)
          else return e
reOrderExpr insts e@(InfixApp lhs op rhs)
  = do let funTy = idType $ semanticsId (op ^. operatorName)
       lhsTy <- lift $ typeExpr lhs
       rhsTy <- lift $ typeExpr rhs
       -- liftIO $ putStrLn $ (show $ isJust $ appTypeMatches insts funTy [lhsTy, rhsTy]) ++ " " ++ (show $ isJust $ appTypeMatches insts funTy [lhsTy, rhsTy]) ++ " " ++ (showSDocUnsafe $ ppr $ funTy) ++ " " ++ (showSDocUnsafe $ ppr $ lhsTy) ++ " " ++ (showSDocUnsafe $ ppr $ rhsTy)
       if not (isJust (appTypeMatches insts funTy [lhsTy, rhsTy])) && isJust (appTypeMatches insts funTy [rhsTy, lhsTy])
          then put True >> return (exprLhs .= rhs $ exprRhs .= lhs $ e)
          else return e
reOrderExpr _ e = return e

------------------------------------------------------------------------------------------

reParen :: RealSrcSpan -> HT.Module -> LocalRefactor (Maybe HT.Module)
reParen sp mod = do let accessibleMods = semanticsModule mod : semanticsPrelTransMods mod ++ concatMap semanticsTransMods (mod ^? modImports & annList :: [HT.ImportDecl])
                        rng:_ = map getRange (mod ^? nodesContained sp :: [Expr])
                    insts <- liftGhc $ fst <$> getInstances accessibleMods
                    (res,done) <- liftGhc $ flip runStateT False ((nodesContained sp & filtered ((==rng) . getRange) !~ reParenExpr insts) mod)
                    return (if done then Just res else Nothing)

reParenExpr :: [ClsInst] -> Expr -> StateT Bool Ghc Expr
reParenExpr insts e = do atoms <- lift $ extractAtoms e
                         case correctParening insts $ map (_2 .- Left) atoms of
                           [e'] -> put True >> return (wrapAtom e')
                           [] -> return e
                           ls -> -- TODO: choose the best one
                                 error $ "multiple correct parentheses were found: " ++ intercalate ", " (map (either prettyPrintAtom prettyPrint) ls)

data Atom = NameA HT.Name
          | OperatorA Operator
          | LiteralA Literal

prettyPrintAtom :: Atom -> String
prettyPrintAtom (NameA n) = prettyPrint n
prettyPrintAtom (OperatorA o) = prettyPrint o
prettyPrintAtom (LiteralA l) = prettyPrint l

type Build = Either Atom Expr

extractAtoms :: Expr -> Ghc [(GHC.Type, Atom)]
extractAtoms e = do lits <- mapM (\l -> (, LiteralA l) <$> literalType l) (e ^? biplateRef)
                    return $ sortOn (srcSpanStart . atomRange . snd)
                           $ map (\n -> (idType $ semanticsId (n ^. simpleName), NameA n)) (e ^? biplateRef)
                               ++ map (\o -> (idType $ semanticsId (o ^. operatorName), OperatorA o)) (e ^? biplateRef)
                               ++ lits

atomRange :: Atom -> SrcSpan
atomRange (NameA n) = getRange n
atomRange (OperatorA n) = getRange n
atomRange (LiteralA n) = getRange n

wrapAtom :: Build -> Expr
wrapAtom (Right e) = e
wrapAtom (Left (NameA n)) = mkVar n
wrapAtom (Left (OperatorA (NormalOp o))) = mkVar (mkParenName o)
wrapAtom (Left (OperatorA (BacktickOp n))) = mkVar (mkNormalName n)
wrapAtom (Left (LiteralA l)) = mkLit l

correctParening :: [ClsInst] -> [(GHC.Type, Build)] -> [Build]
correctParening _ [(_,e)] = [e]
correctParening insts ls = concatMap (correctParening insts) (reduceAtoms insts ls)

reduceAtoms :: [ClsInst] -> [(GHC.Type, Build)] -> [[(GHC.Type, Build)]]
reduceAtoms _ [(t,e)] = [[(t,e)]]
reduceAtoms insts ls = concatMap (reduceBy insts ls) [0 .. length ls - 2]

reduceBy :: [ClsInst] -> [(GHC.Type, Build)] -> Int -> [[(GHC.Type, Build)]]
reduceBy insts (zip [0..] -> ls) i = maybeToList (reduceFunctionApp ls i) ++ maybeToList (reduceOperatorApp ls i)
  where reduceFunctionApp ls i | Just (funT, fun) <- lookup i ls
                               , Just (argT, arg) <- lookup (i+1) ls
                               , Just (subst, resTyp) <- appTypeMatches insts funT [argT]
          = Just $ map ((_1 .- substTy subst) . snd) (take i ls)
                     ++ [(resTyp, mkParen' (mkApp' fun arg))]
                     ++ map ((_1 .- substTy subst) . snd) (drop (i + 2) ls)
        reduceFunctionApp _ _ = Nothing

        reduceOperatorApp ls i | Just (opT, Left (OperatorA op)) <- lookup i ls
                               , Just (lArgT, lArg) <- lookup (i-1) ls
                               , Just (rArgT, rArg) <- lookup (i+1) ls
                               , Just (subst, resTyp) <- appTypeMatches insts opT [lArgT, rArgT]
          = Just $ map ((_1 .- substTy subst) . snd) (take (i - 1) ls)
                     ++ [(resTyp, mkParen' (mkInfixApp' lArg op rArg))]
                     ++ map ((_1 .- substTy subst) . snd) (drop (i + 2) ls)
        reduceOperatorApp _ _ = Nothing

mkApp' :: Build -> Build -> Build
mkApp' (wrapAtom -> f) (wrapAtom -> a) = Right $ mkApp f a

mkInfixApp' :: Build -> Operator -> Build -> Build
mkInfixApp' (wrapAtom -> lhs) op (wrapAtom -> rhs) = Right $ mkInfixApp lhs op rhs

mkParen' :: Build -> Build
mkParen' (wrapAtom -> e) = Right $ mkParen e
