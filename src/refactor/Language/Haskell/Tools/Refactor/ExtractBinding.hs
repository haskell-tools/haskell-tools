
{-# LANGUAGE ViewPatterns
           , RankNTypes
           , FlexibleContexts
           #-}
module Language.Haskell.Tools.Refactor.ExtractBinding where

import qualified GHC
import SrcLoc

import Data.Char
import Data.Maybe
import Data.Generics.Uniplate.Data
import Control.Reference hiding (element)
import Control.Monad.State
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

--import Outputable
--import Debug.Trace

type STWithId = STWithNames GHC.Id

extractBinding' :: RealSrcSpan -> String -> Ann Module STWithId -> RefactoredModule GHC.Id
extractBinding' sp name mod
  = if isValidBindingName name then extractBinding (nodesContaining sp) (nodesContaining sp) name mod
                               else refactError "The given name is not a valid for the extracted binding"

extractBinding :: Simple Traversal (Ann Module STWithId) (Ann Decl STWithId)
                   -> Simple Traversal (Ann Decl STWithId) (Ann Expr STWithId)
                   -> String -> Ann Module STWithId -> RefactoredModule GHC.Id
extractBinding selectDecl selectExpr name mod
  = let declName = head (mod ^? selectDecl & element & valBind & bindingName)
        isTheDecl (Just d) = maybe False (declName ==) (listToMaybe $ d ^? element & valBind & bindingName)
        isTheDecl Nothing = False
     in do (res, st) <- runStateT (selectDecl&selectExpr !~ extractThatBind name $ mod) Nothing
           case st of Just def -> return $ element & modDecl .- insertWhere (mkValueBinding def) isTheDecl (const True) $ res
                      Nothing -> return res

extractThatBind :: String -> Ann Expr STWithId -> StateT (Maybe (Ann ValueBind STWithId)) (Refactor GHC.Id) (Ann Expr STWithId)
extractThatBind name e | Paren {} <- e ^. element
                       = do modified <- doExtract name (fromJust $ e ^? element & exprInner)
                            element & exprInner != modified $ e
extractThatBind name e = doExtract name e

doExtract :: String -> Ann Expr STWithId -> StateT (Maybe (Ann ValueBind STWithId)) (Refactor GHC.Id) (Ann Expr STWithId)
doExtract name e = do ret <- get
                      if (isJust ret) then return e
                         else do params <- lift $ getExternalBinds e
                                 put (Just (generateBind name params e))
                                 return (generateCall name params)

getExternalBinds :: Ann Expr STWithId -> Refactor GHC.Id [Ann Name STWithId]
getExternalBinds expr = keepFirsts <$> filterM isApplicableName (expr ^? uniplateRef & element & exprName)
  where isApplicableName (getNameInfo -> Just nm) = (not (nm `elem` namesDefinedInside) &&) <$> isLocalName nm 
        isApplicableName _ = return False
        namesDefinedInside = catMaybes $ map getNameInfoFromSema $ filter (fromMaybe False . (^? isDefined)) (map (^. semantics) allNames)
        allNames :: [Ann Name STWithId]
        allNames = expr ^? biplateRef

        allPatterns :: [Ann Pattern STWithId]
        allPatterns = expr ^? biplateRef

        isLocalName n = isNothing <$> GHC.lookupName n

        keepFirsts (e:rest) = e : keepFirsts (filter (/= e) rest)
        keepFirsts [] = []

generateCall :: String -> [Ann Name STWithId] -> Ann Expr STWithId
generateCall name args = foldl (\e a -> mkApp e (mkVar a)) (mkVar (mkUnqualName name)) args

generateBind :: String -> [Ann Name STWithId] -> Ann Expr STWithId -> Ann ValueBind STWithId
generateBind name [] e = mkSimpleBind (mkVarPat (mkUnqualName name)) (mkUnguardedRhs e) Nothing
generateBind name args e = mkFunctionBind [mkMatch (mkUnqualName name) (map mkVarPat args) (mkUnguardedRhs e) Nothing]

isValidBindingName :: String -> Bool
isValidBindingName [] = False
isValidBindingName (firstChar:rest) = isIdStartChar firstChar && all isIdChar rest
  where isIdStartChar c = (isLetter c && isUpper c) || c == '\'' || c == '_'
        isIdChar c = isLetter c || c == '\'' || c == '_' || isDigit c
