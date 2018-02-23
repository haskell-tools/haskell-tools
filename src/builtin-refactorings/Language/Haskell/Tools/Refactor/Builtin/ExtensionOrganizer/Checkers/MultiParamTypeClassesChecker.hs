module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.MultiParamTypeClassesChecker where

import Control.Reference ((^.))

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkMultiParamTypeClassesDecl :: CheckNode Decl
chkMultiParamTypeClassesDecl = conditional chkMultiParamTypeClassesDecl' MultiParamTypeClasses


-- | Decides whether a class or instance declaration needs MultiParamTypeClasses
-- Also handles the NullaryTypeClasses case
chkMultiParamTypeClassesDecl' :: CheckNode Decl
chkMultiParamTypeClassesDecl' cd@(ClassDecl _ dh _ _)
  | n <- length . collectTyVars $ dh
  , n /= 1 = addOccurence MultiParamTypeClasses dh >> return cd
chkMultiParamTypeClassesDecl' i@(InstanceDecl rule _)
  | isMultiParamNeeded (rule ^. irHead)
  = addOccurence MultiParamTypeClasses rule >> return i
chkMultiParamTypeClassesDecl' d = return d


collectTyVars :: DeclHead -> [TyVar]
collectTyVars (ParenDeclHead dh)        = collectTyVars dh
collectTyVars (DeclHeadApp   dh tv)     = tv : collectTyVars dh
collectTyVars (InfixDeclHead lhs _ rhs) = [lhs,rhs]
collectTyVars _ = []


-- | Decides whether an instance declaration needs MultiParamTypeClasses
-- Also handles the NullaryTypeClasses case
isMultiParamNeeded :: InstanceHead -> Bool
isMultiParamNeeded InstanceHead{}         = True
isMultiParamNeeded InfixInstanceHead{}    = True
isMultiParamNeeded (ParenInstanceHead ih) = isMultiParamNeeded ih
isMultiParamNeeded (AppInstanceHead f _)  = isMultiParamNeeded' f
  -- one level deeper
  where isMultiParamNeeded' InstanceHead{}         = False
        isMultiParamNeeded' InfixInstanceHead{}    = True
        isMultiParamNeeded' (ParenInstanceHead ih) = isMultiParamNeeded' ih
        isMultiParamNeeded' (AppInstanceHead _ _)  = True
