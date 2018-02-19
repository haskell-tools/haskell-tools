{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.TypeSynonymInstancesChecker
  ( chkTypeSynonymInstancesDecl, chkInstancesDeclWith, chkInstanceRuleWith, collectTyArgs) where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad hiding (StandaloneDeriving)


{-# ANN module "HLint: ignore Redundant bracket" #-}

-- TODO: write "deriving instance ..." tests (should work)

-- | We need to check declarations because we want to avoid checking type family instances
chkTypeSynonymInstancesDecl :: CheckNode Decl
chkTypeSynonymInstancesDecl = conditional (chkInstancesDeclWith $ chkInstanceRuleWith chkInstanceHead) TypeSynonymInstances

-- | Checks an instance rule in declaration
-- We need to check declarations because we want to avoid checking type family instances
chkInstancesDeclWith :: CheckNode InstanceRule -> CheckNode Decl
chkInstancesDeclWith chkRule d@(StandaloneDeriving _ _ rule) = chkRule rule >> return d
chkInstancesDeclWith chkRule d@(InstanceDecl rule _)         = chkRule rule >> return d
chkInstancesDeclWith _ d = return d

-- | Checks and instance head in an instance rule
chkInstanceRuleWith :: CheckNode InstanceHead -> CheckNode InstanceRule
chkInstanceRuleWith chkHead r@(InstanceRule _ _ ihead) = chkHead ihead >> return r
chkInstanceRuleWith _ r = return r


-- | Checks every single type argument in an instance declaration whether it is a synonym
chkInstanceHead :: CheckNode InstanceHead
chkInstanceHead ih = do
  let types = collectTyArgs ih
  mapM_ chkTypeArg types >> return ih

-- | Checks a type argument of class whether it is a synonym
chkTypeArg :: Type -> ExtMonad Type
chkTypeArg ty =
  maybeTM (return ty) (const . addOccurence TypeSynonymInstances $ ty) (semanticsTypeSynRhs ty)

-- | Collects the type arguments in an instance declaration
-- Type arguments are the the types that the class is being instantiated with
collectTyArgs :: InstanceHead -> [Type]
collectTyArgs (InstanceHead _)        = []
collectTyArgs (InfixInstanceHead t _) = [t]
collectTyArgs (ParenInstanceHead ih)  = collectTyArgs ih
collectTyArgs (AppInstanceHead ih t)  = t : collectTyArgs ih
