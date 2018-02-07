module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.GADTsChecker where

import Data.Maybe (catMaybes, fromMaybe)
import Control.Reference ((^.), (&))
import Control.Monad.Trans.Maybe (MaybeT(..))

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.TypeLookup


-- | Checks a GADT-style constructor if GADTSyntax is turned on.
-- Sometimes GADTSyntax is sufficient and GADTs is not even needed.
chkGADTsGadtConDecl :: CheckNode GadtConDecl
chkGADTsGadtConDecl = conditional chkGADTsGadtConDecl' GADTSyntax

-- | Checks a data constructor declaration if GADTs or ExistentialQuantification is turned on.
-- This function is responsible for checking ExistentialQuantification as well.
-- (there is no separate checker for that extension)
chkConDeclForExistentials :: CheckNode ConDecl
chkConDeclForExistentials = conditionalAny chkConDeclForExistentials' [GADTs, ExistentialQuantification]

-- If all data constructors are vanilla Haskell 98 data constructors, then only GADTSyntax is needed.
chkGADTsGadtConDecl' :: CheckNode GadtConDecl
chkGADTsGadtConDecl' conDecl = do
  let conNames = conDecl ^. (gadtConNames & annListElems)
  mres <- mapM (runMaybeT . isVanillaDataConNameM) conNames
  if and . catMaybes $ mres
    then addOccurence GADTSyntax conDecl
    else do addOccurence GADTSyntax conDecl
            addRelation (GADTs `lOr` ExistentialQuantification) conDecl

-- Extracts the name from a ConDecl, and checks whether it is a vanilla
-- data constructor.
chkConDeclForExistentials' :: CheckNode ConDecl
chkConDeclForExistentials' conDecl = liftM (fromMaybe conDecl) . runMaybeT $
  case conDecl ^. element of
    UConDecl _ _ n _         -> chkName n
    URecordDecl _ _ n _      -> chkName n
    UInfixConDecl _ _ _ op _ -> chkName (op ^. operatorName)
  where chkName :: HasNameInfo' n => n -> MaybeT ExtMonad ConDecl
        chkName n = do
          isVanilla <- isVanillaDataConNameM $ n
          if isVanilla
            then return conDecl
            else lift . addRelation (GADTs `lOr` ExistentialQuantification) $ conDecl
