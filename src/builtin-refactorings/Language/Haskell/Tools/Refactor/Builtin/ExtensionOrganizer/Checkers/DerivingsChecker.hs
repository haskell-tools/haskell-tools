{-# LANGUAGE FlexibleContexts, MultiWayIf, ViewPatterns #-}


{-
  NOTE: We need Decl level checking in order to gain extra information
        from the newtype and data keywords.
  NOTE: Here we implicitly constrained the type with ExtDomain.
        but we only really need HasNameInfo.

        When a strategy is not explicitly given, every single extension
        is analysied individually, so every occurence induces a separate
        extension. However, when a strategy is specified, we only add
        GND and DeriveAnyClass once, since no further examination is needed.

        For examples see the test files.

  SEE:  https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DerivingStrategies#Thederivingstrategyresolutionalgorithm

  TODO:
  - write tests for GADTs, data instances
  - correct DerivingStrategies behaviour with StandaloneDeriving
-}


module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.DerivingsChecker where

import Control.Reference ((^.), (!~), (&))
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor as Refact hiding (Enum)
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad as Ext

import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Map as Map (fromList, lookup)

import qualified GHC (Name(..), isNewTyCon)
import PrelNames
import THNames (liftClassName)

-- can be derived
isStockClass = flip elem stockClasses
stockClasses = [ eqClassName
               , ordClassName
               , ixClassName
               , showClassName
               , readClassName
               , enumClassName
               , boundedClassName
               , dataClassName
               , typeableClassName
               , genClassName
               , gen1ClassName
               , functorClassName
               , foldableClassName
               , traversableClassName
               , liftClassName
               ]

-- can be derived for newtypes even without GND
gndNotNeeded :: GHC.Name -> Bool
gndNotNeeded = flip elem gndNotNeededClasses
gndNotNeededClasses =
  [ eqClassName
  , ordClassName
  , ixClassName
  , boundedClassName
  ]

-- can be derived for newtypes with GND
gndNeeded :: GHC.Name -> Bool
gndNeeded = flip elem gndNeededClasses
gndNeededClasses =
  [ functorClassName
  , foldableClassName
  , enumClassName
  ]

-- never selected by default for newtypes (even with GND)
gndNotAllowed :: GHC.Name -> Bool
gndNotAllowed = flip elem gndNotAllowedClasses
gndNotAllowedClasses =
  [ dataClassName
  , typeableClassName
  , showClassName
  , readClassName
  , traversableClassName
  , genClassName
  , gen1ClassName
  , liftClassName
  ]

whichExtension :: GHC.Name -> Maybe Extension
whichExtension    = flip Map.lookup nameExtensionMap

nameExtensionMap = Map.fromList nameExtensions
  where nameExtensions = [ (dataClassName,        DeriveDataTypeable)
                         , (typeableClassName,    DeriveDataTypeable)
                         , (genClassName,         DeriveGeneric)
                         , (gen1ClassName,        DeriveGeneric)
                         , (functorClassName,     DeriveFunctor)
                         , (foldableClassName,    DeriveFoldable)
                         , (traversableClassName, DeriveTraversable)
                         , (liftClassName,        DeriveLift)
                         ]

chkDerivings :: CheckNode Decl
chkDerivings = conditionalAny chkDerivings'         derivingExts
           >=> conditional    chkStandaloneDeriving Ext.StandaloneDeriving

      where chkDerivings' = chkDataDecl
                        >=> chkGADTDataDecl
                        >=> chkDataInstance

            derivingExts = [ DeriveDataTypeable
                           , DeriveGeneric
                           , DeriveFunctor
                           , DeriveFoldable
                           , DeriveTraversable
                           , DeriveLift
                           , DeriveAnyClass
                           , GeneralizedNewtypeDeriving
                           , DerivingStrategies
                           ]


chkDataDecl :: CheckNode Decl
chkDataDecl d@(DataDecl keyw _ _ _ derivs) = do
  annList !~ separateByKeyword keyw $ derivs
  return d
chkDataDecl d = return d

chkGADTDataDecl :: CheckNode Decl
chkGADTDataDecl d@(GADTDataDecl keyw _ _ _ _ derivs) = do
  addEvidence_ GADTs d
  annList !~ separateByKeyword keyw $ derivs
  return d
chkGADTDataDecl d = return d

chkDataInstance :: CheckNode Decl
chkDataInstance d@(DataInstance keyw _ _ derivs) = do
  addEvidence_ TypeFamilies d
  annList !~ separateByKeyword keyw $ derivs
  return d
chkDataInstance d = return d


separateByKeyword :: DataOrNewtypeKeyword -> CheckNode Deriving
separateByKeyword keyw derivs
  | isNewtypeDecl keyw = chkByStrat chkClassForNewtype derivs
  | otherwise          = chkByStrat chkClassForData    derivs
  where isNewtypeDecl keyw = case keyw ^. element of
                               UNewtypeKeyword -> True
                               _               -> False


getStrategy :: Deriving -> Maybe DeriveStrategy
getStrategy d = d ^. (deriveStrategy & annMaybe)

addExtension :: (MonadState ExtMap m, HasRange node) =>
                 GHC.Name -> node -> m node
addExtension sname
  | Just ext <- whichExtension sname = addEvidence ext
  | otherwise                        = return

addStockExtension :: CheckNode InstanceHead
addStockExtension x
  | Just sname <- nameFromStock x = addExtension sname x
  | otherwise = return x

chkByStrat :: CheckNode InstanceHead -> CheckNode Deriving
chkByStrat checker d
  | Just strat <- getStrategy d = do
    addEvidence DerivingStrategies d
    chkDerivingClause (chkStrat strat) d
  | otherwise =
    chkDerivingClause checker d

chkStrat :: DeriveStrategy -> CheckNode InstanceHead
chkStrat (_element -> UStockStrategy)    = addStockExtension
chkStrat (_element -> UNewtypeStrategy)  = addEvidence GeneralizedNewtypeDeriving
chkStrat (_element -> UAnyClassStrategy) = addEvidence DeriveAnyClass

chkDerivingClause :: CheckNode InstanceHead -> CheckNode Deriving
chkDerivingClause checker d@(DerivingOne   x)  = checker x               >> return d
chkDerivingClause checker d@(DerivingMulti xs) = (annList !~ checker) xs >> return d

-- checks whether the class is stock, and if it is, returns its name
nameFromStock :: InstanceHead -> Maybe GHC.Name
nameFromStock x
  | InstanceHead name <- skipParens x,
    Just sname <- semanticsName name,
    isStockClass sname
    = Just sname
  | otherwise = Nothing

chkClassForData :: CheckNode InstanceHead
chkClassForData x
  | Just sname <- nameFromStock x = addExtension sname x
  | otherwise = addEvidence DeriveAnyClass x

-- performs check in case no explicit strategy is given
chkClassForNewtype :: CheckNode InstanceHead
chkClassForNewtype x
  | Just sname <- nameFromStock x
    = if | gndNotNeeded  sname -> return x
         | gndNeeded     sname -> addEvidence GeneralizedNewtypeDeriving x
         | gndNotAllowed sname -> addExtension sname x
  | otherwise = do
      gndOn       <- isTurnedOn GeneralizedNewtypeDeriving
      deriveAnyOn <- isTurnedOn DeriveAnyClass
      if | gndOn && deriveAnyOn -> addEvidence_ DeriveAnyClass x
         | deriveAnyOn          -> addEvidence_ DeriveAnyClass x
         | gndOn                -> addEvidence_ GeneralizedNewtypeDeriving x
         | otherwise             -> return ()
      return x

skipParens :: InstanceHead -> InstanceHead
skipParens (ParenInstanceHead x) = skipParens x
skipParens x = x

chkStandaloneDeriving :: CheckNode Decl
chkStandaloneDeriving d@(Refact.StandaloneDeriving strat _ (decompRule -> (cls,ty)))
  | Just strat' <- strat = do
    addEvidence_  Ext.DerivingStrategies d
    addEvidence_  Ext.StandaloneDeriving d
    chkStrat strat' cls
    return d
  | otherwise = do
    addEvidence_  Ext.StandaloneDeriving d
    itIsNewType    <- isNewtype ty
    itIsSynNewType <- isSynNewType ty
    if itIsNewType || itIsSynNewType
      then chkClassForNewtype cls
      else chkClassForData    cls
    return d
chkStandaloneDeriving d = return d

decompRule :: InstanceRule -> (InstanceHead, Type)
decompRule instRule = (cls, ty)
  where ihead = instRule  ^. irHead
        cls   = getClassCon   ihead
        ty    = rightmostType ihead

getClassCon :: InstanceHead -> InstanceHead
getClassCon (AppInstanceHead f _) = getClassCon f
getClassCon (ParenInstanceHead x) = getClassCon x
getClassCon x = x

rightmostType :: InstanceHead -> Type
rightmostType ihead
  | AppInstanceHead _ tyvar <- skipParens ihead = tyvar

{-
  NOTE: Returns false if the type is certainly not a type synonym.
        Returns true if it is a synonym for a newtype or it could not have been looked up.

  This behaviour will produce false positives.
  This is desirable since the underlying type might be a newtype
  in which case GeneralizedNewtypeDeriving might be necessary.
-}
isSynNewType :: Type -> ExtMonad Bool
isSynNewType t = do
  mtycon <- runMaybeT . lookupType $ t
  case mtycon of
    Nothing    -> return True
    Just tycon -> isSynNewType' tycon
  where isSynNewType' x = case lookupSynDef x of
                            Nothing  -> return False
                            Just def -> return (GHC.isNewTyCon def)
