{-# LANGUAGE LambdaCase
           , TupleSections
           , TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , ScopedTypeVariables
           , MultiParamTypeClasses
           , UndecidableInstances
           , AllowAmbiguousTypes
           , TypeApplications
           #-}
-- | Functions that convert the basic elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.BackendGHC.Names where

import Control.Monad.Reader ((=<<), asks)
import Data.Char (isAlphaNum)
import Data.List.Split (splitOn)

import FastString as GHC (FastString, unpackFS)
import HsSyn as GHC
import Name as GHC (isSymOcc, occNameString)
import qualified Name as GHC (Name)
import OccName as GHC (HasOccName)
import RdrName as GHC (RdrName)
import SrcLoc as GHC

import Language.Haskell.Tools.AST (Ann(..), AnnListG, RangeStage, Dom)
import qualified Language.Haskell.Tools.AST as AST

import Language.Haskell.Tools.BackendGHC.GHCUtils
import Language.Haskell.Tools.BackendGHC.Monad
import Language.Haskell.Tools.BackendGHC.Utils

trfOperator :: TransformName n r => Located n -> Trf (Ann AST.UOperator (Dom r) RangeStage)
trfOperator = trfLocNoSema trfOperator'

trfOperator' :: TransformName n r => n -> Trf (AST.UOperator (Dom r) RangeStage)
trfOperator' n
  | isSymOcc (occName n) = AST.UNormalOp <$> trfQualifiedNameFocus True n
  | otherwise = AST.UBacktickOp <$> trfQualifiedNameFocus True n

trfName :: TransformName n r => Located n -> Trf (Ann AST.UName (Dom r) RangeStage)
trfName = trfLocNoSema trfName'

trfName' :: TransformName n r => n -> Trf (AST.UName (Dom r) RangeStage)
trfName' n
  | isSymOcc (occName n) = (if isSpecKind then AST.UNormalName else AST.UParenName) <$> trfQualifiedNameFocus isSpecKind n
  | otherwise = AST.UNormalName <$> trfQualifiedNameFocus False n
  where -- special names that are operators, but appear in name context
    isSpecKind = occNameString (occName n) `elem` ["*", "#", "?", "??"]

trfAmbiguousFieldName :: TransformName n r => Located (AmbiguousFieldOcc n) -> Trf (Ann AST.UName (Dom r) RangeStage)
trfAmbiguousFieldName (L l af) = trfAmbiguousFieldName' l af

trfAmbiguousFieldName' :: forall n r . TransformName n r => SrcSpan -> AmbiguousFieldOcc n -> Trf (Ann AST.UName (Dom r) RangeStage)
trfAmbiguousFieldName' l (Unambiguous (L _ rdr) pr) = annLocNoSema (pure l) $ trfName' (unpackPostRn @n rdr pr)
-- no Id transformation is done, so we can basically ignore the postTC value
trfAmbiguousFieldName' _ (Ambiguous (L l rdr) _)
  = annLocNoSema (pure l)
      $ (if (isSymOcc (occName rdr)) then AST.UParenName else AST.UNormalName)
          <$> (annLoc (createAmbigousNameInfo rdr l) (pure l) $ AST.nameFromList <$> trfNameStr (isSymOcc (occName rdr)) (rdrNameStr rdr))

trfAmbiguousOperator' :: forall n r . TransformName n r => SrcSpan -> AmbiguousFieldOcc n -> Trf (Ann AST.UOperator (Dom r) RangeStage)
trfAmbiguousOperator' l (Unambiguous (L _ rdr) pr) = annLocNoSema (pure l) $ trfOperator' (unpackPostRn @n rdr pr)
-- no Id transformation is done, so we can basically ignore the postTC value
trfAmbiguousFieldOperator' _ (Ambiguous (L l rdr) _)
  = annLocNoSema (pure l)
      $ (if (isSymOcc (occName rdr)) then AST.UNormalOp else AST.UBacktickOp)
          <$> (annLoc (createAmbigousNameInfo rdr l) (pure l) $ AST.nameFromList <$> trfOperatorStr (not $ isSymOcc (occName rdr)) (rdrNameStr rdr))


class (DataId n, Eq n, GHCName n, FromGHCName n, NameOrRdrName n ~ n, HasOccName n) => TransformableName n where
  correctNameString :: n -> Trf String
  transformSplice :: HsSplice RdrName -> Trf (HsSplice n)

instance TransformableName RdrName where
  correctNameString = pure . rdrNameStr
  transformSplice = pure

instance TransformableName GHC.Name where
  correctNameString n = getOriginalName (rdrName n)
  transformSplice = rdrSplice

-- | This class allows us to use the same transformation code for multiple variants of the GHC AST.
-- GHC UName annotated with 'name' can be transformed to our representation with semantic annotations of 'res'.
class (TransformableName name, HsHasName name, TransformableName res, HsHasName res, GHCName res)
        => TransformName name res where
  -- | Demote a given name
  transformName :: name -> res

instance {-# OVERLAPPABLE #-} (n ~ r, TransformableName n, HsHasName n) => TransformName n r where
  transformName = id

instance {-# OVERLAPS #-} (TransformableName res, GHCName res, HsHasName res) => TransformName GHC.Name res where
  transformName = fromGHCName

trfNameText :: String -> Trf (Ann AST.UName (Dom r) RangeStage)
trfNameText str
  = annContNoSema (AST.UNormalName <$> annLoc (createImplicitNameInfo str) (asks contRange) (AST.nameFromList <$> trfNameStr (isOperatorStr str) str))

trfImplicitName :: HsIPName -> Trf (Ann AST.UName (Dom r) RangeStage)
trfImplicitName (HsIPName fs)
  = let nstr = unpackFS fs
     in do rng <- asks contRange
           let rng' = mkSrcSpan (updateCol (+1) (srcSpanStart rng)) (srcSpanEnd rng)
           annContNoSema (AST.UImplicitName <$> annLoc (createImplicitNameInfo nstr) (pure rng')
                                                  (AST.nameFromList <$> trfNameStr (isOperatorStr nstr) nstr))

isOperatorStr :: String -> Bool
isOperatorStr = any (not . isAlphaNum)

trfQualifiedName :: TransformName n r => Bool -> Located n -> Trf (Ann AST.UQualifiedName (Dom r) RangeStage)
trfQualifiedName isOperator (L l n) = focusOn l $ trfQualifiedNameFocus isOperator n

trfQualifiedNameFocus :: TransformName n r => Bool -> n -> Trf (Ann AST.UQualifiedName (Dom r) RangeStage)
trfQualifiedNameFocus isOperator n
  = do rng <- asks contRange
       let rng' = if isOperator == isSymOcc (occName n) then rng
                    else mkSrcSpan (updateCol (+1) (srcSpanStart rng)) (updateCol (subtract 1) (srcSpanEnd rng))
       annLoc (createNameInfo (transformName n)) (pure rng') (trfQualifiedName' n)

trfQualifiedName' :: TransformName n r => n -> Trf (AST.UQualifiedName (Dom r) RangeStage)
trfQualifiedName' n = AST.nameFromList <$> ((if isSymOcc (occName n) then trfOperatorStr else trfNameStr) False =<< correctNameString n)

trfOperatorStr :: Bool -> String -> Trf (AnnListG AST.UNamePart (Dom r) RangeStage)
trfOperatorStr isInParen str = do rng <- correctSpan <$> asks contRange
                                  makeList "." (pure $ srcSpanStart rng)
                                               (pure [Ann (noSemaInfo $ AST.NodeSpan rng) (AST.UNamePart str)])
  where correctSpan sp = if isInParen then mkSrcSpan (updateCol (+1) (srcSpanStart sp))
                                                     (updateCol (subtract 1) (srcSpanEnd sp))
                                      else sp

-- | Creates a qualified name from a name string
trfNameStr :: Bool -> String -> Trf (AnnListG AST.UNamePart (Dom r) RangeStage)
trfNameStr isInBackticks str = makeList "." atTheStart (trfNameStr' str . correct <$> atTheStart)
  where correct = if isInBackticks then updateCol (+1) else id

trfNameStr' :: String -> SrcLoc -> [Ann AST.UNamePart (Dom r) RangeStage]
trfNameStr' str startLoc = fst $
  foldl (\(r,loc) np -> let nextLoc = advanceAllSrcLoc loc np
                         in ( r ++ [Ann (noSemaInfo $ AST.NodeSpan (mkSrcSpan loc nextLoc)) (AST.UNamePart np)], advanceAllSrcLoc nextLoc "." ) )
  ([], startLoc) (splitOn "." str)
  where -- | Move the source location according to a string
        advanceAllSrcLoc :: SrcLoc -> String -> SrcLoc
        advanceAllSrcLoc (RealSrcLoc rl) str = RealSrcLoc $ foldl advanceSrcLoc rl str
        advanceAllSrcLoc oth _ = oth

trfFastString :: Located FastString -> Trf (Ann AST.UStringNode (Dom r) RangeStage)
trfFastString = trfLocNoSema $ pure . AST.UStringNode . unpackFS
