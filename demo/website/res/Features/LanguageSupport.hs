-- To present that we handle all major extensions (with a few exceptions), in this
-- module we enable and use a few of them.
-- TODO: feel free to experiment on this test module with the refactorings above.

{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, MultiParamTypeClasses
           , FlexibleInstances, PolyKinds, UndecidableInstances, AllowAmbiguousTypes
           , RankNTypes, ScopedTypeVariables, FlexibleContexts, ViewPatterns
           , RecursiveDo, TransformListComp, GADTs
           #-}

module Features.LanguageSupport where

import GHC.TypeLits
import GHC.Exts

-- type level programming, datakinds, type operators

type family l1 :++: l2 where
  '[] :++: l2       = l2
  (e ': r1) :++: l2 = e ': (r1 :++: l2)
  
type family IfThenElse (b :: Bool) (th :: x) (el :: x) :: x where
  IfThenElse True  th el = th
  IfThenElse False th el = el

-- view patterns

viewPats :: (String -> Integer) -> String -> Bool
viewPats f (f -> 4) = True

-- recursive do

recDo = mdo xs <- Just (1:xs)
            return (map negate xs)

-- generalized list comprehensions

genListComp employees 
  = [ (the dept, sum salary)
    | (name, dept, salary) <- employees
    , then group by dept using groupWith
    , then sortWith by (sum salary)
    , then take 5 
    ]

-- GADTs

data Term a where
    Lit    :: Int -> Term Int
    Succ   :: Term Int -> Term Int
    IsZero :: Term Int -> Term Bool
    If     :: Term Bool -> Term a -> Term a -> Term a
    Pair   :: Term a -> Term b -> Term (a,b)

