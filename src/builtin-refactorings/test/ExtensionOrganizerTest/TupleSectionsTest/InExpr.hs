{-# LANGUAGE MultiWayIf,
             TupleSections
             #-}

module InExpr where

import Definitions

{-# ANN module "HLint: ignore Redundant lambda" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}

x1 = (0,) : [(0,)]  {-* TupleSections, TupleSections *-}

x2 = - (fst $ (0,) 6) {-* TupleSections *-}

f1 g = g (0,)  {-* TupleSections *-}

f2 g h = h g (0,)  {-* TupleSections *-}

f3 = \x -> fst . (0,)  {-* TupleSections *-}

x3 = let x = (0,) 0 in x {-* TupleSections *-}

x4 = if fst $ (True,) 0   {-* TupleSections *-}
       then (0,)          {-* TupleSections *-}
       else (,0)          {-* TupleSections *-}

x5 = if | fst $ (True,) 0 -> 20             {-* TupleSections *-}
        | snd $ (,True) 0 -> fst $ (0,) 5   {-* TupleSections, TupleSections *-}
        | otherwise -> fst $ (0,) 5         {-* TupleSections, MultiWayIf *-}

x6 = case fst $ ([],) () of  {-* TupleSections *-}
       [] -> (0,)             {-* TupleSections *-}
       xs -> (0,)             {-* TupleSections *-}

x7 = ((0,) , 5)       {-* TupleSections *-}

x8 = ((0,),(0,))         {-* TupleSections, TupleSections *-}

x9 = ([(0,)])      {-* TupleSections *-}

f4 = ((fst $ (0,) 5) +)   {-* TupleSections *-}

f5 = (+ (fst $ (0,) 5))   {-* TupleSections *-}

x10 = Rec { tup = (0,) 0 }    {-* TupleSections *-}

x11 r = r { tup = (0,) 0 }    {-* TupleSections *-}

x12 = [fst $ (0,) 0, fst $ (2,) 0 .. fst $ (10,) 0 ]    {-* TupleSections, TupleSections, TupleSections *-}

x13 = ((0,) 0) :: (Int,Int)   {-* TupleSections *-}

-- x14 = (\case {_ -> 5}) @Int ((\case {_ -> 5}) 5)
