{-# LANGUAGE MultiWayIf,
             TupleSections
             #-}

module InExpr where

import Definitions

{-# ANN module "HLint: ignore Redundant lambda" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}

x1 = (if | True -> 0 | False -> 1) : [if | True -> 0 | False -> 1]  {-* MultiWayIf, MultiWayIf *-}

x2 = - ((if | True -> id | False -> id) 6) {-* MultiWayIf *-}

f1 g = g (if | True -> 0 | False -> 1)  {-* MultiWayIf *-}

f2 g h = h g (if | True -> 0 | False -> 1)  {-* MultiWayIf *-}

f3 = \x -> if | True -> 0 | False -> 1  {-* MultiWayIf *-}

x3 = let x = (if | True -> id | False -> id) 5 in x {-* MultiWayIf *-}

x4 = if (if | True -> id | False -> id) True       {-* MultiWayIf *-}
       then (if | True -> 0 | False -> 1)       {-* MultiWayIf *-}
       else (if | True -> 0 | False -> 2)       {-* MultiWayIf *-}

x6 = case (if | True -> [] | False -> []) of  {-* MultiWayIf *-}
       [] -> ()            
       xs -> ()

x7 = (if | True -> 0 | False -> 1, 5)       {-* MultiWayIf *-}

x8 = (if | True -> 0 | False -> 1,)         {-* MultiWayIf, TupleSections *-}

x9 = ([(if | True -> 0 | False -> 1)])      {-* MultiWayIf *-}

f4 = ((if | True -> 0 | False -> 1) +)   {-* MultiWayIf *-}

f5 = (+ (if | True -> 0 | False -> 1))   {-* MultiWayIf *-}

x10 = Rec { num = (if | True -> 0 | False -> 1) }    {-* MultiWayIf *-}

x11 r = r { num = (if | True -> 0 | False -> 1) }    {-* MultiWayIf *-}

x12 = [(if | True -> 0 | False -> 1), (if | True -> 0 | False -> 1) .. (if | True -> 0 | False -> 1) ]    {-* MultiWayIf, MultiWayIf, MultiWayIf *-}

x13 = (if | True -> 0 | False -> 1) :: Int   {-* MultiWayIf *-}
