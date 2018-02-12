{-# LANGUAGE ParallelListComp,
             MultiWayIf,
             TupleSections
             #-}

module InExpr where

import Definitions

{-# ANN module "HLint: ignore Redundant lambda" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Redundant if" #-}

x1 = [ (x,y) | x <- [1..10] | y <- [1..10] ] : [[ (x,y) | x <- [1..10] | y <- [1..10] ]]  {-* ParallelListComp, ParallelListComp *-}

x2 = length ([ (x,y) | x <- [1..10] | y <- [1..10] ]) {-* ParallelListComp *-}

f1 g = g [ (x,y) | x <- [1..10] | y <- [1..10] ]  {-* ParallelListComp *-}

f2 g h = h g ([ (x,y) | x <- [1..10] | y <- [1..10] ])  {-* ParallelListComp *-}

f3 = \z -> [ (x,y) | x <- [1..10] | y <- [1..10] ]  {-* ParallelListComp *-}

x3 = let z = [ (x,y) | x <- [1..10] | y <- [1..10] ] in z {-* ParallelListComp *-}

x4 = if null [ (x,y) | x <- [1..10] | y <- [1..10] ]     {-* ParallelListComp *-}
       then ([ (x,y) | x <- [1..10] | y <- [1..10] ])    {-* ParallelListComp *-}
       else ([ (x,y) | x <- [1..10] | y <- [1..10] ])    {-* ParallelListComp *-}

x5 = if | null [ (x,y) | x <- [1..10] | y <- [1..10] ] -> []                    {-* ParallelListComp *-}
        | null [ (x,y) | x <- [1..10] | y <- [1..10] ] -> [ (x,y) | x <- [1..10] | y <- [1..10] ]   {-* ParallelListComp, ParallelListComp *-}
        | otherwise -> [ (x,y) | x <- [1..10] | y <- [1..10] ]               {-* ParallelListComp, MultiWayIf *-}

x6 = case [ (x,y) | x <- [1..10] | y <- [1..10] ] of  {-* ParallelListComp *-}
       [] -> [ (x,y) | x <- [1..10] | y <- [1..10] ]             {-* ParallelListComp *-}
       xs -> [ (x,y) | x <- [1..10] | y <- [1..10] ]             {-* ParallelListComp *-}

x7 = ([ (x,y) | x <- [1..10] | y <- [1..10] ], 5)       {-* ParallelListComp *-}

x8 = ([ (x,y) | x <- [1..10] | y <- [1..10] ],)         {-* ParallelListComp, TupleSections *-}

x9 = ([[ (x,y) | x <- [1..10] | y <- [1..10] ]])      {-* ParallelListComp *-}

f4 = (([ (x,y) | x <- [1..10] | y <- [1..10] ]) ++)   {-* ParallelListComp *-}

f5 = (++ ([ (x,y) | x <- [1..10] | y <- [1..10] ]))   {-* ParallelListComp *-}

x10 = Rec { num = length [ (x,y) | x <- [1..10] | y <- [1..10] ] }    {-* ParallelListComp *-}

x11 r = r { num = length [ (x,y) | x <- [1..10] | y <- [1..10] ] }    {-* ParallelListComp *-}

x12 = [(length [ (x,y) | x <- [1..10] | y <- [1..10] ]) .. (length [ (x,y) | x <- [1..10] | y <- [1..10] ]) ]    {-* ParallelListComp, ParallelListComp *-}

x13 = (length [ (x,y) | x <- [1..10] | y <- [1..10] ]) :: Int   {-* ParallelListComp *-}

-- x14 = (\case {_ -> 5}) @Int ((\case {_ -> 5}) 5)
