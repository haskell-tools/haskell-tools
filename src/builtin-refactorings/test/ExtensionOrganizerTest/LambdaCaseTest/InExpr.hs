{-# LANGUAGE LambdaCase,
             MultiWayIf,
             TupleSections
             #-}

module InExpr where

import Definitions

{-# ANN module "HLint: ignore Redundant lambda" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}

x1 = (\case {() -> ()}) : [\case {() -> ()}]  {-* LambdaCase, LambdaCase *-}

x2 = - ((\case {_ -> 5}) 6) {-* LambdaCase *-}

f1 g = g (\case {_ -> ()})  {-* LambdaCase *-}

f2 g h = h g (\case {_ -> ()})  {-* LambdaCase *-}

f3 = \x -> \case {_ -> ()}  {-* LambdaCase *-}

x3 = let x = (\case {_ -> ()}) 5 in x {-* LambdaCase *-}

x4 = if (\case {_ -> True}) 5       {-* LambdaCase *-}
       then (\case {_ -> True})     {-* LambdaCase *-}
       else (\case {_ -> False})    {-* LambdaCase *-}

x5 = if | (\case {_ -> False}) 5 -> 20                    {-* LambdaCase *-}
        | (\case {_ -> True}) 5 -> (\case {_ -> 10}) ()   {-* LambdaCase, LambdaCase *-}
        | otherwise -> (\case {_ -> 10}) ()               {-* LambdaCase *-} --MultiWayIf

x6 = case (\case {_ -> [1..10]}) () of  {-* LambdaCase *-}
       [] -> \case {_ -> 5}             {-* LambdaCase *-}
       xs -> \case {_ -> 7}             {-* LambdaCase *-}

x7 = (\case {_ -> ()}, 5)       {-* LambdaCase *-}

x8 = (\case {_ -> ()},)         {-* LambdaCase, TupleSections *-}

x9 = ([(\case {_ -> ()})])      {-* LambdaCase *-}

f4 = (((\case {_ -> 5}) 0) +)   {-* LambdaCase *-}

f5 = (+ ((\case {_ -> 5}) 0))   {-* LambdaCase *-}

x10 = Rec { num = (\case {_ -> 5}) 0 }    {-* LambdaCase *-}

x11 r = r { num = (\case {_ -> 5}) 0 }    {-* LambdaCase *-}

x12 = [(\case {_ -> 1}) 0, ((\case {_ -> 2}) 0) .. ((\case {_ -> 10}) 0) ]    {-* LambdaCase, LambdaCase, LambdaCase *-}

x13 = ((\case {_ -> 5}) 0) :: Int   {-* LambdaCase *-}

-- x14 = (\case {_ -> 5}) @Int ((\case {_ -> 5}) 5)
