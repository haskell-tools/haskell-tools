{-# LANGUAGE MagicHash,
             BangPatterns,
             ScopedTypeVariables,
             ViewPatterns
             #-}

module InPattern where

-- NOTE: non-exhaustive

data Rec = Rec# { x :: Int }    {-* MagicHash *-}

f1 :: a -> ()
f1 x# = ()                      {-* MagicHash *-}

f2 :: [a] -> ()
f2 (a# : as#) = ()              {-* MagicHash, MagicHash *-}
f2 [x#] = ()                    {-* MagicHash *-}
f2 _ = ()

f3 :: Maybe a -> ()
f3 (Just x#) = ()               {-* MagicHash *-}
f3 _ = ()

f4 :: (a,a) -> ()
f4 (x#, y#) = ()                {-* MagicHash, MagicHash *-}

f5 :: Rec -> ()
f5 Rec#{x = x} = ()             {-* MagicHash *-}

f6 :: a -> ()
f6 x#@y# = ()                   {-* MagicHash, MagicHash *-}

f7 :: a -> ()
f7 !x# = ()                     {-* MagicHash, BangPatterns *-}

f8 :: a -> ()
f8 (x# :: a) = ()               {-* MagicHash *-} -- ScopedTypeVariables

f9 :: a -> ()
f9 (id -> x#) = ()              {-* MagicHash, ViewPatterns *-}
