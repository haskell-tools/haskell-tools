{-# LANGUAGE Arrows #-}

module Basic where

import Control.Arrow (returnA)

idA :: a -> a
idA = proc a -> returnA -< a  {-* Arrows *-}

plusOne :: Int -> Int
plusOne = proc a -> returnA -< (a+1)  {-* Arrows *-}

plusFive =
 proc a -> do b <- plusOne -< a {-* Arrows *-}
              c <- plusOne -< b {-* Arrows *-}
              d <- plusOne -< c {-* Arrows *-}
              e <- plusOne -< d {-* Arrows *-}
              plusOne -< e      {-* Arrows, Arrows *-}
