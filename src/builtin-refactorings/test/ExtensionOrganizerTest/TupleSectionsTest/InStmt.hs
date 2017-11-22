{-# LANGUAGE TupleSections #-}

module InStmt where

f x = do
  let k = (0,)               {-* TupleSections *-}
  fst $ (Just 5,) 0          {-* TupleSections *-}
  y <- fst $ (Just 5,) 0     {-* TupleSections *-}
  z <- fst $ (Just 5,) 0     {-* TupleSections *-}
  return $ (0,) z            {-* TupleSections *-}
