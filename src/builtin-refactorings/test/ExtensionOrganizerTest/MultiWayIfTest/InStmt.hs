{-# LANGUAGE MultiWayIf #-}

module InStmt where

f x = do
  let k = if | True -> 0 | False -> 1          {-* MultiWayIf *-}
  if | True -> Just 0 | False -> Just 1        {-* MultiWayIf *-}
  y <- if | True -> Just 0 | False -> Just 1   {-* MultiWayIf *-}
  z <- if | True -> Just 0 | False -> Just 1   {-* MultiWayIf *-}
  return $ if | True -> 0 | False -> 1         {-* MultiWayIf *-}
