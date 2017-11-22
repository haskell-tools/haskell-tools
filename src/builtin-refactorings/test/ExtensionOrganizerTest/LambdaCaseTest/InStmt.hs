{-# LANGUAGE LambdaCase #-}

module InStmt where

f x = do
  let k = \case {_ -> 5}          {-* LambdaCase *-}
  (\case {_ -> Just ()}) 5        {-* LambdaCase *-}
  y <- (\case {_ -> Just 5}) x    {-* LambdaCase *-}
  z <- (\case {_ -> Just 5}) y    {-* LambdaCase *-}
  return $ (\case {_ -> 5}) z     {-* LambdaCase *-}
