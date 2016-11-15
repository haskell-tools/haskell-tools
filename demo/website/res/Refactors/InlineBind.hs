module Refactors.InlineBind where

-- TODO: use [Ctrl, Alt and/or Shift + I] to inline the selected definition. Inline plus, plusPlus and plusTup.

plus :: Int -> Int -> Int
plus = (+)

minus :: Int -> Int -> Int
minus a b = a - b

plusTup :: (Int, Int) -> Int
plusTup (a,b) = a + b

calculator :: Int -> Int -> IO ()
calculator a b = do
  putStrLn $ "a+b = " ++ show (plus a b)
  putStrLn $ "a-b = " ++ show (minus a b)
  putStrLn $ "(a+b) = " ++ show (plusTup (a,b))