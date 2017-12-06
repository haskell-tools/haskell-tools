module Type.Ctx where

sh :: Show a => a -> String
sh = show

sh' :: (Show a) => a -> String
sh' = show

sh'' :: (Show a, Eq a) => a -> String
sh'' = show