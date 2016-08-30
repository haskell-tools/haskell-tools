module Features.LayoutSensitive where

-- This should be our main program. 
-- TODO: Rename it to main with [Ctrl, Alt and/or Shift + R]!
-- Notice how the indentation is kept.
a = do putStrLn "What is your name?"
       name <- readLn
       putStrLn $ "Hello, " ++ name ++ "!"
