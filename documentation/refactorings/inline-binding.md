# Inline binding Refactoring

The inline binding refactoring removes the selected binding and replaces all its uses with the right-hand side of the definition. It can be used the simplify the program if there are a lot of definitions that are only used once. Inline binding should only be used for simple functions that are only used locally.

Here the plus operation is only a new name for the `+` operator:

```
plus :: Int -> Int -> Int
plus = (+)

calculator :: Int -> Int -> IO ()
calculator a b = putStrLn $ "a+b = " ++ show (plus a b)
```

Inlining it will result a much simpler program (of course, the `+` operator should be used infix):

```
plus :: Int -> Int -> Int
plus = (+)

calculator :: Int -> Int -> IO ()
calculator a b = putStrLn $ "a+b = " ++ show ((+) a b)
```

This is the opposite of the [Extract Binding](extract-binding.md) refactoring. Extracting and inlining a binding should result the same, not considering a few added parentheses.

Inline binding cannot remove recursive functions.

Inline binding will not remove functions that are not used anywhere. They may be used by the API, or otherwise can be safely deleted by the user.
