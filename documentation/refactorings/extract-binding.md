# Extract binding Refactoring

The extract binding can be used to reduce the complexity of an expression by introducing a local definition. It can also improve the clarity of the definition by giving descriptive names to the sub-expressions. The definition will be placed in the `where` section of the nearest containing binding.

Any used local definitions, parameters and implicitely passed values will be explicitely passed to the new function.

```
combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
                         ++ [ (ys,x:zs) | (ys,zs) <- combination n xs ]
```

Extracting the two part of the last right hand side gives us:

```
combination n (x:xs) = currentSelected ++ currentNotSelected
  where currentSelected = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
        currentNotSelected = [ (ys,x:zs) | (ys,zs) <- combination n xs ]
```

The refactoring requires a new name for the extracted definition. The new name should not be the same as of another definition in the same scope.

It does not automatically find equivalent or similar expressions, but after extracting them you can replace other occurrences of the expression with the binding or generalize the generated local definition.

You can apply the [Float out](float-out.md) refactor to promote the generated binding into a top-level binding if you want to use it in other definitions.
