# Generate type signature refactoring

Generates the type signature of a binding. Top-level signatures should have type signatures, but the refactoring can also be applied to local bindings. The refactoring is useful if the type is complex and is not easy to figure out.

The selection must be inside the binding that needs type signature.

Usually the most generic type signature will be generated, but for top-level definitions, the default rules may apply.

```
x = applyFuns $ map (+) [1..10]
  where applyFuns = foldl (.) id
```

If we give type signature to each:

```
x :: Integer -> Integer
x = applyFuns $ map (+) [1..10]
  where applyFuns :: [c -> c] -> c -> c
        applyFuns = foldl (.) id
```
