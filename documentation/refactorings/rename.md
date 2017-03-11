# Rename definition refactoring

To rename a definition, select one of the occurrences. All occurrences (even in other modules) of the selected name will be renamed. Every Haskell definition (function, type, variable, constructor, field, typeclass, etc...) can be renamed. The selected name doesn't have to be in the definition of the renamed thing, it can be a simple usage of it.

```
f :: (Num a, Integral a) => a -> a -> a
f a b = (a + b) `div` 2

g :: Point -> Point -> Point
g p1 p2 = Point (f (x p1) (x p2)) (f (y p1) (y p2))
```

After renaming `f` to `avg` all occurrences are replaced:

```
avg :: (Num a, Integral a) => a -> a -> a
avg a b = (a + b) `div` 2

g :: Point -> Point -> Point
g p1 p2 = Point (avg (x p1) (x p2)) (avg (y p1) (y p2))
```

Renaming requires the new name as a parameter. The new name must be applicable to the renamed definition. For example, value definitions can only be renamed to names starting with lowercase letter. Operators can be only renamed to other operators.

It is checked that the renaming does not cause name conflicts. So you cannot rename a definition to shadow a used or imported definition.

The refactoring can only be performed if the renamed definition is loaded to the Haskell-tools session. So don't try to rename definitions imported from installed packages.

Please mind that renaming a definition can change the API of your package and cause problems for your users.

You can rename modules with the rename definition refactoring. The source file of the module will be renamed and placed into the correct folder.
