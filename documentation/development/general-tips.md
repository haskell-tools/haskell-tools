# Practical information about the Haskell-tools framework

This section of the documentation gives you some tips about how different parts of the framework can be used.

## Pattern matching and generating the AST

Pattern matching on the Haskell-tools AST is done by pattern synonyms. The synonyms are one-directional, their generator counterparts are named differently. This is because pattern matching and re-generation of the AST fragment does not keep the fragment intact, it removes source and semantic information.

## Handling of comments

Handling source-code related information consist of white-spaces and comments. White spaces are implicitely attached to the nodes as the source code fragments are attached to their AST element, but comments are outside of these elements. However it is important to attach the comments to their correspondig element. For example, it would be confusing if after reordering some imports, a comment that was before an import would remain at the same place even when the import had been moved to a different line. When comments are attached to their AST elements they move with them and are deleted when the elements are removed.

We get the position and text of comments from GHC parser phase. We attach them to the AST elements in the [PlaceComments](https://github.com/haskell-tools/haskell-tools/blob/master/src/prettyprint/Language/Haskell/Tools/Transform/PlaceComments.hs) module.

When we attach comments to elements we use simple rules to check if a comment belongs to a given element. First we define the AST elements that may have comments. These are:
  - *Decl* (declarations)
  - *TypeSignature* (local ones are not covered by Decl)
  - *ClassElement* (definitions in type classes)
  - *ValueBind* (value, function definitions, local ones are not covered by Decl)
  - *ConDecl*, *GadtConDecl* (constructors)
  - *Type* (uses of types)

The attaching rules respect the haddock conventions:
  - Comments starting with `-- |` are attached to the next element.
  - Comments starting with `-- ^` are attached to the previous element.
  - If a comment is after an element in the same line, than it is attached to this element.
  - A comment is attached to an element if it is in the previous line and is aligned to the same column.

## Handling indentation

Handling indentation in a Haskell module is very important because some language elements depend on it. Some elements restrict the position of their children elements. For example all statements in a do-block must start on the same column.

There is another automatic transformation rule for indentations: When an element is shifted in its line, the children elements are automatically shifted with it.

## Optional AST elements

Optional elements in the AST may or may not contain a child element. They may contain before and after texts. If the optional element was originally empty, but during the transformation it gains a child, the before and after elements are written before and after the inserted element.

## AST elements with multiplicity

If an element in the AST can have any number of children (for example, a list of declarations), we store the separators between the children and the text before and after the list. If the list is empty, we don't pretty print anything for the node. If the list has at least one element, we show the before and after texts. If the list has at least two element, we put a separator between them.

The separators that are present in the original source code are intact. When new element is added, we get the most common separator from the list and use that to separate the new element from the rest. If there are no separators, than a default one will be used that depends on the kind of node we have.

In the [ListOperations](https://github.com/haskell-tools/haskell-tools/blob/master/src/refactor/Language/Haskell/Tools/Refactor/ListOperations.hs) module defines a few useful operations on multiplicity AST elements, for example filtering and inserting new elements.

## Source information

You can get the position of a node in the original source by the `getRange` function. Refactorings should not change any of the source informations in the nodes.

## Semantic information

**Domain**: The collection of the semantic informations added to a given AST is the domain of the AST. The domain decides which AST elements have semantic annotation and what will be the type of these annotations.

The domain of an AST depends on how far GHC can progress with the compilation of the module. If the module is type-correct, its domain will be `IdDom`, if it can be renamed but it is not type correct, its domain will be `Dom Name`, if the module can be parsed but cannot be renamed, the domain will be `Dom RdrName`.

All the semantic information datatypes can be found in the [`SemaInfoTypes` module](https://www.stackage.org/haddock/nightly/haskell-tools-ast/Language-Haskell-Tools-AST-SemaInfoTypes.html). The accessor functions are found in [`SemaInfoClasses`](https://www.stackage.org/haddock/nightly/haskell-tools-ast/Language-Haskell-Tools-AST-SemaInfoClasses.html)

Common semantic information:
  - **Names** (`QualifiedName` AST element): `NameInfo`. It contains the GHC representation of a name and the names that are in scope of that AST element. It also has a flag to decide if the name was defined at that place or elsewhere. Additionally it can contain fixity information if the name is an operator.
  - **Expressions** (`Expr` AST element): `ScopeInfo`. It contains the names that are in scope for that expression.
  - **Imports** (`ImportDecl` AST element): `ImportInfo`. It contains the GHC representation of the module that is imported. All the names that could be imported, and all the names that are actually imported. It also contain some information about typeclass instances loaded.
  - **Module** (`Module` AST element): `ModuleInfo`. Contains the module name, a flag to mark `.hs-boot` modules, and all the implicitely imported definitions. It also contain some information about typeclass instances loaded implicitely.
  - **Record wildcards** (`FieldWildcard` AST element): `ImplicitFieldInfo`. Contains the fields that will be modified or accessed implicitely by the wildcard pattern.
