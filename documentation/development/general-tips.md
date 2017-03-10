# Practical information about the Haskell-tools framework

This section of the documentation gives you some tips about how different parts of the framework can be used.

## Pattern matching and generating the AST

Pattern matching on the Haskell-tools AST is done by pattern synonyms. The synonyms are one-directional, their generator counterparts are named differently. This is because pattern matching and re-generation of the AST fragment does not keep the fragment intact, it removes source and semantic information.

## Handling of comments

## Optional AST elements

## AST elements with multiplicity
