# Float out refactoring

This refactoring moves a binding into an outer scope. Local definitions can become top-level ones, or local ones in an outer binding. The refactoring only floats one level at a time, so multiple uses may be needed to move a definition to top-level.

The refactoring checks for name collosions because a local definition that shadowed another may become conflicting with it. It is also checked if the definition implicitly uses values defined in the binding it would leave with the transformation. In this case the refactoring is refused (if you really want the binding to be floated out, you should make the values to be passed explicitly, and then use the refactoring).

The type signature is also moved to the outer scope if exists. The refactoring is refused if the definition shares its type signature with other definitions. In these cases you should split the type signatures before the refactoring.
