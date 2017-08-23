# Known limitation

- Currently all modules are loaded into a shared GHC session. This may be changed in the future if different GHC sessions can work in the same process without interfering with each other.
  - Because of that, the tool does not support modules with the same qualified name.
  - Also, the tool applies some flags to more package components than it would be needed.
- The tool does not support packages where some of the haskell code is generated. (By parser generators, hsc files, ...)
- In general the tool does not support preprocessor usage. Preprocessor pragmas for conditional compilation are handled. (They are commonly used for backward compatibility.)
- Literate Haskell (.lhs files) is not supported. 
- Some semantic information is missing, for example, kinds of type variables, types of variables local to TH splices, fixity of locally defined operators.
  - We don't have the type of each expression. To acquire it, we would need to re-type check the expressions (with the knowledge of the type of names inside).
