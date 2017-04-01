# Known limitation

- Currently all modules are loaded into a shared GHC session. This may be changed in the future if different GHC sessions can work in the same process without interfering with each other.
- Some semantic information is missing, for example, kinds of type variables, types of variables local to TH splices, fixity of locally defined operators.
  - We don't have the type of each expression. To acquire it, we would need to re-type check the expressions (with the knowledge of the type of names inside).

