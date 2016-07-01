# CHANGELOG

  - Extract binding now generates a local binding. Fewer values must be passed as arguments.
  - Extract binding have better checks for when does it need to put values in parentheses.
  - Extract binding creates functions with arguments from lambdas.
  - Imported names are now present in the scope. Rename and extract binding is aware of them and prevents name clashes.
