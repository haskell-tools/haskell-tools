# How to report bugs of Haskell-tools?

  - First check if you are using a non-supported language option: [limitations](limitations.md). A likely candidate is preprocessor usage, that could cause a variety of errors.
  - Check if the problem already has an [issue](https://github.com/haskell-tools/haskell-tools/issues). You can use keywords to search.
  - The problems related to the editor plugins supporting Haskell-tools should be reported in the repository of the editor plugin.

Don't forget to attach the source files you used the refactor on. If possible please provide a minimal example on which the problem can be reproduced.

  - If a refactor outputs wrong code also attach that to the issue.
  - If the error can only be reproduced by a number of steps, that describe them in the ticket.
