![Travis](documentation/Haskelltools.png)

[![Travis](https://img.shields.io/travis/haskell-tools/haskell-tools/master.svg)](https://travis-ci.org/haskell-tools/haskell-tools) [![Hackage](https://img.shields.io/hackage/v/haskell-tools-refactor.svg)](http://hackage.haskell.org/package/haskell-tools-refactor) [![stackage LTS](http://stackage.org/package/haskell-tools-refactor/badge/lts)](http://stackage.org/lts/package/haskell-tools-refactor) [![stackage nightly](http://stackage.org/package/haskell-tools-refactor/badge/nightly)](http://stackage.org/nightly/package/haskell-tools-refactor)

The goal of this project is to create developer tools for the functional programming language Haskell. Currently this project is about refactoring Haskell programs. We have a couple of refactorings working, with support for using them in your editor, or programmatically from command line.

[Available in Atom](https://github.com/nboldi/haskell-tools-atom) soon.

[Demo](http://haskelltools.org) We have a live online demo that you can try

# [Installation instructions](documentation/installation.md)
  - The package is available from hackage and stackage
  - `stack install haskell-tools-daemon haskell-tools-cli --resolver=nightly-[current-date]`
  - When we are not yet on the latest GHC, the only way to install the latest version is to clone this repository and `stack install` it. See the stackage nightly badge above.

# User manuals
   - Use in editor: [Atom](https://github.com/nboldi/haskell-tools-atom/blob/master/documentation/user-manual.md), Sublime (Coming soon...)
   - [Official implemented refactorings](documentation/refactorings.md): The detailed description of the officialy refactorings supported by Haskell-tools Refactor.
   - [ht-refact](documentation/ht-refact.md): A command-line refactorer tool for standalone use.
   - [haskell-tools-demo](documentation/haskell-tools-demo.md): An interactive web-based demo tool for Haskell Tools.

# Contribute

## How to contribute to the Haskell-tools project?

If you encounter a problem, [reporting bugs](report-bugs.md) always helps us.

If you want to help us by making new tools, refactorings or improving existings ones, here are some useful resources for you.
 - We have a [general overview](development/framework-overview.md) of the framework, to let you understand the architecture.
 - The [refactoring packages](development/packages.md) describes how the functionality of the framework is distributed between several packages.
 - A collection of [programming tips](development/general-tips.md) may help you use the framework as it was intended.
 - The [project information](development/project-info.md) page tells how to run, test your code, what are the coding and versioning conventions.
 - You can access the API documentation of the [last build](https://haskell-tools.github.io/master/api/index.html), and the [latest release](https://www.stackage.org/nightly/hoogle?q=haskell-tools).

## Write your own refactorings

- Check out the [Tutorials](development/tutorials.md) for the know-how of refactorings. Please check the [reference](https://github.com/nboldi/references/wiki/References-Tutorial) tutorial also.
- [Guide for writing refactorings](development/refactoring-guide.md).
- [Limitations](development/limitations.md)

## Write other tools working with Haskell-tools

(Comming soon...)

## Integrate the tool with your favourite editor.

By implementing a client to handle a simple protocol you can make your favourite editor work with Haskell-tools. Check out the [editor integration](development/editor-integration.md) tutorial.

## Help to improve the framework

[This section](development/framework-improvement.md) is for those of you who want to improve the framework to help your refactorings and tools. The [limitations](development/limitations.md) section could be a good start where to improve the system.


[Known issues and limitations](documentation/limitations.md)

[Guide for reporting bugs](documentation/report-bugs.md)
