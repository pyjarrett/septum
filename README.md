# septum

[![Build Status](https://github.com/pyjarrett/septum/actions/workflows/ada.yml/badge.svg)](https://github.com/pyjarrett/septum/actions)

Context-based code search tool

## What does this do?

Septum is like `grep`, but searches for matching contexts of contiguous lines,
rather than just single lines.

## Why does this exist?

Finding what you need in large codebases is hard.  Sometimes terms have multiple
meanings in different parts of the project, and figuring out what you're looking
for needs to be done in an incremental fashion.

Septum is a stack of applied search filters that you can push and pop interactively
as you search.

## Example

      > add-dirs D:/dev/calendon/src
      > find-regex ^\s*#\s*if
      > find-text clang
      > exclude-text pop
      > match-contexts

       D:\dev\calendon\src\tests\unit\test-utf8.c
       1        #include <calendon/test.h>
       2
       3        #include <calendon/utf8.h>
       4
       5        #include <stdlib.h>
       6        #include <stdio.h>
       7
    -> 8        #if defined(__clang__)
    -> 9            #pragma clang diagnostic push
    -> 10           #pragma clang diagnostic ignored "-Wpointer-sign"
       11       #endif
       12
       13       CN_TEST_SUITE_BEGIN("UTF-8")
       14
       15           CN_TEST_UNIT("Bytes in UTF-8 code point") {

Commands can be abbreviated, e.g. `find-regex` can be abbreviated as `find-r`.

# Installation

> Septum is currently available as a prerelease beta.

**Windows Chocolatey** users can it in the [Chocolatey Community Repository](https://community.chocolatey.org/packages/septum/):

```powershell
choco install septum --pre
```

# Design principles

septum shall:

- provide features to help search large codebases
- aim to be performant
- only read the search tree, except for .septum/ directories

# Contributing

Septum aims to help every developers everywhere.  You're encouraged to recommend
features, report bugs, or submit pull requests.

# License

Septum is released under the [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0)
