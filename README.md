# septum

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/septum.json)](https://alire.ada.dev/crates/septum.html)
[![Build Status](https://github.com/pyjarrett/septum/actions/workflows/ada.yml/badge.svg)](https://github.com/pyjarrett/septum/actions)
[![Test Status](https://github.com/pyjarrett/septum/actions/workflows/unit-tests.yml/badge.svg)](https://github.com/pyjarrett/septum/actions)

Context-based code search tool

## What does this do?

Septum is like `grep`, but searches for matching contexts of contiguous lines,
rather than just single lines.

Limiting the search into blocks around search terms allows searching for elements
in arbitrary order which may span across lines, in a way which can be difficult
to express in other tools. Sometimes terms appear multiple times in a project and
have names which change based on context. Septum allows exclusion of these contexts.

## Why does this exist?

Finding what you need in large codebases is hard.  Sometimes terms have multiple
meanings in different parts of the project, and figuring out what you're looking
for needs to be done in an incremental fashion.

Septum provides an interactive environment to push and pop search filters
to narrow or expand a search.

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

# Building

1. This project requires a recent release of the [Alire](https://github.com/alire-project/alire/releases) tool to build.
2. Install a toolchain.

```bash
alr toolchain --select
```
   
3. Build

```bash
alr build
```

4. Executable should be at `bin/septum(.exe)`

# Installation

> Septum is currently available as a prerelease beta.

**Windows Chocolatey** users can it in the [Chocolatey Community Repository](https://community.chocolatey.org/packages/septum/):

```powershell
choco install septum --version=0.0.4
```

⚠️ The 0.0.4 version on Chocolatey has a crash (an internal assert) when trying to tab complete an
empty string.  This is fixed in 0.0.5, but a new version can't be published until 0.0.4 gets through
Chocolatey moderation.  This doesn't affect typical use and a new version published as soon as possible.

# Design principles

septum shall:

- provide features to help search large codebases
- only read the search tree, except for .septum/ directories

# Contributing

Septum aims to help every developers everywhere.  You're encouraged to recommend
features, report bugs, or submit pull requests.

# License

Septum is released under the [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0)
