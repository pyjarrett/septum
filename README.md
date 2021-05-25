# septum

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

# Usage

      add-dirs                 Adds directory to the search list.
      add-exts                 Adds extensions to filter by.
      clear-dirs               Removes all directories from the search list.
      clear-filters            Pops all filters.
      disable-line-numbers     Disables prefixing of lines with line numbers.
      enable-line-numbers      Enables prefixing of lines with line numbers.
      exclude-regex            Adds regex to exclude.
      exclude-text             Adds text to exclude.
      exit                     Exits the search program.
      find-regex               Adds filter regex.
      find-text                Adds filter text.
      help                     Print commands or help for a specific command
      list-dirs                List the directories in the search list.
      list-exts                List current extensions.
      list-filters             Lists all applied filters.
      matching-contexts        Lists contexts matching the current filter.
      pop                      Pops the last applied filter.
      quit                     Exits the search program.
      reload                   Reloads the file cache.
      remove-exts              Removes extensions from the search.
      set-context-width        Sets the width of the context in which to find matches.
      set-max-results          Sets the maximum results returned before only the total number of results are returned.
      stats                    Print file cache statistics.

## Example

      > add-dirs D:/dev/calendon/src
      > find-regex ^\s*#\s*if
      > find-text clang
      > exclude-text pop
      > matching-contexts

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

> Septum is currently available as a prerelease alpha.

**Windows Chocolatey** users can it in the [Chocolatey Community Repository](https://community.chocolatey.org/packages/septum/):

```powershell
choco install septum --pre
```
