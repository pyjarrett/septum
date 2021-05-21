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


# Example

    > add-dirs D:/dev/calendon/src
    
    Loading with 12 tasks.
    Added D:/dev/calendon/src to search path.
    
     > find-regex ^\s*#\s*if
     > find-text clang
     > exclude-text pop 
     > matching-contexts
    
    D:\dev\calendon\src\calendon\compat-spng.h
       2        #define CN_COMPAT_SPNG_H
       3
    -> 4        #ifdef __GNUC__
       5            #pragma GCC diagnostic push
       6            #pragma GCC diagnostic ignored "-Wdiscarded-qualifiers"
       7        #endif
       8
    -> 9        #ifdef __clang__
    -> 10           #pragma clang diagnostic push
    -> 11           #pragma clang diagnostic ignored "-Wincompatible-pointer-types-discards-qualifiers"
       12       #endif
       13
    -> 14       #ifdef _MSC_VER
       15           #pragma warning(push)
       16           // "different 'const' qualifiers"
       
       D:\dev\calendon\src\calendon\utf8.h
       24        * represent a single UTF code point.
       25        */
       26
       27       #include <calendon/cn.h>
       28
       29       #include <stdio.h>
       30
    -> 31       #if defined(__clang__)
    -> 32           #pragma clang diagnostic push
    -> 33           #pragma clang diagnostic ignored "-Wpointer-sign"
       34       #endif
       35
    -> 36       #ifdef __cplusplus
       37       extern "C" {
       38       #endif
       
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
