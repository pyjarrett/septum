## Feature : Running searches

### Scenario : Match files
  - When I run `bin/septum run --no-config examples/match_files.septum`
  - Then I get
```
Loading commands from: D:\dev\ada\septum\examples\match_files.septum

 > clear-dirs


 > clear-files


 > disable-auto-search


 > add-dirs docs/tests

Loading with 32 tasks.


Added docs/tests to search path.

 > find-like septum


 > match-files


docs/tests/basic_command_line.md
docs/tests/filters.md
docs/tests/searches.md

Matching files: 3
```

### Scenario : Single result
  - When I run `bin/septum run --no-config examples/single_result.septum`
  - Then I get
```
Loading commands from: D:\dev\ada\septum\examples\single_result.septum

 > clear-dirs


 > clear-files


 > add-files LICENSE

Added LICENSE to search.

 > find-like Version 2.0, January 2004


 > match-c
Resolved to: match-contexts


LICENSE
        1
        2                                   Apache License
->      3                             Version 2.0, January 2004
        4                          http://www.apache.org/licenses/
        5
        6     TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION
        7
        8     1. Definitions.
        9
       10        "License" shall mean the terms and conditions for use, reproduction,


Matching contexts:  1
Matching files: 1
```

### Scenario : Single result, followed by matching files
  - When I run `bin/septum run --no-config examples/single_result_and_match_files.septum`
  - Then I get
```
Loading commands from: D:\dev\ada\septum\examples\single_result_and_match_files.septum

 > clear-dirs


 > clear-files


 > add-files LICENSE

Added LICENSE to search.

 > find-like Version 2.0, January 2004


 > match-c
Resolved to: match-contexts


LICENSE
        1
        2                                   Apache License
->      3                             Version 2.0, January 2004
        4                          http://www.apache.org/licenses/
        5
        6     TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION
        7
        8     1. Definitions.
        9
       10        "License" shall mean the terms and conditions for use, reproduction,


Matching contexts:  1
Matching files: 1

 > match-files


LICENSE

Matching files: 1
```

### Scenario : Multiple searches
  - When I run `bin/septum run --no-config examples/single_result_multiple_searches.septum`
  - Then I get

```
Loading commands from: D:\dev\ada\septum\examples\single_result_multiple_searches.septum

 > clear-dirs


 > clear-files


 > disable-auto-search


 > add-files LICENSE

Added LICENSE to search.

 > find-like Version 2.0, January 2004


 > match-c
Resolved to: match-contexts


LICENSE
        1
        2                                   Apache License
->      3                             Version 2.0, January 2004
        4                          http://www.apache.org/licenses/
        5
        6     TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION
        7
        8     1. Definitions.
        9
       10        "License" shall mean the terms and conditions for use, reproduction,


Matching contexts:  1
Matching files: 1

 > match-c
Resolved to: match-contexts


LICENSE
        1
        2                                   Apache License
->      3                             Version 2.0, January 2004
        4                          http://www.apache.org/licenses/
        5
        6     TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION
        7
        8     1. Definitions.
        9
       10        "License" shall mean the terms and conditions for use, reproduction,


Matching contexts:  1
Matching files: 1
```