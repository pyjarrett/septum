## Feature : Running searches

### Scenario : Single result
  - When I run `bin/septum run examples/single_result.septum`
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