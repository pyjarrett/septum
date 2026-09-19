## Feature : Filters

### Scenario : Dropping line filters
  - When I run `bin/septum run --no-config --script examples/drop_line_filters.septum`
  - Then I get
```
Loading commands from: D:\dev\ada\septum\examples\drop_line_filters.septum

 > find-like string unbounded return function


 > list-line-filters

KEEP : Case Insensitive Match "STRING"
KEEP : Case Insensitive Match "UNBOUNDED"
KEEP : Case Insensitive Match "RETURN"
KEEP : Case Insensitive Match "FUNCTION"

 > drop 5

No filter exists at that index to drop.

 > list-line-filters

KEEP : Case Insensitive Match "STRING"
KEEP : Case Insensitive Match "UNBOUNDED"
KEEP : Case Insensitive Match "RETURN"
KEEP : Case Insensitive Match "FUNCTION"

 > drop 2 3

Dropping filter: Case Insensitive Match "RETURN"
Dropping filter: Case Insensitive Match "UNBOUNDED"

 > list-line-filters

KEEP : Case Insensitive Match "STRING"
KEEP : Case Insensitive Match "FUNCTION"

 > drop

Dropping filter: Case Insensitive Match "FUNCTION"

 > list-line-filters

KEEP : Case Insensitive Match "STRING"
```
