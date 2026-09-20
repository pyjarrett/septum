## Feature : Filters

### Scenario : Reordering filters
  - When I run `bin/septum run --no-config examples/reorder_filters.septum`
  - Then I get
```

 > find-like string unbounded return function


 > list-line-filters

KEEP : Case Insensitive Match "STRING"
KEEP : Case Insensitive Match "UNBOUNDED"
KEEP : Case Insensitive Match "RETURN"
KEEP : Case Insensitive Match "FUNCTION"

 > reorder 4 3 1 2


 > list-line-filters

KEEP : Case Insensitive Match "FUNCTION"
KEEP : Case Insensitive Match "RETURN"
KEEP : Case Insensitive Match "STRING"
KEEP : Case Insensitive Match "UNBOUNDED"

 > reorder 3 3 3 3

Index appears multiple times:  3
Command failed: reorder 3 3 3 3
```

### Scenario : Dropping line filters
  - When I run `bin/septum run --no-config examples/drop_line_filters.septum`
  - Then I get
```

 > find-like string unbounded return function


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

 > drop 5

Drop indices must be valid line filter indices.
Command failed: drop 5
```

### Scenario : Dropping a non-existent line filter
  - When I run `bin/septum run --no-config examples/drop_line_filters_fail.septum`
  - Then I get
```

 > find-like string unbounded return function


 > list-line-filters

KEEP : Case Insensitive Match "STRING"
KEEP : Case Insensitive Match "UNBOUNDED"
KEEP : Case Insensitive Match "RETURN"
KEEP : Case Insensitive Match "FUNCTION"

 > drop 2 3 5

Drop indices must be valid line filter indices.
Command failed: drop 2 3 5
```

### Scenario : Dropping path filters
  - When I run `bin/septum run --no-config examples/drop_path_filters.septum`
  - Then I get

```

 > exclude-path .git .config temp/


 > find-any-path .cpp


 > list-path-filters

EXCLUDE : Case Sensitive Match ".git"
EXCLUDE : Case Sensitive Match ".config"
EXCLUDE : Case Sensitive Match "temp/"
KEEP : Case Sensitive Match ".cpp"

 > drop-path-filters 2 3

Dropping filter: Case Sensitive Match "temp/"
Dropping filter: Case Sensitive Match ".config"

 > list-path-filters

EXCLUDE : Case Sensitive Match ".git"
KEEP : Case Sensitive Match ".cpp"

 > drop-path-filters

Dropping filter: Case Sensitive Match ".cpp"
```

### Scenario : Popping path filters
  - When I run `bin/septum run --no-config examples/pop_path_filters.septum`
  - Then I get

```

 > exclude-path .git .config temp


 > find-any-path .cpp


 > list-path-filters

EXCLUDE : Case Sensitive Match ".git"
EXCLUDE : Case Sensitive Match ".config"
EXCLUDE : Case Sensitive Match "temp"
KEEP : Case Sensitive Match ".cpp"

 > pop-path-filters

Dropping filter: Case Sensitive Match ".cpp"

 > list-path-filters

EXCLUDE : Case Sensitive Match ".git"
EXCLUDE : Case Sensitive Match ".config"
EXCLUDE : Case Sensitive Match "temp"

 > pop-path-filters

Dropping filter: Case Sensitive Match "temp"

 > list-path-filters

EXCLUDE : Case Sensitive Match ".git"
EXCLUDE : Case Sensitive Match ".config"

 > pop-path-filters

Dropping filter: Case Sensitive Match ".config"

 > list-path-filters

EXCLUDE : Case Sensitive Match ".git"

 > pop-path-filters

Dropping filter: Case Sensitive Match ".git"

 > list-path-filters


 > pop-path-filters

There are no filters to pop.
```