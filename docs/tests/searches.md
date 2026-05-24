## Feature : Running searches

### Scenario : Tool call for results
  - When I run `bin/septum run --no-config --tool examples/match_files.septum`
  - Then I get
```
[
{
    "command": "match-files",
    "results": [
        docs/tests/basic_command_line.md,
        docs/tests/searches.md
    ]
}
]
```

### Scenario : Tool call with single result
  - When I run `bin/septum run --no-config --tool examples/single_result.septum`
  - Then I get
```
[
{
    "command": "match-contexts",
    "matching_contexts": 1,
    "matching_files": 1,
    "results": [
        {
            "file": "LICENSE",
            "range": [ 1,  10 ],
            "matches": [
                 3
            ],
            "lines": [
                "",
                "                                 Apache License",
                "                           Version 2.0, January 2004",
                "                        http://www.apache.org/licenses/",
                "",
                "   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION",
                "",
                "   1. Definitions.",
                "",
                "      \"License\" shall mean the terms and conditions for use, reproduction,"
            ]
        }
    ]
}
]
```

### Scenario : Tool call with single result, followed by matching files
  - When I run `bin/septum run --no-config --tool examples/single_result_and_match_files.septum`
  - Then I get
```
[
{
    "command": "match-contexts",
    "matching_contexts": 1,
    "matching_files": 1,
    "results": [
        {
            "file": "LICENSE",
            "range": [ 1,  10 ],
            "matches": [
                 3
            ],
            "lines": [
                "",
                "                                 Apache License",
                "                           Version 2.0, January 2004",
                "                        http://www.apache.org/licenses/",
                "",
                "   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION",
                "",
                "   1. Definitions.",
                "",
                "      \"License\" shall mean the terms and conditions for use, reproduction,"
            ]
        }
    ]
},
{
    "command": "match-files",
    "results": [
        LICENSE
    ]
}
]
```

### Scenario : Single result without JSON
  - When I run `bin/septum run --no-config --script examples/single_result_no_json.septum`
  - Then I get
```
Loading commands from: D:\dev\ada\septum\examples\single_result_no_json.septum

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

### Scenario : Tool call with multiple searches
  - When I run `bin/septum run --no-config --tool examples/single_result_multiple_searches.septum`
  - Then I get

```
[
{
    "command": "match-contexts",
    "matching_contexts": 1,
    "matching_files": 1,
    "results": [
        {
            "file": "LICENSE",
            "range": [ 1,  10 ],
            "matches": [
                 3
            ],
            "lines": [
                "",
                "                                 Apache License",
                "                           Version 2.0, January 2004",
                "                        http://www.apache.org/licenses/",
                "",
                "   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION",
                "",
                "   1. Definitions.",
                "",
                "      \"License\" shall mean the terms and conditions for use, reproduction,"
            ]
        }
    ]
},
{
    "command": "match-contexts",
    "matching_contexts": 1,
    "matching_files": 1,
    "results": [
        {
            "file": "LICENSE",
            "range": [ 1,  10 ],
            "matches": [
                 3
            ],
            "lines": [
                "",
                "                                 Apache License",
                "                           Version 2.0, January 2004",
                "                        http://www.apache.org/licenses/",
                "",
                "   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION",
                "",
                "   1. Definitions.",
                "",
                "      \"License\" shall mean the terms and conditions for use, reproduction,"
            ]
        }
    ]
}
]
```