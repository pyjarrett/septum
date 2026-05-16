## Feature : Command line

### Scenario : Version message check
  - When I run `bin/septum version`
  - Then I get no error
  - Then the output is `septum v0.2.1`

### Scenario : Printing help information
  - When I run `bin/septum help`
  - Then I get no error

### Scenario : Run multiple noop programs
  - When I run `bin/septum run examples/enable_everything.septum examples/disable_everything.septum`
  - Then I get no error

### Scenario : Comment & empty lines
  - When I run `bin/septum run examples/commented_and_empty_lines.septum`
  - Then I get no error

### Scenario : Interactive only allowed in tty
  - When I run `bin/septum`
  - Then I get error
