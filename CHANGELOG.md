# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
but adds emojis.

Types of changes:

- β `Added` for new features.
- π `Changed` for changes in existing functionality.
- β οΈ `Deprecated` for soon-to-be removed features.
- β `Removed` for now removed features.
- π οΈ `Fixed` for any bug fixes.
- π₯π₯π₯ `Security` in case of vulnerabilities. Triple π₯ for emphasis.

## [0.0.7]

- β Added useful startup commands to .septum/config when running `init`
- β Added preliminary mac support.
- π οΈ Fixed some linux issues.

## [0.0.6]

- β Added coloration of file names in search results.

## [0.0.5]

- π οΈ Fixed crash when tab completing an empty command.

## [0.0.4]

- π **BREAKING CHANGE!**  config file name from `.config` to `config`.
- β Added `drop` command to remove filters out of order.
- β Added `reorder` command to change filter application order.
- β Added filter list to the prompt.
- β Added up arrow to scroll through history.
- π οΈ Fixed jumping/blinking cursor when predicting commands.
- π οΈ Fixed jumping/blinking cursor in progress update.

## [0.0.3]

- π οΈ Fixed tab crash on Linux.
- π οΈ Fixed case of slow output in certain Linux terminals.

## [0.0.1-beta3]

- β Added `test` command to see which filters will match.
- β Added `--version` command to print the executable version.
- β Added duration reporting and progress spinners to search.
- π οΈ Fixed `source` command to prevent cyclic inclusion of scripts.
- π οΈ Fixed issue where max results would be ignored.
- π οΈ Fixed issue where `pop` would crash.

## [0.0.1-beta2]

- β Added pinning of load and search tasks to CPUs.
- β Added tab completion for directories in `add-dirs`.
- β Added coloration of regular expressions: π΄redπ΄ when invalid, and π’greenπ’ when valid.
- β Added progress indication during searches.
- β Added `source` command to run commands from file.
- β Added program termination if UTF-8 or VT100 cannot be enabled.
- π Changed completions to sort lexicographically.
- π Changed `match-contexts` to accept optional `first` argument.
- β Removed dependency on GNATColl.
- β Removed dependency on `Ada.Directories.Hierarchical_File_Names`.
- π οΈ Fixed Regex filter display to show "Regex".

## [0.0.1-beta]

- β Added hinting for commands.
- β Added tab-completion for commands.
- β Added coloration of matching lines with `enable-line-colors`.
- π οΈ Fixed crash bug on pasting text into input.
- π οΈ Fixed crash bug on existing input when prompt shows up.

## [0.0.1-alpha11]

- β Added input coloration. Commands run π΄redπ΄ when invalid, π‘yellowπ‘ when matching a valid prefix, and π’greenπ’ when correct.
- β Added input coloration. Paths turn π΅blueπ΅ when valid.

## [0.0.1-alpha10]

- β Added `find-like` and `exclude-like` for case-insensitive search.
- β Added internal crate under tests/ using Trendy Test for tests.
- π οΈ Fixed auto-search to not always when disabled.
- π οΈ Fixed crash when no .septum/ folder exists in starting directory.
