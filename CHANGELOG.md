# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
but adds emojis.

Types of changes:

- ✅ `Added` for new features.
- 🔄 `Changed` for changes in existing functionality.
- ⚠️ `Deprecated` for soon-to-be removed features.
- ❌ `Removed` for now removed features.
- 🛠️ `Fixed` for any bug fixes.
- 💥💥💥 `Security` in case of vulnerabilities. Triple 💥 for emphasis.

## [0.1.0]

- 🔄 Set `enable-auto-search` by default in `septum init`.
- 🛠️ `drop` with filter numbers now runs auto-search when enabled.
- 🛠️ Fixed problem where `run` command doesn't verify the terminal environment is set up properly.
- ✅ Added support for '#' comments and empty lines in scripts.

## [0.0.7]

- ✅ Added useful startup commands to .septum/config when running `init`
- ✅ Added preliminary mac support.
- 🛠️ Fixed some linux issues.

## [0.0.6]

- ✅ Added coloration of file names in search results.

## [0.0.5]

- 🛠️ Fixed crash when tab completing an empty command.

## [0.0.4]

- 🔄 **BREAKING CHANGE!**  config file name from `.config` to `config`.
- ✅ Added `drop` command to remove filters out of order.
- ✅ Added `reorder` command to change filter application order.
- ✅ Added filter list to the prompt.
- ✅ Added up arrow to scroll through history.
- 🛠️ Fixed jumping/blinking cursor when predicting commands.
- 🛠️ Fixed jumping/blinking cursor in progress update.

## [0.0.3]

- 🛠️ Fixed tab crash on Linux.
- 🛠️ Fixed case of slow output in certain Linux terminals.

## [0.0.1-beta3]

- ✅ Added `test` command to see which filters will match.
- ✅ Added `--version` command to print the executable version.
- ✅ Added duration reporting and progress spinners to search.
- 🛠️ Fixed `source` command to prevent cyclic inclusion of scripts.
- 🛠️ Fixed issue where max results would be ignored.
- 🛠️ Fixed issue where `pop` would crash.

## [0.0.1-beta2]

- ✅ Added pinning of load and search tasks to CPUs.
- ✅ Added tab completion for directories in `add-dirs`.
- ✅ Added coloration of regular expressions: 🔴red🔴 when invalid, and 🟢green🟢 when valid.
- ✅ Added progress indication during searches.
- ✅ Added `source` command to run commands from file.
- ✅ Added program termination if UTF-8 or VT100 cannot be enabled.
- 🔄 Changed completions to sort lexicographically.
- 🔄 Changed `match-contexts` to accept optional `first` argument.
- ❌ Removed dependency on GNATColl.
- ❌ Removed dependency on `Ada.Directories.Hierarchical_File_Names`.
- 🛠️ Fixed Regex filter display to show "Regex".

## [0.0.1-beta]

- ✅ Added hinting for commands.
- ✅ Added tab-completion for commands.
- ✅ Added coloration of matching lines with `enable-line-colors`.
- 🛠️ Fixed crash bug on pasting text into input.
- 🛠️ Fixed crash bug on existing input when prompt shows up.

## [0.0.1-alpha11]

- ✅ Added input coloration. Commands run 🔴red🔴 when invalid, 🟡yellow🟡 when matching a valid prefix, and 🟢green🟢 when correct.
- ✅ Added input coloration. Paths turn 🔵blue🔵 when valid.

## [0.0.1-alpha10]

- ✅ Added `find-like` and `exclude-like` for case-insensitive search.
- ✅ Added internal crate under tests/ using Trendy Test for tests.
- 🛠️ Fixed auto-search to not always when disabled.
- 🛠️ Fixed crash when no .septum/ folder exists in starting directory.
