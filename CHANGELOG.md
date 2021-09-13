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
