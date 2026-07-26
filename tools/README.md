# Help generation tools

This directory contains a small Python tool that turns markdown help sources into Ada code for Septum’s in-program help (`SP.Help.Header` / `SP.Help.Block` / `Colorize_Command`).

## Purpose

Help topics are authored as simple markdown files (see `help/` at the repo root). `write_help.py` reads those files and emits Ada fragments that can be integrated into the help system, instead of hand-writing long string concatenations in Ada.

Typical flow:

```
help/*.md  →  write_help.py  →  Ada SP.Help.* calls on stdout
```

## Running the tool

From the repository root:

```bash
python tools/write_help.py help/path_filters.md
```

You can pass multiple markdown files; each is processed in order and written to stdout:

```bash
python tools/write_help.py help/topic_a.md help/topic_b.md
```

With no arguments, the tool exits without writing anything.

## Input format

The parser understands a **small markdown subset**, not full CommonMark.

| Construct | Behavior |
|-----------|----------|
| `# Topic name` | Starts a new help topic. The text after `#` (stripped) is the topic title. |
| Blank line | Ends the current paragraph. |
| Wrapped lines | Consecutive non-blank lines are joined with spaces into one paragraph. |
| `` `command` `` | Inline command reference; becomes a colorized Ada call (see below). |
| `"` in text | Escaped for Ada string literals (`""`). |

Rules and edge cases:

- A line is a topic heading only if it **starts with** `#` (leading whitespace before `#` is not treated as a heading).
- Duplicate topic names in the same file raise `ValueError`.
- Text before the first `#` heading is ignored.
- An empty topic (heading with no body) is allowed and emits only a header.

### Example input

```markdown
# Commands

Use `add-dirs` to load directories into the search pool.

`list-dirs` lists currently loaded directories while
`list-files` lists individual files.
```

### Example output

```ada
SP.Help.Header ("Commands");

SP.Help.Block (
    "Use " & Colorize_Command ("add-dirs") & " to load directories into the search pool."
);

SP.Help.Block (
    Colorize_Command ("list-dirs") & " lists currently loaded directories while " & Colorize_Command ("list-files") & " lists individual files."
);
```

## How generation works

`write_help.py` is a short pipeline:

1. **`read_help_files(path)`**  
   Parse the markdown file into `{topic_name: [paragraph, ...]}`.

2. **`transform_to_ada(text)`**  
   Convert one paragraph:
   - double quotes → Ada `""`
   - `` `cmd` `` → `Colorize_Command ("cmd")`
   - remaining text → `"literal"`
   - pieces joined with Ada `&`

3. **`render_ada_help(path)`**  
   For each topic, emit `SP.Help.Header ("…");` then one `SP.Help.Block (…);` per paragraph.

4. **`main()`**  
   CLI entry: for each argument path, write `render_ada_help` to stdout.

## Validating the subset

Before generating Ada, check that help sources stay within the subset above:

```bash
python tools/validate_help_md.py help/
python tools/validate_help_md.py help/full_help.md
```

The checker rejects CommonMark features the generator does not implement (lists, links, fenced code, tables, headings deeper than `#`, HTML, emphasis markers, and so on). It also reports unbalanced backticks, duplicate topics, and `"` in topic titles.

Warnings (preamble text before the first heading, empty topics) do not fail the run unless you pass `--strict`.

## Layout

```
tools/
  README.md                 This file
  write_help.py             Generator
  validate_help_md.py       Subset linter for help markdown
  test_write_help.py        Unit tests (parse / transform / CLI)
  test_write_help_golden.py Golden-file tests (full Ada output)
  test_validate_help_md.py  Unit tests for the subset linter
  testdata/golden/
    <case_name>/
      input.md              Sample help markdown
      expected.ada          Expected Ada output
```

## Tests

Uses the standard library `unittest` framework (no extra dependencies).

From the repository root:

```bash
python -m unittest discover -s tools -p "test_*.py" -v
```

### Golden files

End-to-end Ada output is checked against committed fixtures under `testdata/golden/`. Each case directory holds an `input.md` and an `expected.ada`.

After an intentional change to the generator’s output format, refresh the expected files:

```bash
python tools/test_write_help_golden.py --update
```

Alternatively, set `WRITE_HELP_UPDATE_GOLDEN=1` when running the golden tests.

To add a new golden case:

1. Create `tools/testdata/golden/<name>/input.md`
2. Run with `--update` to produce `expected.ada`
3. Review the new expected file and commit both

## Notes

- Output is Ada **source fragments**, not a complete compilable package by itself.
- Topic titles in headers are not yet quote-escaped the same way body text is; avoid `"` in topic names for now.
- Full markdown (lists, links, fenced code blocks, etc.) is not supported—keep help sources to headings, paragraphs, and backtick command names.
