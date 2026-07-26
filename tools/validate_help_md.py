"""Validate help markdown against the write_help.py subset.

Allowed constructs (see tools/README.md):

* ``# Topic name`` — first-level headings only (line must start with a single ``#``)
* Blank lines — paragraph separators
* Paragraph text — consecutive non-blank lines join with spaces
* `` `command` `` — non-empty inline backticks for command names
* Fenced examples — `` ``` `` … `` ``` `` (optional language tag on the opener)

Everything else CommonMark offers (lists, links, ``~~~`` fences, tables, deeper
headings, HTML, …) is rejected so authors do not write markup the generator
silently flattens or mis-handles.

Usage (from the repository root)::

    python tools/validate_help_md.py help/full_help.md
    python tools/validate_help_md.py help/
    python tools/validate_help_md.py tools/testdata/golden/*/input.md

Exit status is 0 when every file is clean, 1 when any error is reported.
Warnings alone do not fail the run unless ``--strict`` is passed.
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path

TOOLS_DIR = Path(__file__).resolve().parent
if str(TOOLS_DIR) not in sys.path:
    sys.path.insert(0, str(TOOLS_DIR))

import write_help

# --- line-level patterns for disallowed CommonMark ---------------------------------

_ATX_MULTI = re.compile(r"^#{2,6}(\s|$)")
_UNORDERED_LIST = re.compile(r"^(\s*)([-*+])\s+\S")
_ORDERED_LIST = re.compile(r"^(\s*)\d+\.\s+\S")
_BLOCKQUOTE = re.compile(r"^\s{0,3}>")
_TICK_FENCE = re.compile(r"^\s*```")
_TILDE_FENCE = re.compile(r"^\s{0,3}~~~")
_TABLE_ROW = re.compile(r"^\s*\|.*\|\s*$")
_HORIZONTAL_RULE = re.compile(r"^\s{0,3}((-\s*){3,}|(\*\s*){3,}|(_\s*){3,})\s*$")
_INDENTED_CODE = re.compile(r"^ {4,}\S")
_SET_EXT_UNDERLINE = re.compile(r"^\s{0,3}(=+|-+)\s*$")
_LINK = re.compile(r"(?<!!)\[[^\]]*\]\([^)]*\)")
_REFERENCE_LINK = re.compile(r"(?<!!)\[[^\]]+\]\[[^\]]*\]")
_IMAGE = re.compile(r"!\[[^\]]*\](\([^)]*\)|\[[^\]]*\])")
_HTML_TAG = re.compile(r"</?[A-Za-z][A-Za-z0-9:-]*(\s[^>]*)?>")
_AUTOLINK = re.compile(r"<https?://[^>\s]+>")
_EMPHASIS = re.compile(
    r"(?<!\*)\*\*[^*\n]+\*\*(?!\*)"  # **bold**
    r"|(?<!_)__[^_\n]+__(?!_)"  # __bold__
    r"|(?<!\*)\*[^*\s][^*\n]*\*(?!\*)"  # *italic* (no spaces-only)
    r"|(?<!_)_[^_\s][^_\n]*_(?!_)"  # _italic_
)
_STRIKE = re.compile(r"~~.+?~~")
_HEADING_OK = re.compile(r"^#(\s.*)?$")


def _backtick_issues(path: Path, line_no: int, line: str) -> list[Issue]:
    """Require matched, non-empty `` `command` `` spans (adjacent spans are OK)."""
    issues: list[Issue] = []
    i = 0
    while i < len(line):
        if line[i] != "`":
            i += 1
            continue
        close = line.find("`", i + 1)
        if close < 0:
            issues.append(
                Issue(
                    path,
                    line_no,
                    "error",
                    "unbalanced backticks; each command span must be closed (`command`)",
                )
            )
            break
        if close == i + 1:
            issues.append(
                Issue(
                    path,
                    line_no,
                    "error",
                    "empty backtick span is not a valid command; use `command`",
                )
            )
        i = close + 1
    return issues


@dataclass(frozen=True)
class Issue:
    path: Path
    line: int | None
    severity: str  # "error" | "warning"
    message: str

    def format(self) -> str:
        loc = f"{self.path}"
        if self.line is not None:
            loc = f"{self.path}:{self.line}"
        return f"{loc}: {self.severity}: {self.message}"


def _body_line_issues(path: Path, line_no: int, line: str) -> list[Issue]:
    """Return issues for a non-heading content line (outside example fences)."""
    issues: list[Issue] = []

    checks: list[tuple[re.Pattern[str], str]] = [
        (_ATX_MULTI, "only first-level headings are allowed (`# Topic`); deeper ATX headings are not supported"),
        (_UNORDERED_LIST, "unordered list markers are not supported; use plain paragraphs"),
        (_ORDERED_LIST, "ordered list markers are not supported; use plain paragraphs"),
        (_BLOCKQUOTE, "blockquotes are not supported"),
        (_TILDE_FENCE, "only ``` fenced examples are supported; ~~~ fences are not"),
        (_TABLE_ROW, "tables are not supported"),
        (_HORIZONTAL_RULE, "horizontal rules are not supported"),
        (_INDENTED_CODE, "indented code blocks are not supported; the generator strips leading spaces"),
    ]
    for pattern, message in checks:
        if pattern.search(line):
            issues.append(Issue(path, line_no, "error", message))

    # Setext underlines only matter after a short title line; flag any all-= or all-- line.
    if _SET_EXT_UNDERLINE.match(line) and not line.strip().startswith("#"):
        issues.append(
            Issue(
                path,
                line_no,
                "error",
                "setext-style underlines are not supported; use `# Topic` headings",
            )
        )

    inline_checks: list[tuple[re.Pattern[str], str]] = [
        (_IMAGE, "images are not supported"),
        (_LINK, "markdown links are not supported; write the command or path as plain text"),
        (_REFERENCE_LINK, "reference links are not supported"),
        (_AUTOLINK, "autolinks are not supported"),
        (_HTML_TAG, "HTML tags are not supported"),
        (_STRIKE, "strikethrough is not supported"),
        (_EMPHASIS, "emphasis markers (`*`, `_`, `**`, `__`) are not part of the help subset"),
    ]
    for pattern, message in inline_checks:
        if pattern.search(line):
            issues.append(Issue(path, line_no, "error", message))

    issues.extend(_backtick_issues(path, line_no, line))

    # Leading whitespace before a lone # is body text in write_help, not a heading.
    if re.match(r"^[ \t]+#", line):
        issues.append(
            Issue(
                path,
                line_no,
                "warning",
                "leading whitespace before `#` means this is paragraph text, not a topic heading",
            )
        )

    return issues


def _heading_issues(path: Path, line_no: int, line: str) -> list[Issue]:
    issues: list[Issue] = []
    if _ATX_MULTI.match(line):
        issues.append(
            Issue(
                path,
                line_no,
                "error",
                "only first-level headings are allowed (`# Topic`)",
            )
        )
        return issues

    if not _HEADING_OK.match(line):
        # e.g. `#Topic` without space, or `#` with trailing junk like `#foo#`
        if line.startswith("#") and not line.startswith("# ") and line not in {"#", "#\t"}:
            # `#Topic` is accepted by write_help (title "Topic") but is easy to confuse with tags.
            if re.match(r"^#[^#\s]", line):
                issues.append(
                    Issue(
                        path,
                        line_no,
                        "warning",
                        "prefer `# Topic` with a space after `#` for readability",
                    )
                )

    title = line[1:].strip()
    if not title:
        issues.append(
            Issue(path, line_no, "warning", "empty topic title; heading has no name after `#`")
        )
    if '"' in title:
        issues.append(
            Issue(
                path,
                line_no,
                "error",
                'topic titles must not contain double quotes (headers are not quote-escaped)',
            )
        )
    return issues


def validate_text(path: Path, text: str) -> list[Issue]:
    """Validate *text* as help markdown; *path* is used only in issue locations."""
    issues: list[Issue] = []
    lines = text.splitlines()
    seen_heading = False
    prev_nonempty: str | None = None
    prev_nonempty_no: int | None = None
    in_example = False
    example_open_line: int | None = None

    for line_no, raw in enumerate(lines, start=1):
        line = raw.rstrip("\n")

        if in_example:
            if _TICK_FENCE.match(line):
                in_example = False
                example_open_line = None
            # Content inside examples is free-form (session transcripts, etc.).
            prev_nonempty = None
            prev_nonempty_no = None
            continue

        if _TICK_FENCE.match(line):
            in_example = True
            example_open_line = line_no
            if not seen_heading:
                issues.append(
                    Issue(
                        path,
                        line_no,
                        "warning",
                        "text before the first `#` heading is ignored by write_help.py",
                    )
                )
            prev_nonempty = None
            prev_nonempty_no = None
            continue

        # Preserve internal content; do not strip yet so indented constructs are visible.
        if not line.strip():
            prev_nonempty = None
            prev_nonempty_no = None
            continue

        if line.startswith("#"):
            seen_heading = True
            issues.extend(_heading_issues(path, line_no, line))
            prev_nonempty = line
            prev_nonempty_no = line_no
            continue

        if not seen_heading:
            issues.append(
                Issue(
                    path,
                    line_no,
                    "warning",
                    "text before the first `#` heading is ignored by write_help.py",
                )
            )

        # Setext: previous short line + === or --- underline.
        if (
            prev_nonempty is not None
            and prev_nonempty_no is not None
            and not prev_nonempty.startswith("#")
            and _SET_EXT_UNDERLINE.match(line)
        ):
            issues.append(
                Issue(
                    path,
                    line_no,
                    "error",
                    f"setext heading underline (after line {prev_nonempty_no}) is not supported; use `# Topic`",
                )
            )

        issues.extend(_body_line_issues(path, line_no, line))
        prev_nonempty = line
        prev_nonempty_no = line_no

    if in_example:
        issues.append(
            Issue(
                path,
                example_open_line,
                "error",
                "unclosed example fence; each ``` opener needs a matching ``` closer",
            )
        )

    # Structural checks via the real parser (duplicate topics, load errors).
    try:
        # write_help reads from disk; use a temporary path only when needed.
        # Callers of validate_file always have a real path.
        if path.is_file():
            topics = write_help.read_help_files(path)
        else:
            topics = _parse_topics_from_text(text)
    except ValueError as exc:
        issues.append(Issue(path, None, "error", str(exc)))
        return _dedupe_issues(issues)
    except OSError as exc:
        issues.append(Issue(path, None, "error", f"cannot read file: {exc}"))
        return _dedupe_issues(issues)

    if not topics:
        issues.append(
            Issue(path, None, "warning", "no topics found; file has no `#` headings")
        )
    for title, blocks in topics.items():
        if not blocks:
            issues.append(
                Issue(
                    path,
                    None,
                    "warning",
                    f'topic "{title}" has no body paragraphs',
                )
            )
        if '"' in title:
            issues.append(
                Issue(
                    path,
                    None,
                    "error",
                    f'topic "{title}" contains double quotes (headers are not quote-escaped)',
                )
            )

    return _dedupe_issues(issues)


def _parse_topics_from_text(text: str) -> dict[str, list[write_help.Block]]:
    """Mirror write_help.read_help_files for in-memory strings (tests)."""
    topics: dict[str, list[write_help.Block]] = {}
    topic: str | None = None
    buf: list[str] = []
    example_lines: list[str] | None = None

    def flush_paragraph() -> None:
        nonlocal buf
        if topic is not None and buf:
            topics[topic].append(write_help.Paragraph(" ".join(buf)))
        buf = []

    def flush_example() -> None:
        nonlocal example_lines
        if topic is not None and example_lines is not None:
            topics[topic].append(write_help.Example(tuple(example_lines)))
        example_lines = None

    for raw in text.splitlines():
        line = raw.rstrip("\n")
        if example_lines is not None:
            if _TICK_FENCE.match(line):
                flush_example()
            else:
                example_lines.append(line)
            continue

        if _TICK_FENCE.match(line):
            flush_paragraph()
            example_lines = []
            continue

        if line.startswith("#"):
            flush_paragraph()
            topic = line[1:].strip()
            if topic in topics:
                raise ValueError(f"Found a duplicate topic {topic}")
            topics[topic] = []
        elif not line.strip():
            flush_paragraph()
        else:
            buf.append(line.strip())

    if example_lines is not None:
        raise ValueError("Unclosed example fence")
    flush_paragraph()
    return topics


def _dedupe_issues(issues: list[Issue]) -> list[Issue]:
    seen: set[tuple[str, int | None, str, str]] = set()
    out: list[Issue] = []
    for issue in issues:
        key = (str(issue.path), issue.line, issue.severity, issue.message)
        if key in seen:
            continue
        seen.add(key)
        out.append(issue)
    return out


def validate_file(path: Path) -> list[Issue]:
    """Validate a single markdown file on disk."""
    path = path.resolve() if path.exists() else path
    try:
        text = path.read_text(encoding="utf-8")
    except OSError as exc:
        return [Issue(path, None, "error", f"cannot read file: {exc}")]
    return validate_text(path, text)


def iter_markdown_paths(paths: list[Path]) -> list[Path]:
    """Expand files and directories into a sorted list of ``*.md`` paths."""
    found: list[Path] = []
    for path in paths:
        if path.is_dir():
            found.extend(sorted(path.rglob("*.md")))
        else:
            found.append(path)
    # Stable unique
    seen: set[Path] = set()
    unique: list[Path] = []
    for path in found:
        resolved = path.resolve() if path.exists() else path
        if resolved in seen:
            continue
        seen.add(resolved)
        unique.append(path)
    return unique


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Validate help markdown against the write_help.py subset."
    )
    parser.add_argument(
        "paths",
        nargs="+",
        type=Path,
        help="markdown files or directories to check",
    )
    parser.add_argument(
        "--strict",
        action="store_true",
        help="treat warnings as failures (non-zero exit)",
    )
    args = parser.parse_args(argv)

    files = iter_markdown_paths(args.paths)
    if not files:
        print("No markdown files to validate.", file=sys.stderr)
        return 1

    all_issues: list[Issue] = []
    for path in files:
        all_issues.extend(validate_file(path))

    for issue in all_issues:
        print(issue.format())

    errors = sum(1 for i in all_issues if i.severity == "error")
    warnings = sum(1 for i in all_issues if i.severity == "warning")
    print(
        f"Checked {len(files)} file(s): {errors} error(s), {warnings} warning(s)."
    )

    if errors:
        return 1
    if args.strict and warnings:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
