"""Generate Ada help text from markdown help files."""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from os import PathLike


# Split on `command` spans; capturing keeps the delimiters in re.split results.
_BACKTICK = re.compile(r"(`[^`]+`)")

# Opening or closing fence: optional indent, three+ backticks, optional info string.
_FENCE = re.compile(r"^\s*```")


@dataclass(frozen=True)
class Paragraph:
    """A prose paragraph (wrapped source lines joined with spaces)."""

    text: str


@dataclass(frozen=True)
class Example:
    """A fenced code-block example (lines preserved as written)."""

    lines: tuple[str, ...]


Block = Paragraph | Example


def _is_fence(line: str) -> bool:
    return bool(_FENCE.match(line))


def read_help_files(path: str | PathLike[str]) -> dict[str, list[Block]]:
    """Parse help markdown into ``{topic: [block, ...]}``.

    Topics start with ``#``. Blank lines separate paragraphs; wrapped lines in a
    paragraph are joined with spaces. Fenced code blocks (```` ``` ```` … ```` ``` ````)
    become :class:`Example` blocks with one string per line.
    """
    topics: dict[str, list[Block]] = {}
    topic: str | None = None
    lines: list[str] = []
    example_lines: list[str] | None = None

    def flush_paragraph() -> None:
        nonlocal lines
        if topic is not None and lines:
            topics[topic].append(Paragraph(" ".join(lines)))
        lines = []

    def flush_example() -> None:
        nonlocal example_lines
        if topic is not None and example_lines is not None:
            topics[topic].append(Example(tuple(example_lines)))
        example_lines = None

    with open(path, encoding="utf-8") as handle:
        for raw in handle:
            line = raw.rstrip("\n")
            if example_lines is not None:
                if _is_fence(line):
                    flush_example()
                else:
                    example_lines.append(line)
                continue

            if _is_fence(line):
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
                lines.append(line.strip())

        if example_lines is not None:
            raise ValueError(f"Unclosed example fence in {path}")
        flush_paragraph()

    return topics


def _escape_ada_string(text: str) -> str:
    return text.replace('"', '""')


def transform_to_ada(text: str) -> str:
    """Turn a paragraph into Ada string literals joined with Colorize_Command."""
    text = _escape_ada_string(text)
    parts = []
    for part in _BACKTICK.split(text):
        if part.startswith("`") and part.endswith("`"):
            parts.append(f'Colorize_Command ("{part[1:-1]}")')
        elif part:
            parts.append(f'"{part}"')
    return " & ".join(parts)


def transform_example_to_ada(lines: tuple[str, ...] | list[str]) -> str:
    """Turn example lines into an ``SP.Help.Example`` call with a string vector."""
    if not lines:
        return "SP.Help.Example (String_Vectors.Empty_Vector);"

    pieces = [
        f'To_Unbounded_String ("{_escape_ada_string(line)}")' for line in lines
    ]
    joined = "\n    & ".join(pieces)
    return (
        "SP.Help.Example (\n"
        f"    String_Vectors.Empty_Vector\n"
        f"    & {joined}\n"
        ");"
    )


def render_ada_help(path: str | PathLike[str]) -> str:
    """Parse *path* and return the full Ada help text."""
    chunks: list[str] = []
    for topic, blocks in read_help_files(path).items():
        chunks.append(f'SP.Help.Header ("{topic}");\n\n')
        for block in blocks:
            if isinstance(block, Paragraph):
                chunks.append(
                    f"SP.Help.Block (\n    {transform_to_ada(block.text)}\n);\n\n"
                )
            else:
                chunks.append(f"{transform_example_to_ada(block.lines)}\n\n")
    return "".join(chunks)


def main(argv: list[str] | None = None) -> None:
    parser = argparse.ArgumentParser(
        description="Generate Ada help text from markdown help files."
    )
    parser.add_argument(
        "paths",
        nargs="*",
        help="markdown help files to process",
    )
    parser.add_argument(
        "--output",
        "-o",
        metavar="FILE",
        help="write Ada help text to FILE (default: stdout)",
    )
    args = parser.parse_args(argv)

    text = "".join(render_ada_help(path) for path in args.paths)
    if args.output is not None:
        with open(args.output, "w", encoding="utf-8", newline="\n") as handle:
            handle.write(text)
    else:
        sys.stdout.write(text)


if __name__ == "__main__":
    main()
