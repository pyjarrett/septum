"""Generate Ada help text from markdown help files."""

from __future__ import annotations

import re
import sys
from os import PathLike

# Split on `command` spans; capturing keeps the delimiters in re.split results.
_BACKTICK = re.compile(r"(`[^`]+`)")


def read_help_files(path: str | PathLike[str]) -> dict[str, list[str]]:
    """Parse help markdown into ``{topic: [paragraph, ...]}``.

    Topics start with ``#``. Blank lines separate paragraphs; wrapped lines in a
    paragraph are joined with spaces.
    """
    topics: dict[str, list[str]] = {}
    topic: str | None = None
    lines: list[str] = []

    def flush() -> None:
        nonlocal lines
        if topic is not None and lines:
            topics[topic].append(" ".join(lines))
        lines = []

    with open(path, encoding="utf-8") as handle:
        for raw in handle:
            line = raw.rstrip("\n")
            if line.startswith("#"):
                flush()
                topic = line[1:].strip()
                if topic in topics:
                    raise ValueError(f"Found a duplicate topic {topic}")
                topics[topic] = []
            elif not line.strip():
                flush()
            else:
                lines.append(line.strip())
        flush()

    return topics


def transform_to_ada(text: str) -> str:
    """Turn a paragraph into Ada string literals joined with Colorize_Command."""
    text = text.replace('"', '""')
    parts = []
    for part in _BACKTICK.split(text):
        if part.startswith("`") and part.endswith("`"):
            parts.append(f'Colorize_Command ("{part[1:-1]}")')
        elif part:
            parts.append(f'"{part}"')
    return " & ".join(parts)


def render_ada_help(path: str | PathLike[str]) -> str:
    """Parse *path* and return the full Ada help text."""
    chunks: list[str] = []
    for topic, paragraphs in read_help_files(path).items():
        chunks.append(f'SP.Help.Header ("{topic}");\n\n')
        for paragraph in paragraphs:
            chunks.append(f"SP.Help.Block (\n    {transform_to_ada(paragraph)}\n);\n\n")
    return "".join(chunks)


def main(argv: list[str] | None = None) -> None:
    args = sys.argv[1:] if argv is None else argv
    for path in args:
        sys.stdout.write(render_ada_help(path))


if __name__ == "__main__":
    main()
