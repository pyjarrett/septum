"""Unit tests for write_help.py.

Run from the repository root::

    python -m unittest discover -s tools -p "test_*.py" -v
"""

from __future__ import annotations

import io
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

TOOLS_DIR = Path(__file__).resolve().parent
if str(TOOLS_DIR) not in sys.path:
    sys.path.insert(0, str(TOOLS_DIR))

import write_help
from write_help import read_help_files, render_ada_help, transform_to_ada


class ReadHelpFilesTests(unittest.TestCase):
    def write(self, content: str) -> Path:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        path = Path(tmp.name) / "help.md"
        path.write_text(content, encoding="utf-8")
        return path

    def test_empty_file(self):
        self.assertEqual(read_help_files(self.write("")), {})

    def test_single_paragraph(self):
        topics = read_help_files(self.write("# My Topic\n\nHello world.\n"))
        self.assertEqual(topics, {"My Topic": ["Hello world."]})

    def test_joins_wrapped_lines(self):
        topics = read_help_files(
            self.write("# Topic\n\nFirst line\nsecond line\nthird line\n")
        )
        self.assertEqual(topics["Topic"], ["First line second line third line"])

    def test_blank_lines_split_paragraphs(self):
        topics = read_help_files(
            self.write("# Topic\n\nOne.\n\nTwo.\n\n\nThree.\n")
        )
        self.assertEqual(topics["Topic"], ["One.", "Two.", "Three."])

    def test_multiple_topics(self):
        topics = read_help_files(
            self.write("# First\n\nAlpha.\n\n# Second\n\nBeta.\nGamma.\n")
        )
        self.assertEqual(topics, {"First": ["Alpha."], "Second": ["Beta. Gamma."]})

    def test_strips_topic_and_body(self):
        topics = read_help_files(self.write("#   Spaced   \n\n  body  \n\tmore\n"))
        self.assertEqual(topics, {"Spaced": ["body more"]})

    def test_duplicate_topic_raises(self):
        path = self.write("# Same\n\nOne.\n\n# Same\n\nTwo.\n")
        with self.assertRaisesRegex(ValueError, "duplicate topic Same"):
            read_help_files(path)

    def test_empty_topic_body(self):
        self.assertEqual(read_help_files(self.write("# Empty\n")), {"Empty": []})

    def test_preserves_backticks(self):
        topics = read_help_files(self.write("# T\n\nUse `add-dirs` here.\n"))
        self.assertEqual(topics["T"], ["Use `add-dirs` here."])

    def test_discards_preamble(self):
        topics = read_help_files(self.write("Lost.\n\n# Kept\n\nBody.\n"))
        self.assertEqual(topics, {"Kept": ["Body."]})


class TransformToAdaTests(unittest.TestCase):
    CASES = [
        ("plain", "Hello world.", '"Hello world."'),
        ("empty", "", ""),
        ("quotes", 'Say "hi"', '"Say ""hi"""'),
        ("command", "`add-dirs`", 'Colorize_Command ("add-dirs")'),
        (
            "mixed",
            "Use `reload` now.",
            '"Use " & Colorize_Command ("reload") & " now."',
        ),
        (
            "two_commands",
            "`a` then `b`",
            'Colorize_Command ("a") & " then " & Colorize_Command ("b")',
        ),
        (
            "adjacent",
            "`first``second`",
            'Colorize_Command ("first") & Colorize_Command ("second")',
        ),
        (
            "quotes_in_command",
            '`say "x"`',
            'Colorize_Command ("say ""x""")',
        ),
    ]

    def test_transform_cases(self):
        for name, source, expected in self.CASES:
            with self.subTest(name=name):
                self.assertEqual(transform_to_ada(source), expected)


class RenderAndMainTests(unittest.TestCase):
    def test_render_ada_help(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "help.md"
            path.write_text("# Demo\n\nTry `list-dirs` please.\n", encoding="utf-8")
            out = render_ada_help(path)
            self.assertIn('SP.Help.Header ("Demo");', out)
            self.assertIn("SP.Help.Block (", out)
            self.assertIn('Colorize_Command ("list-dirs")', out)

    def test_main_no_args(self):
        buf = io.StringIO()
        with patch.object(sys, "stdout", buf):
            write_help.main([])
        self.assertEqual(buf.getvalue(), "")

    def test_main_writes_stdout(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "help.md"
            path.write_text("# Demo\n\nHi.\n", encoding="utf-8")
            buf = io.StringIO()
            with patch.object(sys, "stdout", buf):
                write_help.main([str(path)])
            self.assertEqual(buf.getvalue(), render_ada_help(path))


if __name__ == "__main__":
    unittest.main()
