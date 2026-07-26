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
from write_help import (
    Example,
    Paragraph,
    read_help_files,
    render_ada_help,
    transform_example_to_ada,
    transform_to_ada,
)


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
        self.assertEqual(topics, {"My Topic": [Paragraph("Hello world.")]})

    def test_joins_wrapped_lines(self):
        topics = read_help_files(
            self.write("# Topic\n\nFirst line\nsecond line\nthird line\n")
        )
        self.assertEqual(
            topics["Topic"], [Paragraph("First line second line third line")]
        )

    def test_blank_lines_split_paragraphs(self):
        topics = read_help_files(
            self.write("# Topic\n\nOne.\n\nTwo.\n\n\nThree.\n")
        )
        self.assertEqual(
            topics["Topic"],
            [Paragraph("One."), Paragraph("Two."), Paragraph("Three.")],
        )

    def test_multiple_topics(self):
        topics = read_help_files(
            self.write("# First\n\nAlpha.\n\n# Second\n\nBeta.\nGamma.\n")
        )
        self.assertEqual(
            topics,
            {
                "First": [Paragraph("Alpha.")],
                "Second": [Paragraph("Beta. Gamma.")],
            },
        )

    def test_strips_topic_and_body(self):
        topics = read_help_files(self.write("#   Spaced   \n\n  body  \n\tmore\n"))
        self.assertEqual(topics, {"Spaced": [Paragraph("body more")]})

    def test_duplicate_topic_raises(self):
        path = self.write("# Same\n\nOne.\n\n# Same\n\nTwo.\n")
        with self.assertRaisesRegex(ValueError, "duplicate topic Same"):
            read_help_files(path)

    def test_empty_topic_body(self):
        self.assertEqual(read_help_files(self.write("# Empty\n")), {"Empty": []})

    def test_preserves_backticks(self):
        topics = read_help_files(self.write("# T\n\nUse `add-dirs` here.\n"))
        self.assertEqual(topics["T"], [Paragraph("Use `add-dirs` here.")])

    def test_discards_preamble(self):
        topics = read_help_files(self.write("Lost.\n\n# Kept\n\nBody.\n"))
        self.assertEqual(topics, {"Kept": [Paragraph("Body.")]})

    def test_example_fence(self):
        topics = read_help_files(
            self.write(
                "# T\n\n"
                "Intro.\n\n"
                "```\n"
                "add-dirs src\n"
                "find-text foo\n"
                "```\n\n"
                "Outro.\n"
            )
        )
        self.assertEqual(
            topics["T"],
            [
                Paragraph("Intro."),
                Example(("add-dirs src", "find-text foo")),
                Paragraph("Outro."),
            ],
        )

    def test_example_preserves_blank_and_indent(self):
        topics = read_help_files(
            self.write(
                "# T\n"
                "```\n"
                "  indented\n"
                "\n"
                "after blank\n"
                "```\n"
            )
        )
        self.assertEqual(
            topics["T"],
            [Example(("  indented", "", "after blank"))],
        )

    def test_example_with_language_tag(self):
        topics = read_help_files(
            self.write("# T\n\n```text\nhello\n```\n")
        )
        self.assertEqual(topics["T"], [Example(("hello",))])

    def test_empty_example(self):
        topics = read_help_files(self.write("# T\n\n```\n```\n"))
        self.assertEqual(topics["T"], [Example(())])

    def test_example_flushes_open_paragraph(self):
        topics = read_help_files(
            self.write("# T\n\nBefore fence\nstill para\n```\ncmd\n```\n")
        )
        self.assertEqual(
            topics["T"],
            [Paragraph("Before fence still para"), Example(("cmd",))],
        )

    def test_unclosed_example_raises(self):
        path = self.write("# T\n\n```\nno close\n")
        with self.assertRaisesRegex(ValueError, "Unclosed example fence"):
            read_help_files(path)

    def test_fence_in_preamble_does_not_create_topic(self):
        topics = read_help_files(
            self.write("preamble\n```\n# Not a topic\n```\n# Real\n\nBody.\n")
        )
        self.assertEqual(topics, {"Real": [Paragraph("Body.")]})


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


class TransformExampleToAdaTests(unittest.TestCase):
    def test_empty(self):
        self.assertEqual(
            transform_example_to_ada(()),
            "SP.Help.Example (String_Vectors.Empty_Vector);",
        )

    def test_single_line(self):
        out = transform_example_to_ada(("add-dirs src",))
        self.assertEqual(
            out,
            "SP.Help.Example (\n"
            "    String_Vectors.Empty_Vector\n"
            '    & To_Unbounded_String ("add-dirs src")\n'
            ");",
        )

    def test_multiple_lines(self):
        out = transform_example_to_ada(("one", "two"))
        self.assertIn('To_Unbounded_String ("one")', out)
        self.assertIn('To_Unbounded_String ("two")', out)
        self.assertIn("String_Vectors.Empty_Vector", out)

    def test_quotes_escaped(self):
        out = transform_example_to_ada(('echo "hi"',))
        self.assertIn('To_Unbounded_String ("echo ""hi""")', out)


class RenderAndMainTests(unittest.TestCase):
    def test_render_ada_help(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "help.md"
            path.write_text("# Demo\n\nTry `list-dirs` please.\n", encoding="utf-8")
            out = render_ada_help(path)
            self.assertIn('SP.Help.Header ("Demo");', out)
            self.assertIn("SP.Help.Block (", out)
            self.assertIn('Colorize_Command ("list-dirs")', out)

    def test_render_example(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "help.md"
            path.write_text(
                "# Demo\n\nTry this:\n\n```\nadd-dirs .\n```\n",
                encoding="utf-8",
            )
            out = render_ada_help(path)
            self.assertIn("SP.Help.Example (", out)
            self.assertIn('To_Unbounded_String ("add-dirs .")', out)
            self.assertIn("String_Vectors.Empty_Vector", out)

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

    def test_main_writes_output_file(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "help.md"
            out = Path(tmp) / "help.ada"
            path.write_text("# Demo\n\nHi.\n", encoding="utf-8")
            buf = io.StringIO()
            with patch.object(sys, "stdout", buf):
                write_help.main(["--output", str(out), str(path)])
            self.assertEqual(buf.getvalue(), "")
            self.assertEqual(out.read_text(encoding="utf-8"), render_ada_help(path))

    def test_main_output_short_flag(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "help.md"
            out = Path(tmp) / "out.ada"
            path.write_text("# Demo\n\nHi.\n", encoding="utf-8")
            write_help.main(["-o", str(out), str(path)])
            self.assertEqual(out.read_text(encoding="utf-8"), render_ada_help(path))


if __name__ == "__main__":
    unittest.main()
