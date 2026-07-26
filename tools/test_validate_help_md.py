"""Unit tests for validate_help_md.py.

Run from the repository root::

    python -m unittest discover -s tools -p "test_*.py" -v
"""

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path

TOOLS_DIR = Path(__file__).resolve().parent
if str(TOOLS_DIR) not in sys.path:
    sys.path.insert(0, str(TOOLS_DIR))

from validate_help_md import (  # noqa: E402
    Issue,
    iter_markdown_paths,
    main,
    validate_file,
    validate_text,
)


class ValidateTextTests(unittest.TestCase):
    def issues(self, text: str, path: str = "sample.md") -> list[Issue]:
        return validate_text(Path(path), text)

    def errors(self, text: str) -> list[Issue]:
        return [i for i in self.issues(text) if i.severity == "error"]

    def warnings(self, text: str) -> list[Issue]:
        return [i for i in self.issues(text) if i.severity == "warning"]

    def test_valid_minimal_file(self):
        text = (
            "# Commands\n"
            "\n"
            "Use `add-dirs` to load directories.\n"
            "\n"
            "`list-dirs` and `list-files` inspect the cache.\n"
        )
        self.assertEqual(self.errors(text), [])

    def test_multiple_topics(self):
        text = "# A\n\nBody one.\n\n# B\n\nBody two with `run`.\n"
        self.assertEqual(self.errors(text), [])

    def test_rejects_second_level_heading(self):
        errs = self.errors("# Top\n\n## Nested\n\nText.\n")
        self.assertTrue(any("first-level" in e.message for e in errs))

    def test_rejects_unordered_list(self):
        for marker in ("-", "*", "+"):
            with self.subTest(marker=marker):
                errs = self.errors(f"# T\n\n{marker} item one\n")
                self.assertTrue(any("unordered list" in e.message for e in errs), errs)

    def test_rejects_ordered_list(self):
        errs = self.errors("# T\n\n1. first\n2. second\n")
        self.assertTrue(any("ordered list" in e.message for e in errs))

    def test_rejects_blockquote(self):
        errs = self.errors("# T\n\n> quoted\n")
        self.assertTrue(any("blockquote" in e.message for e in errs))

    def test_rejects_fenced_code(self):
        errs = self.errors("# T\n\n```\ncode\n```\n")
        self.assertTrue(any("fenced" in e.message for e in errs))

    def test_rejects_table(self):
        errs = self.errors("# T\n\n| a | b |\n| --- | --- |\n| 1 | 2 |\n")
        self.assertTrue(any("table" in e.message for e in errs))

    def test_rejects_horizontal_rule(self):
        errs = self.errors("# T\n\n---\n\nMore.\n")
        self.assertTrue(any("horizontal rule" in e.message for e in errs))

    def test_rejects_link(self):
        errs = self.errors("# T\n\nSee [docs](https://example.com).\n")
        self.assertTrue(any("link" in e.message for e in errs))

    def test_rejects_image(self):
        errs = self.errors("# T\n\n![alt](img.png)\n")
        self.assertTrue(any("image" in e.message for e in errs))

    def test_rejects_html(self):
        errs = self.errors("# T\n\nUse <br> here.\n")
        self.assertTrue(any("HTML" in e.message for e in errs))

    def test_rejects_emphasis(self):
        errs = self.errors("# T\n\nThis is **bold** text.\n")
        self.assertTrue(any("emphasis" in e.message for e in errs))

    def test_rejects_unbalanced_backticks(self):
        errs = self.errors("# T\n\nUse `add-dirs to load.\n")
        self.assertTrue(any("unbalanced" in e.message for e in errs))

    def test_rejects_empty_backticks(self):
        errs = self.errors("# T\n\nEmpty `` spans.\n")
        self.assertTrue(any("empty backtick" in e.message for e in errs))

    def test_adjacent_command_spans_ok(self):
        # write_help accepts `first``second` as two command spans.
        text = "# T\n\nJoin `first``second` without space.\n"
        self.assertEqual(self.errors(text), [])

    def test_rejects_quote_in_topic_title(self):
        errs = self.errors('# Say "hi"\n\nBody.\n')
        self.assertTrue(any("double quotes" in e.message for e in errs))

    def test_duplicate_topic_error(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "dup.md"
            path.write_text("# Same\n\nOne.\n\n# Same\n\nTwo.\n", encoding="utf-8")
            issues = validate_file(path)
            self.assertTrue(
                any(i.severity == "error" and "duplicate" in i.message.lower() for i in issues)
            )

    def test_preamble_warning(self):
        warns = self.warnings("Preamble.\n\n# Topic\n\nBody.\n")
        self.assertTrue(any("before the first" in w.message for w in warns))

    def test_indented_heading_warning(self):
        warns = self.warnings("# T\n\n  # not a heading\n")
        self.assertTrue(any("leading whitespace" in w.message for w in warns))

    def test_minus_in_prose_is_ok(self):
        # Mid-sentence dashes and hyphenated words must not trip list detection.
        text = "# T\n\nUse find-text - then exclude-like when needed.\n"
        self.assertEqual(self.errors(text), [])

    def test_hash_in_prose_is_ok(self):
        text = "# T\n\nLines that start with # are comments in config files.\n"
        # This line starts with "Lines", not "#", so OK. A body line that is only
        # about # comments:
        text = (
            "# System\n\n"
            "In config files, lines whose first character is a # are comments.\n"
        )
        self.assertEqual(self.errors(text), [])


class PathAndMainTests(unittest.TestCase):
    def test_iter_markdown_paths_dir(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "a.md").write_text("# A\n", encoding="utf-8")
            (root / "b.txt").write_text("nope", encoding="utf-8")
            sub = root / "sub"
            sub.mkdir()
            (sub / "c.md").write_text("# C\n", encoding="utf-8")
            paths = iter_markdown_paths([root])
            names = sorted(p.name for p in paths)
            self.assertEqual(names, ["a.md", "c.md"])

    def test_main_clean_file(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "ok.md"
            path.write_text("# Topic\n\nUse `reload` after edits.\n", encoding="utf-8")
            self.assertEqual(main([str(path)]), 0)

    def test_main_fails_on_error(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "bad.md"
            path.write_text("# Topic\n\n- list item\n", encoding="utf-8")
            self.assertEqual(main([str(path)]), 1)

    def test_main_strict_warnings(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "preamble.md"
            path.write_text("ignored\n\n# Topic\n\nBody.\n", encoding="utf-8")
            self.assertEqual(main([str(path)]), 0)
            self.assertEqual(main(["--strict", str(path)]), 1)


if __name__ == "__main__":
    unittest.main()
