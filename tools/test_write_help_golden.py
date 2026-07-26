"""Golden-file tests for Ada help output.

Cases live under ``tools/testdata/golden/<name>/``::

    input.md      source help markdown
    expected.ada  expected Ada output

Run::

    python -m unittest discover -s tools -p "test_*.py" -v

Refresh expected files after intentional changes::

    python tools/test_write_help_golden.py --update
"""

from __future__ import annotations

import os
import sys
import unittest
from pathlib import Path

TOOLS_DIR = Path(__file__).resolve().parent
if str(TOOLS_DIR) not in sys.path:
    sys.path.insert(0, str(TOOLS_DIR))

from write_help import render_ada_help

GOLDEN_ROOT = TOOLS_DIR / "testdata" / "golden"


def _nl(text: str) -> str:
    return text.replace("\r\n", "\n").replace("\r", "\n")


def _cases() -> list[Path]:
    if not GOLDEN_ROOT.is_dir():
        return []
    return sorted(
        p for p in GOLDEN_ROOT.iterdir() if p.is_dir() and (p / "input.md").is_file()
    )


def _update() -> bool:
    return "--update" in sys.argv or os.environ.get("WRITE_HELP_UPDATE_GOLDEN") == "1"


class GoldenAdaHelpTests(unittest.TestCase):
    def test_ada_output_matches_golden_files(self):
        cases = _cases()
        self.assertTrue(cases, f"no golden cases under {GOLDEN_ROOT}")

        for case in cases:
            with self.subTest(case=case.name):
                actual = _nl(render_ada_help(case / "input.md"))
                expected_path = case / "expected.ada"

                if _update():
                    expected_path.write_text(actual, encoding="utf-8", newline="\n")
                    continue

                self.assertTrue(
                    expected_path.is_file(),
                    f"missing {expected_path}; run with --update",
                )
                expected = _nl(expected_path.read_text(encoding="utf-8"))
                self.assertMultiLineEqual(actual, expected)


if __name__ == "__main__":
    if "--update" in sys.argv:
        sys.argv = [a for a in sys.argv if a != "--update"]
        os.environ["WRITE_HELP_UPDATE_GOLDEN"] = "1"
    unittest.main()
