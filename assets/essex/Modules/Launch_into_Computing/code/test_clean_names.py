# test_clean_names.py
# Tests for clean_names.py — covers core behaviour, edge cases, and validation

import unittest
import sys
import os

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))
from clean_names import process_names


class TestProcessNames(unittest.TestCase):

    def test_removes_case_insensitive_duplicates(self):
        """Core fix: 'alice', 'Alice', 'ALICE' should produce one entry."""
        result = process_names(["Alice", "alice", "ALICE"])
        self.assertEqual(result, ["Alice"])

    def test_returns_sorted_title_case(self):
        """Output should be sorted alphabetically in Title Case."""
        result = process_names(["Charlie", "alice", "BOB"])
        self.assertEqual(result, ["Alice", "Bob", "Charlie"])

    def test_original_test_data(self):
        """The original tutor test data should now return 5 unique names."""
        names = ["Alice", "bob", "alice", "Charlie", "BOB", "dave", "Eve", "charlie"]
        result = process_names(names)
        self.assertEqual(result, ["Alice", "Bob", "Charlie", "Dave", "Eve"])

    def test_raises_on_non_list_input(self):
        """Should raise TypeError if input is not a list."""
        with self.assertRaises(TypeError):
            process_names("Alice")

    def test_raises_on_none_input(self):
        """Should raise TypeError if input is None."""
        with self.assertRaises(TypeError):
            process_names(None)

    def test_raises_on_empty_list(self):
        """Should raise ValueError if list is empty."""
        with self.assertRaises(ValueError):
            process_names([])

    def test_raises_on_non_string_elements(self):
        """Should raise TypeError if any element is not a string."""
        with self.assertRaises(TypeError):
            process_names(["Alice", 42, "Bob"])


if __name__ == "__main__":
    unittest.main()
